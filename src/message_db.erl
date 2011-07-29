%% File : message_db.erl
%% Description : database for user messages

-module(message_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-export([init/2]).
-export([start/2, stop/1,
	 save_message/2, get_message/1, get_message/2, get_latest_message/1,
	 get_sent_timeline/2]).

%%
%% @doc initial setup functions
%%

start(UserName, DBPid)->
    spawn_link(?MODULE, init, [UserName, DBPid]).

stop(Pid) ->
    Pid ! {stop, self()}.

init(UserName, DBPid)->
    process_flag(trap_exit, true),
    create_tables(UserName, DBPid),
    restore_table(UserName, DBPid),
    {ok, User} = user_db:lookup_name(UserName),
    loop({User, DBPid}).

create_tables(UserName, DBPid)->  
    Device = db_name(UserName),
    ets:new(Device, [ordered_set, named_table, {keypos, #message.id}]),
    create_sqlite3_tables(DBPid).

create_sqlite3_tables(DBPid) ->
    case lists:member(messages, sqlite3:list_tables(DBPid)) of
	true -> ok;
	false ->
	    sqlite3:sql_exec(DBPid, 
			     "create table messages (
                                id INTEGER PRIMARY KEY,
                                message_id INTEGER NOT NULL,
                                text TEXT, 
                                datetime INTEGER)")
    end.    

close_tables(Device)->
    ets:delete(Device).

restore_table(UserName, DBPid)->
    MessageMaxSizeOnMemory = message_box_config:get(message_max_size_on_memory),
    SqlResults = sqlite3:sql_exec(DBPid,
				  "select * from messages
                                     order by id desc limit :max", 
                                  [{':max', MessageMaxSizeOnMemory}]),
    Records = parse_message_records(SqlResults),
    Device = db_name(UserName),
    restore_records(Device, Records).

restore_records(Device, Records) ->
    case Records of
	[] -> ok;
	[Record | Tail] ->
	    ets:insert(Device, Record),
	    restore_records(Device, Tail)
    end.

%%
%% @doc export functions
%%

save_message(Pid, Text)->
    call(Pid, save_message, [Text]).

get_message(Id)->
    case util:get_user_from_message_id(Id) of
	{ok, User} ->
	    case user_db:get_pid(User#user.name) of
		{ok, UserPid} -> 
		    get_message(UserPid, Id);
		Other -> Other
	    end;
	Other -> Other
    end.

get_message(Pid, Id)->
    reference_call(Pid, get_message, [Id]).

get_sent_timeline(Pid, Count)->
    reference_call(Pid, get_sent_timeline, [Count]).

get_latest_message(Pid)->
    reference_call(Pid, get_latest_message, []).

%%
%% @doc remote call functions.
%%

call(Pid, Name, Args)->
    Pid ! {request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reference_call(Pid, Name, Args)->
    Pid ! {ref_request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reply(To, Pid, Result)->
    To ! {Pid, reply, Result}.

loop({User, DBPid}) ->
    UserName = User#user.name,
    Pid = self(),
    receive
	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_request(Name, [User, DBPid | Args]),
			  reply(From, Pid, Result)
		  end),
	    loop({User, DBPid});

	{request, From, Name, Args} ->
	    Result = handle_request(Name, [User, DBPid | Args]),
	    reply(From, Pid, Result),
	    loop({User, DBPid});
	{stop, From} ->
	    Device = db_name(UserName),
	    reply(From, Pid, close_tables(Device));

	{'EXIT', ExitPid, _Reason} ->
	    io:format("~p: user process(~p) is shutdown.~n", 
		      [?MODULE, ExitPid]),
	    close_tables(UserName)
    end.

%%
%% @doc server handlers
%%

handle_request(save_message, [User, DBPid, Text])->
    Id = get_max_id(DBPid) - 1,
    MessageId = get_message_id(User#user.id, Id),
    Message = #message{id = Id, 
                       message_id = MessageId, 
                       text = Text, 
		       datetime={date(), time()}},

    Device = db_name(User#user.name),
    MessageMaxSizeOnMemory = message_box_config:get(message_max_size_on_memory),
    ets:insert(Device, Message),
    insert_message_to_sqlite3(DBPid, Message),
    util:shurink_ets(Device, MessageMaxSizeOnMemory),
    {ok, MessageId};

handle_request(get_message, [User, _DBPid, MessageId])->
    MessagePattern = #message{id='$1', message_id=MessageId, text='_', 
			      datetime='_'},
    Device = db_name(User#user.name),
    case ets:match(Device, MessagePattern) of
	[] -> {error, not_found};
	[[Id]] -> 
	    case ets:lookup(Device, Id) of
		[Message0] ->
		    Message1 = Message0#message{user=User},
		    {ok, Message1};
		Other -> {error, Other}
	    end
    end;

handle_request(get_sent_timeline, [User, DBPid, Count])->
    Device = db_name(User#user.name),
    First = ets:first(Device),

    case ets:first(Device) of
	'$end_of_table' -> [];
	First ->
	    MessageIds = util:get_timeline_ids(Device, Count, First, [First]),
	    lists:map(fun(Id) -> 
                              case ets:lookup(Device, Id) of
                                  [Msg] -> Msg;
                                  [] -> get_message_from_db(DBPid, Id)
                              end
                      end,
		      MessageIds)
    end;
    

handle_request(get_latest_message, [User, _DBPid])->
    Device = db_name(User#user.name),
    case ets:first(Device) of
	'$end_of_table' -> {error, no_message_exist};
	Id -> 
	    [Message] = ets:lookup(Device, Id),
	    Message
    end.
	    
%%
%% @doc get message from sqlite3
%%
get_message_from_db(DBPid, Id) ->
    SqlResults = sqlite3:sql_exec(DBPid, 
                                  "select * from messages where id = :id",
                                  [{':id', Id}]),
    case SqlResults of
        [] ->
            {error, not_found};
        Records ->
            [Msg] = parse_message_records(Records),
            Msg
    end.

get_max_id(DBPid)->
    Result = sqlite3:sql_exec(DBPid, "select * from messages 
                                           order by id limit 1"),
    
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message.id
    end.

%%
%% @doc local functions.
%%

get_message_id(UserId, Id)->
    FormattedUserId = util:formatted_number(UserId, 9),
    FormattedId = util:formatted_number(abs(Id), 9),
    list_to_integer(string:concat(FormattedUserId, FormattedId)).

db_name(UserName)-> UserName.

%%
%% @doc parse message record from sqlite3 to erlang record.
%%

parse_message_records(Result) ->
    [{columns, _ColumnList}, {rows, RowList}] = Result,
    parse_message_records(RowList, []).

parse_message_records(RowList, RecordList) ->
    case RowList of
	[] -> lists:reverse(RecordList);
	[Row | Tail] -> 
	    {Id, MsgId, TextBin, Sec} = Row,
	    Datetime = calendar:gregorian_seconds_to_datetime(Sec),
	    Record = #message{id = Id, message_id = MsgId,
			      text = binary:bin_to_list(TextBin),
			      datetime = Datetime},
	    parse_message_records(Tail, [Record | RecordList])
    end.

insert_message_to_sqlite3(DBPid, Message) ->
    Sec = calendar:datetime_to_gregorian_seconds(Message#message.datetime),
    sqlite3:sql_exec(DBPid,
		    "insert into messages (id, message_id, text, datetime)
                        values(:id, :message_id, :text, :datetime)",
		    [{':id'        , Message#message.id},
		     {':message_id', Message#message.message_id},
		     {':text'      , Message#message.text},
		     {':datetime'  , Sec}]).
