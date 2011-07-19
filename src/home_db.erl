%% File : home_db.erl
%% Description : users home time line database.

-module(home_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").
-export([init/2]).
-export([start/2, stop/1]).
-export([save_message_id/2, get_timeline/2]).

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
    ets:new(Device, [ordered_set, named_table, {keypos, #message_index.id}]),
    create_sqlite3_tables(DBPid).

restore_table(UserName, DBPid)->
    SqlResults = sqlite3:sql_exec(DBPid,
				  "select * from home
                                     order by id desc limit 100"),
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

close_tables(Device)->
    ets:delete(Device).

create_sqlite3_tables(DBPid) ->
    case lists:member(home, sqlite3:list_tables(DBPid)) of
	true -> ok;
	false ->
	    sqlite3:sql_exec(DBPid, 
			     "create table home (
                                id INTEGER PRIMARY KEY,
                                message_id INTEGER NOT NULL)")
    end.

%%
%% @doc export functions
%%

save_message_id(Pid, MessageId) ->
    call(Pid, save_message_id, [MessageId]).

get_timeline(Pid, Count) ->
    reference_call(Pid, get_timeline, [Count]).

%%
%% @doc remote call functions.
%%

call(Pid, Name, Args) ->
    Pid ! {request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reference_call(Pid, Name, Args) ->
    Pid ! {ref_request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reply(To, Pid, Result) ->
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

	{'EXIT', ExitPid, Reason} ->
	    io:format("~p: user process(~p) is shutdown(Reason:~p).~n", 
		      [?MODULE, ExitPid, Reason]),
	    Device = db_name(UserName),
	    close_tables(Device)
    end.

%%
%% @doc server handlers
%%

handle_request(save_message_id, [User, DBPid, MessageId])->
    Id = get_max_id(DBPid) - 1,
    MessageIndex = #message_index{id=Id, message_id=MessageId},
    Device = db_name(User#user.name),
    ets:insert(Device, MessageIndex),
    insert_message_to_sqlite3(DBPid, MessageIndex),
    {ok, MessageId};

handle_request(get_timeline, [User, DBPid, Count]) ->
    Device = db_name(User#user.name),
    Pid = self(),
    MessageIds = get_timeline_ids(Device, DBPid, Count),

    Fun = fun(Id) ->
		  [MessageIndex] = ets:lookup(Device, Id),
		  MessageId = MessageIndex#message_index.message_id,
		  
		  Result = 
		      case util:get_user_from_message_id(MessageId) of
			  {ok, FollowerUser} ->
			      m_user:get_message(FollowerUser#user.id, 
						 MessageId);
			  Other -> {error, {Other, {not_found, MessageId}}}
		      end,
		  Pid ! {message_reply, Result}
	  end,
    lists:map(fun(Id) -> spawn(fun() -> Fun(Id) end) end, MessageIds),
    MessageList = collect_loop(Pid, length(MessageIds), []),
    lists:sort(fun(A, B) -> A#message.datetime > B#message.datetime end, 
	       MessageList).

collect_loop(Pid, Count, Result) ->
    if length(Result) >= Count -> Result;
       true ->
	    receive
		{message_reply, {ok, NewMessage}} ->
		    collect_loop(Pid, Count, [NewMessage | Result]);
		{message_reply, _Other} ->
		    collect_loop(Pid, Count, Result)
	    after 2000 -> Result
	    end
    end.

%%
%% @doc local functions.
%%

%%
%% @doc get message id list from local ets and sqlite3 database.
%%
get_timeline_ids(Device, DBPid, Count) ->
    MessageIdsFromEts = 
	case ets:first(Device) of
	    '$end_of_table' -> [];
	    First -> util:get_timeline_ids(Device, Count, First, [First])
	end,

    if length(MessageIdsFromEts) < Count -> 
	    get_message_ids_from_db(DBPid, Count, MessageIdsFromEts);
       true -> MessageIdsFromEts
    end.    

get_message_ids_from_db(_DBPid, _Count, []) -> [];
get_message_ids_from_db(DBPid, Count, MessageIdsFromEts) ->
	    SqlResult = 
		sqlite3:sql_exec(DBPid, "select * from home
                                           where id > :id
                                           order by id limit :limit",
				 [{':id', lists:max(MessageIdsFromEts)},
				  {':limit', 
				   Count - length(MessageIdsFromEts)}]),
	    
	    Ids = lists:map(fun(Record) -> Record#message_index.id end,
			    parse_message_records(SqlResult)),
	    MessageIdsFromEts ++ Ids.    

get_max_id(DBPid) ->
    Result = sqlite3:sql_exec(DBPid, "select * from home 
                                           order by id limit 1"),
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message_index.id
    end.

db_name(UserName)-> 
    list_to_atom(atom_to_list(UserName) ++ "_home").

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
	    {Id, MsgId} = Row,
	    Record = #message_index{id = Id, message_id = MsgId},
	    parse_message_records(Tail, [Record | RecordList])
    end.

insert_message_to_sqlite3(DBPid, MessageIndex) ->
    sqlite3:sql_exec(DBPid,
		    "insert into home (id, message_id)
                        values(:id, :message_id)",
		    [{':id'        , MessageIndex#message_index.id},
		     {':message_id', MessageIndex#message_index.message_id}]).
