%% File : mention_db.erl
%% Description : users reply time line database.

-module(mentions_db).
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").
-export([init/2]).
-export([start/2, stop/1]).
-export([save_message_id/2, get_timeline/2]).

%%
%% @doc start process
%%
-spec(start(atom(), pid()) -> pid()).

start(UserName, DBPid)->
    spawn_link(?MODULE, init, [UserName, DBPid]).

%%
%% @doc stop process
%%
-spec(stop(pid()) -> ok).

stop(Pid) ->
    Pid ! {stop, self()}.

init(UserName, DBPid)->
    process_flag(trap_exit, true),
    create_tables(UserName, DBPid),
    restore_table(UserName, DBPid),
    {ok, User} = user_db:lookup_name(UserName),
    loop({User, DBPid}).

%%
%% @doc create mentions databases.
%%
-spec(create_tables(atom(), pid()) -> ok).

create_tables(UserName, DBPid)->
    Device = db_name(UserName),  
    ets:new(Device, [ordered_set, named_table, {keypos, #message_index.id}]),
    create_sqlite3_tables(DBPid).

%%
%% @doc read data from file and write to ets table.
%%
-spec(restore_table(atom(), pid()) -> ok).

restore_table(UserName, DBPid)->
    MessageMaxSizeOnMemory = message_box_config:get(message_max_size_on_memory),
    SqlResults = sqlite3:sql_exec(DBPid,
				  "select * from mentions
                                     order by id desc limit :max", 
                                  [{':max', MessageMaxSizeOnMemory}]),
    Records = parse_message_records(SqlResults),
    Device = db_name(UserName),
    restore_records(Device, Records).

%%
%% @doc read data from file and write to ets table.
%%
-spec(restore_records(atom(), list(#message{})) -> ok).

restore_records(Device, Records) ->
    case Records of
	[] -> ok;
	[Record | Tail] ->
	    ets:insert(Device, Record),
	    restore_records(Device, Tail)
    end.

%%
%% @doc close table.
%%
-spec(close_tables(atom()) -> true).

close_tables(Device)->
    ets:delete(Device).

%%
%% @doc close table.
%%
-spec(create_sqlite3_tables(pid()) -> ok).

create_sqlite3_tables(DBPid) ->
    case lists:member(mentions, sqlite3:list_tables(DBPid)) of
	true -> ok;
	false ->
	    sqlite3:sql_exec(DBPid, 
			     "create table mentions (
                                id INTEGER PRIMARY KEY,
                                message_id INTEGER NOT NULL)")
    end.

%%
%% @doc save message_id to ets and sqlite3 database.
%%
-spec(save_message_id(pid(), #message{}) -> ok).

save_message_id(Pid, MessageId) ->
    call(Pid, save_message_id, [MessageId]).

%%
%% @doc create mentions timeline and return list of message.
%%
-spec(get_timeline(pid(), integer()) -> list(#message{})).

get_timeline(Pid, Count) ->
    reference_call(Pid, get_timeline, [Count]).

%%
%% @doc remote call function.
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

	{'EXIT', ExitPid, _Reason} ->
	    io:format("~p: user process(~p) is shutdown.~n", 
		      [?MODULE, ExitPid]),
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
    MessageMaxSizeOnMemory = message_box_config:get(message_max_size_on_memory),
    ets:insert(Device, MessageIndex),
    insert_message_to_sqlite3(DBPid, MessageIndex),
    util:shurink_ets(Device, MessageMaxSizeOnMemory),
    {ok, MessageId};

handle_request(get_timeline, [User, DBPid, Count])->
    Device = db_name(User#user.name),
    Pid = self(),
    MessageIds = get_timeline_ids(Device, DBPid, Count),

    Fun = fun(Id) ->
		  MessageIndex = case ets:lookup(Device, Id) of
                                     [Index] -> Index;
                                     [] -> get_message_from_db(DBPid, Id)
                                 end,

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

%%
%% @doc get message from sqlite3
%%
-spec(get_message_from_db(pid(), integer()) -> list(#message_index{}) ).

get_message_from_db(DBPid, Id) ->
    SqlResults = sqlite3:sql_exec(DBPid, 
                                  "select * from mentions where id = :id",
                                  [{':id', Id}]),
    case SqlResults of
        [] ->
            {error, not_found};
        Records ->
            [Msg] = parse_message_records(Records),
            Msg
    end.

%%
%% @doc collect other users message
%%
-spec(collect_loop(pid(), integer(), list()) -> list(#message{}) ).

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
%% @doc get id list of message.
%%
-spec(get_timeline_ids(atom(), pid(), integer()) -> list(#message_index{}) ).

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

%%
%% @doc get id list of message.
%%
-spec(get_message_ids_from_db(pid(), integer(), list(#message_index{})) -> 
             list(#message_index{}) ).

get_message_ids_from_db(_DBPid, _Count, []) -> [];
get_message_ids_from_db(DBPid, Count, MessageIdsFromEts) ->
	    SqlResult = 
		sqlite3:sql_exec(DBPid, "select * from mentions
                                           where id > :id
                                           order by id limit :limit",
				 [{':id', lists:max(MessageIdsFromEts)},
				  {':limit', 
				   Count - length(MessageIdsFromEts)}]),
	    
	    Ids = lists:map(fun(Record) -> Record#message_index.id end,
			    parse_message_records(SqlResult)),
	    MessageIdsFromEts ++ Ids.    

%%
%% @doc get max id for select next id.
%%
-spec(get_max_id(pid()) -> integer() ).

get_max_id(DBPid) ->
    Result = sqlite3:sql_exec(DBPid, "select * from mentions 
                                           order by id limit 1"),
    
    case parse_message_records(Result) of
	[] -> 0;
	[LastRecord] -> LastRecord#message_index.id
    end.

%%
%% @doc get max id for select next id.
%%
-spec(db_name(atom()) -> atom() ).

db_name(UserName)-> 
    list_to_atom(atom_to_list(UserName) ++ "_mentions").

%%
%% @doc parse message record from sqlite3 to erlang record.
%%
-spec(parse_message_records(list()) -> list(#message_index{}) ).

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

%%
%% @doc insert new message to sqlite3.
%%
-spec(insert_message_to_sqlite3(pid(), #message_index{}) -> 
             {rowid, integer()} | {error, integer(), string()} ).

insert_message_to_sqlite3(DBPid, MessageIndex) ->
    sqlite3:sql_exec(DBPid,
		    "insert into mentions (id, message_id)
                        values(:id, :message_id)",
		    [{':id'        , MessageIndex#message_index.id},
		     {':message_id', MessageIndex#message_index.message_id}]).
