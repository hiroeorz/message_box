%% File : home_db.erl
%% Description : users home time line database.

-module(home_db).
-include("message.hrl").
-include("user.hrl").

-export([init/1]).
-export([start/1, stop/1]).
-export([save_message_id/2, get_timeline/2]).

-define(DB_DIR, "./db/").

%%
%% @doc initial setup functions
%%

start(UserName)->
    spawn_link(?MODULE, init, [UserName]).

stop(Pid) ->
    Pid ! {stop, self()}.

init(UserName)->
    process_flag(trap_exit, true),
    Device = db_name(UserName),
    create_tables(Device),
    restore_table(Device),
    {ok, User} = user_db:lookup_name(UserName),
    loop(User).

create_tables(Device)->  
    ets:new(Device, [ordered_set, named_table, {keypos, #message_index.id}]),
    {DiscName, FileName} = dets_info(Device),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #message_index.id}]).

restore_table(Device)->
    Insert = fun(#message_index{id=_Id, message_id=_MessageId} = Message)->
		     ets:insert(Device, Message),
		     continue
	     end,
    {DiscName, _FileName} = dets_info(Device),
    dets:traverse(DiscName, Insert).

close_tables(Device)->
    ets:delete(Device),
    {DiscName, _FileName} = dets_info(Device),
    dets:close(DiscName).

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

loop(User) ->
    UserName = User#user.name,
    Pid = self(),
    receive
	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_request(Name, [User | Args]),
			  reply(From, Pid, Result)
		  end),
	    loop(User);

	{request, From, Name, Args} ->
	    Result = handle_request(Name, [User | Args]),
	    reply(From, Pid, Result),
	    loop(User);
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

handle_request(save_message_id, [User, MessageId])->
    Id = get_max_id(User#user.name) - 1,
    MessageIndex = #message_index{id=Id, message_id=MessageId},
    Device = db_name(User#user.name),
    {DiscName, _FileName} = dets_info(Device),
    ets:insert(Device, MessageIndex),
    dets:insert(DiscName, MessageIndex),
    {ok, MessageId};

handle_request(get_timeline, [User, Count])->
    Device = db_name(User#user.name),
    First = ets:first(Device),
    MessageIds = util:get_timeline_ids(Device, Count, First, [First]),
    lists:map(fun(Id) -> Id end, MessageIds).

%%
%% @doc local functions.
%%

get_max_id(UserName) ->
    Device = db_name(UserName),
    case ets:first(Device) of
	'$end_of_table' -> 0;
	First -> First
    end.

dets_info(Device)->
    DiscName = list_to_atom(atom_to_list(Device) ++ "_Home_Disk"),
    FileName = ?DB_DIR ++ atom_to_list(Device),
    {DiscName, FileName}.

db_name(UserName)-> 
    list_to_atom(atom_to_list(UserName) ++ "_home").

