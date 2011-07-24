%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follower_db).
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([save_follower/2, delete_follower/2, get_follower_ids/1, map_do/2]).

%%
%% @doc initial setup functions
%%

start(UserName)->
    spawn_link(?MODULE, init, [UserName]).

stop(Pid) ->
    Pid ! {stop, self()}.

init(UserName) ->
    process_flag(trap_exit, true),
    Device = db_name(UserName),
    create_tables(Device),
    restore_table(Device),
    {ok, User} = user_db:lookup_name(UserName),
    loop(User).

create_tables(Device)->  
    ets:new(Device, [ordered_set, named_table, {keypos, #follower.id}]),
    {DiscName, FileName} = dets_info(Device),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #follower.id}]).

restore_table(Device)->
    Insert = fun(#follower{id=_Id, datetime=_DateTime} = Message)->
		     ets:insert(Device, Message),
		     continue
	     end,
    {DiscName, _FileName} = dets_info(Device),
    dets:traverse(DiscName, Insert).

close_tables(Device) ->
    ets:delete(Device),
    {DiscName, _FileName} = dets_info(Device),
    dets:close(DiscName).

%%
%% @doc export functions
%%

save_follower(Pid, Id) ->
    call(Pid, save_follower, [Id]).

delete_follower(Pid, Id) ->
    call(Pid, delete_follower, [Id]).

get_follower_ids(Pid) ->
    reference_call(Pid, get_follower_ids, []).

map_do(Pid, Fun) ->
    call(Pid, map_do, [Fun]).

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

handle_request(save_follower, [User, Id]) ->
    Follower = #follower{id=Id, datetime={date(), time()}},
    
    case is_following(User, Id) of
	true -> {error, already_following};
	false ->
	    Device = db_name(User#user.name),
	    {DiscName, _} = dets_info(Device),
	    ets:insert(Device, Follower),
	    dets:insert(DiscName, Follower),
	    ok
    end;

handle_request(delete_follower, [User, Id]) ->
    case is_following(User, Id) of
	true ->
	    Device = db_name(User#user.name),
	    {DiscName, _} = dets_info(Device),
	    ets:delete(Device, Id),
	    dets:delete(DiscName, Id),
	    {ok, deleted};
	false -> {error, not_following}
    end;

handle_request(map_do, [User, Fun]) ->
    Device = db_name(User#user.name),
    case ets:first(Device) of
	'$end_of_table' ->
	    ok;
	First ->
	    [Follower] = ets:lookup(Device, First),
	    Fun(Follower),
	    map_do(Device, Fun, First)
    end;

handle_request(get_follower_ids, [User]) ->
    Device = db_name(User#user.name),
    case ets:first(Device) of
	'$end_of_table' -> [];
	First -> collect_id(Device, First, [First])
    end.
    

%%
%% @doc local functions.
%%

collect_id(Device, Before, Result) ->
    case ets:next(Device, Before) of
	'$end_of_table' -> Result;
	FollowerId -> collect_id(Device, FollowerId, [FollowerId | Result])
    end.	    

is_following(User, Id) ->
    Device = db_name(User#user.name),
    case ets:lookup(Device, Id) of
	[_Follower] -> true;
	[] -> false
    end.    

dets_info(Device)->
    DiscName = list_to_atom(atom_to_list(Device) ++ "_Follower_Disk"),
    DB_DIR = message_box_config:get(database_dir),
    FileName = DB_DIR ++ atom_to_list(Device),
    {DiscName, FileName}.

db_name(UserName)-> 
    list_to_atom(atom_to_list(UserName) ++ "_follower").

map_do(Device, Fun, Entry) ->
    case ets:next(Device, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [Follower] = ets:lookup(Device, Next),
	    Fun(Follower),
	    map_do(Device, Fun, Next)
    end.
