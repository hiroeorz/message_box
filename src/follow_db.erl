%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follow_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([save_follow_user/2, delete_follow_user/2, get_follow_ids/1, 
	 map_do/2, is_follow/2]).

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
    ets:new(Device, [ordered_set, named_table, {keypos, #follow.id}]),
    {DiscName, FileName} = dets_info(Device),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #follow.id}]).

restore_table(Device)->
    Insert = fun(#follow{id=_Id, datetime=_DateTime} = Message)->
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

save_follow_user(Pid, Id) ->
    call(Pid, save_follow_user, [Id]).

delete_follow_user(Pid, Id) ->
    call(Pid, delete_follow_user, [Id]).

get_follow_ids(Pid) ->
    reference_call(Pid, get_follow_ids, []).

map_do(Pid, Fun) ->
    call(Pid, map_do, [Fun]).

is_follow(Pid, UserId) ->
    reference_call(Pid, is_follow, [UserId]).

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

handle_request(save_follow_user, [User, Id]) ->
    Follow = #follow{id=Id, datetime={date(), time()}},
    
    case is_following(User, Id) of
	true -> {error, already_following};
	false ->
	    Device = db_name(User#user.name),
	    {DiscName, _} = dets_info(Device),
	    ets:insert(Device, Follow),
	    dets:insert(DiscName, Follow),
	    ok
    end;

handle_request(delete_follow_user, [User, Id]) ->    
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
	    [Follow] = ets:lookup(Device, First),
	    Fun(Follow),
	    map_do(Device, Fun, First)
    end;

handle_request(get_follow_ids, [User]) ->
    Device = db_name(User#user.name),
    case ets:first(Device) of
	'$end_of_table' -> [];
	First -> collect_id(Device, First, [First])
    end;

handle_request(is_follow, [User, FollowId]) ->
    Device = db_name(User#user.name),
    case ets:lookup(Device, FollowId) of
	[_FollowingUser] -> true;
	[] -> false
    end.
	    
%%
%% @doc local functions.
%%

collect_id(Device, Before, Result) ->
    case ets:next(Device, Before) of
	'$end_of_table' -> Result;
	FollowId -> collect_id(Device, FollowId, [FollowId | Result])
    end.	    

is_following(User, Id) ->
    Device = db_name(User#user.name),
    case ets:lookup(Device, Id) of
	[_Follow] -> true;
	[] -> false
    end.    

dets_info(Device)->
    DiscName = list_to_atom(atom_to_list(Device) ++ "_Follow_Disk"),
    {ok, DB_DIR} = application:get_env(message_box, database_dir),
    FileName = DB_DIR ++ atom_to_list(Device),
    {DiscName, FileName}.

db_name(UserName)-> 
    list_to_atom(atom_to_list(UserName) ++ "_follow").

map_do(Device, Fun, Entry) ->
    case ets:next(Device, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [Follow] = ets:lookup(Device, Next),
	    Fun(Follow),
	    map_do(Device, Fun, Next)
    end.
