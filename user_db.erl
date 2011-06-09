%% File : usr.erl
%% Description : Include file for user_db

-module(user_db).
-include("user.hrl").

-export([start/0, stop/0, loop/1,
	 call/2, reply/2, 
	 add_user/3, update_user/1, delete_user/1, lookup_id/1]).
-export([init/1]).

%%
%% spawn remote database process.
%%
start() ->
    register(?MODULE, spawn(?MODULE, init, ["/tmp/user_db"])).

%%
%% client functions.
%%

add_user(Id, Name, Status)->
    call(add_user, [Id, Name, Status]).

update_user(#user{id=_UserID, name=_UserName, status=_Status} = User)->
    call(update_user, [User]).

delete_user(Id)->
    call(delete_user, [Id]).

lookup_id(Id)->
    reference_call(lookup_id, [Id]).

%%
%% initial setup functions.
%%

init(FileName)->
    create_tables(FileName),
    restore_table(),
    loop(ok).

create_tables(FileName) ->
    ets:new(userRam, [named_table, {keypos, #user.id}]),
    dets:open_file(userDisk, [{file, FileName}, {keypos, #user.id}]).

close_tables()->
    ets:delete(userRam),
    dets:close(userDisk).

restore_table()->
    Insert = fun(#user{id=_UserID, name=_UserName, status=_Status} = User)->
		     ets:insert(userRam, User),
		     continue
	     end,
    dets:traverse(userDisk, Insert).

%%
%% remote call functions.
%%

call(Name, Args)->
    ?MODULE ! {request, self(), Name, Args},
    receive
	{reply, Result} -> Result
    end.

reference_call(Name, Args)->
    ?MODULE ! {ref_request, self(), Name, Args},
    receive
	{reply, Result} -> Result
    end.

reply(To, Result)->
    To ! {reply, Result}.

stop() ->
    ?MODULE ! {stop, self()}.

loop(State)->
    receive
	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_msg(Name, Args),
			  reply(From, Result)
		  end),
	    loop(State);

	{request, From, Name, Args} ->
	    Result = handle_msg(Name, Args),
	    reply(From, Result),
	    loop(State);
	{stop, From}->
	    reply(From, close_tables());
	_Other->
	    reply(error, unknownMessage)
    end.

%%
%% server handlers.
%%

handle_msg(add_user, [Id, Name, Status])->
    User = #user{id=Id, name=Name, status=Status},
    ets:insert(userRam, User),
    dets:insert(userDisk, User),
    ok;

handle_msg(update_user, [User])->
    ets:insert(userRam, User),
    dets:insert(userDisk, User),
    ok;

handle_msg(delete_user, [Id])->
    ets:delete(userRam, Id),
    dets:delete(userDisk, Id);

handle_msg(lookup_id, [Id])->
    case ets:lookup(userRam, Id) of
	[User] -> {ok, User};
	[] -> {error, instance}
    end.
	    
