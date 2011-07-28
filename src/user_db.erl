%% File : usr.erl
%% Description : Include file for user_db

-module(user_db).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("user.hrl").
-export([init/1]).
-export([start/0, start/1, stop/0, loop/1,
	 call/2, reply/3, 
	 add_user/3, update_user/1, delete_user/1, 
	 lookup_id/1, lookup_name/1, lookup_pid/1,
	 map_do/1, save_pid/2, get_pid/1]).

%%
%% initial setup functions.
%%

start() ->
    message_box_config:load(),
    UserDbFilePath = message_box_config:get(user_db_file_path),
    register(?MODULE, spawn_link(?MODULE, init, [UserDbFilePath])).

start(FileName) ->
    register(?MODULE, spawn_link(?MODULE, init, [FileName])).

init(FileName)->
    process_flag(trap_exit, true),
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
    Insert = fun(#user{id=_UserID, name=_UserName, status=_Status,
		       mail=_Mail, password=_Password} = User)->
		     ets:insert(userRam, User),
		     continue
	     end,
    dets:traverse(userDisk, Insert).


%%
%% export functions.
%%

add_user(Name, Mail, Password)->
    Status = true,
    call(add_user, [Name, Status, Mail, Password]).

update_user(#user{id=_UserID, name=_UserName, status=_Status,
		  mail=_Mail, password=_Password} = User)->
    call(update_user, [User]).

delete_user(Id)->
    call(delete_user, [Id]).

lookup_id(Id)->
    reference_call(lookup_id, [Id]).

lookup_name(Name) when is_list(Name)->
    lookup_name(list_to_atom(Name));

lookup_name(Name) when is_atom(Name) ->
    reference_call(lookup_name, [Name]).

lookup_pid(Pid) ->
    reference_call(lookup_pid, [Pid]).

map_do(Fun) ->
    call(map_do, [Fun]).

save_pid(Id, Pid) ->
    call(save_pid, [Id, Pid]).

get_pid(UserName_OR_Id)  ->
    reference_call(get_pid, [UserName_OR_Id]).

%%
%% remote call functions.
%%

call(Name, Args)->
    Pid = whereis(?MODULE),
    Pid ! {request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 30000 -> {error, timeout}
    end.

reference_call(Name, Args)->
    Pid = whereis(?MODULE),
    Pid ! {ref_request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 30000 -> {error, timeout}
    end.

reply(To, Pid, Result)->
    To ! {Pid, reply, Result}.

stop() ->
    ?MODULE ! {stop, self()}.

loop(State)->
    Pid = self(),
    receive
	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_request(Name, Args),
			  reply(From, Pid, Result)
		  end),
	    loop(State);

	{request, From, Name, Args} ->
	    Result = handle_request(Name, Args),
	    reply(From, Pid, Result),
	    loop(State);

	{stop, From}-> handle_stop([From, normal]);

	{'EXIT', ExitPid, from_root} -> handle_stop([ExitPid, from_root]);

	{'EXIT', ExitPid, Reason} ->
	    io:format("~p exit received from ~p Reason:~p~n", 
		      [?MODULE, ExitPid, Reason]),
	    loop(State);

	_Other-> reply(error, Pid, unknownMessage)
    end.

%%
%% server handlers.
%%

handle_stop([From, Reason]) ->
    io:format("~p stopped from ~p~n", [?MODULE, From]),
    close_tables(),
    exit(Reason).

handle_request(add_user, [Name, Status, Mail, Password])->
    case get_user_by_name(Name) of
	{ok, _User} -> {error, already_exist};
	{error, not_found} ->
	    NextUserId = case ets:last(userRam) of
			     '$end_of_table' -> 1;
			     UserId -> UserId + 1
			 end,
	    User = #user{id=NextUserId, name=Name, status=Status,
			 mail=Mail, password=undefined},
	    MD5Password = util:get_md5_password(User, Password),
	    NewUser = User#user{password=MD5Password},

	    ets:insert(userRam, NewUser),
	    dets:insert(userDisk, NewUser),
	    {ok, NewUser}
    end;

handle_request(update_user, [User])->
    case get_user_by_id(User#user.id) of
	{ok, _} ->
	    ets:insert(userRam, User),
	    dets:insert(userDisk, User),
	    {ok, User};
	{error, not_found} -> {error, not_found}
    end;

handle_request(delete_user, [Id])->
    ets:delete(userRam, Id),
    dets:delete(userDisk, Id);

handle_request(lookup_id, [Id])->
    get_user_by_id(Id);

handle_request(lookup_name, [Name])->
    get_user_by_name(Name);

handle_request(lookup_pid, [Pid])->
    get_user_by_pid(Pid);

handle_request(map_do, [Fun]) ->
    case ets:first(userRam) of
	'$end_of_table' ->
	    ok;
	First ->
	    [User] = ets:lookup(userRam, First),
	    Fun(User),
	    map_do(Fun, First)
    end;

handle_request(save_pid, [Id, Pid])->
    case get_user_by_id(Id) of
	{ok, User} ->
	    UpdatedUser = User#user{pid=Pid},
	    ets:insert(userRam, UpdatedUser),
	    dets:insert(userDisk, UpdatedUser),
	    ok;
	{error, not_found} -> {error, not_found}
    end;

handle_request(get_pid, [UserName]) when is_list(UserName) ->
    case get_user_by_name(list_to_atom(UserName)) of
	{ok, User} -> {ok, User#user.pid};
	Other -> Other
    end;

handle_request(get_pid, [UserName]) when is_atom(UserName) ->
    case get_user_by_name(UserName) of
	{ok, User} -> {ok, User#user.pid};
	Other -> Other
    end;

handle_request(get_pid, [UserId]) when is_integer(UserId) ->
    case get_user_by_id(UserId) of
	{ok, User} -> {ok, User#user.pid};
	Other -> Other
    end.

%%
%% local functions.
%%

get_user_by_pid(Pid) ->
    Pattern = #user{id='$1', name='_', status='_', pid=Pid, 
		    mail='_', password='_'},
    case ets:match(userRam, Pattern) of
	[]-> {error, not_found};
	[[UserId]] -> get_user_by_id(UserId)
    end.

get_user_by_name(Name) ->
    Pattern = #user{id='$1', name=Name, status='_', pid='_', 
		    mail='_', password='_'},
    case ets:match(userRam, Pattern) of
	[]-> {error, not_found};
	[[UserId]] -> get_user_by_id(UserId)
    end.

get_user_by_id(Id) ->
    case ets:lookup(userRam, Id) of
	[] -> {error, not_found};
	[User] -> {ok, User}
    end.

map_do(Fun, Entry) ->
    case ets:next(userRam, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [User] = ets:lookup(userRam, Next),
	    Fun(User),
	    map_do(Fun, Next)
    end.
