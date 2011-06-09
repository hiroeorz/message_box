%% File : usr.erl
%% Description : Include file for user_db

-module(user_db).
-include("user.hrl").
-export([start/0, start/1, stop/0,
	 create_tables/1, close_tables/0, restore_table/0,
	 add_user/1, add_user/3, update_user/1, delete_user/1, lookup_id/1]).

start() ->
    start("/tmp/user_db").

start(FileName)->
    create_tables(FileName),
    restore_table().

stop()->
    close_tables().

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

add_user(Id, Name, Status)->
    add_user(#user{id=Id, name=Name, status=Status}).

add_user(#user{id=_UserID, name=_UserName, status=_Status} = User) ->
    ets:insert(userRam, User),
    dets:insert(userDisk, User),
    ok.

update_user(#user{id=_UserID, name=_UserName, status=_Status} = User)->
    add_user(User).

delete_user(Id)->
    ets:delete(userRam, Id),
    dets:delete(userDisk, Id).

lookup_id(Id)->
    case ets:lookup(userRam, Id) of
	[User] -> {ok, User};
	[] -> {error, instance}
    end.
	    
