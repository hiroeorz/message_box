%% File : message_box.erl
%% Description : system management module.

-module(message_box).
-include("user.hrl").
-export([start/0, stop/0, restart_all_users/0]).

start() ->
    user_db:start(),
    user_manager:start(),
    user_manager:start_all_users().

restart_all_users() ->
    io:format("restart all users~n", []),
    user_manager:stop(),
    user_manager:start(),
    user_manager:start_all_users().    

stop() ->
    user_manager:stop(),
    user_db:stop().
