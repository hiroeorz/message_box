%% File : message_box.erl
%% Description : system management module.

-module(message_box).
-include("user.hrl").
-export([init/0]).
-export([start/0, stop/0]).
-export([get_message/1, create_user/1, send_message/2]).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    user_db:start(),
    user_manager:start(),
    user_manager:start_all_users(),
    process_flag(trap_exit, true),
    loop({date()}).

stop() ->
    call(stop, []).

%%
%% @doc export functions
%%

get_message(MessageId) ->
    spawn_call(get_message, [MessageId]).

create_user(UserName) ->
    spawn_call(create_user, [UserName]).

send_message(Id, Message) ->
    spawn_call(send_message, [Id, Message]).

%%
%% @doc remote call functions.
%%

call(stop, Args)->
    ?MODULE ! {request, self(), stop, Args};

call(Name, Args)->
    Pid = whereis(?MODULE),
    Pid ! {request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

spawn_call(Name, Args)->
    Pid = whereis(?MODULE),
    Pid ! {spawn_request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reply(To, Pid, Result)->
    To ! {Pid, reply, Result}.

loop({_StartTime}=State) ->
    Pid = self(),
    receive
	{request, From, stop, _Args} ->
	    handle_stop([From]);

	{request, From, Name, Args} ->
	    Result = handle_request(Name, Args),
	    reply(From, Pid, Result),
	    loop(State);

	{spawn_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_request(Name, Args),
			  reply(From, Pid, Result)
		  end),
	    loop(State);

	{'EXIT', ExitPid, _Reason} ->
	    io:format("~p: process(~p) is shutdown.~n", [?MODULE, ExitPid]),
	    loop(State)
    end.

%%
%% @doc server handlers
%%

handle_stop([From]) ->
    io:format("~p stopped from ~p~n", [?MODULE, From]),
    exit(from_root).

handle_request(get_message, [MessageId]) ->
    message_db:get_message(MessageId);

handle_request(create_user, [UserName]) ->
    {ok, User} = user_db:add_user(UserName),
    Pid = m_user:start(User#user.name),
    AssignedUser = User#user{pid = Pid},
    {ok, AssignedUser};

handle_request(send_message, [Id, Message]) ->
    m_user:send_message(Id, Message).

