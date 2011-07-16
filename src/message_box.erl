%% File : message_box.erl
%% Description : system management module.

-module(message_box).
-include("user.hrl").
-export([init/1]).
-export([start/0, start/1, stop/0]).
-export([get_message/1, create_user/1, send_message/2, follow/2, is_follow/2,
	 get_home_timeline/2, get_mentions_timeline/2, get_sent_timeline/2]).

start() ->
    register(?MODULE, spawn(?MODULE, init, [?USER_DB_FILENAME])).

start(UserDbFilePath) ->
    register(?MODULE, spawn(?MODULE, init, [UserDbFilePath])).

init(UserDbFilePath) ->
    user_db:start(UserDbFilePath),
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

follow(UserId1, UserId2) ->
    spawn_call(follow, [UserId1, UserId2]).

get_home_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_home_timeline, [UserId_OR_Name, Count]).    

get_mentions_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_mentions_timeline, [UserId_OR_Name, Count]).    

get_sent_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_sent_timeline, [UserId_OR_Name, Count]).    

is_follow(UserId_OR_Name, Id) ->
    spawn_call(is_follow, [UserId_OR_Name, Id]).

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
    case user_db:add_user(UserName) of
	{ok, User} ->
	    Pid = m_user:start(User#user.name),
	    AssignedUser = User#user{pid = Pid},
	    {ok, AssignedUser};
	{error, already_exist} ->
	    {error, already_exist}
    end;

handle_request(send_message, [Id, Message]) ->
    m_user:send_message(Id, Message);

handle_request(follow, [UserId1, UserId2]) ->
    m_user:follow(UserId1, UserId2);

handle_request(get_home_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_home_timeline(UserId_OR_Name, Count);

handle_request(get_mentions_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_mentions_timeline(UserId_OR_Name, Count);

handle_request(get_sent_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_sent_timeline(UserId_OR_Name, Count);
   
handle_request(is_follow, [UserId_OR_Name, Id]) ->
    m_user:is_follow(UserId_OR_Name, Id).

