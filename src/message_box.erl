%% File : message_box.erl
%% Description : system management module.

-module(message_box).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("user.hrl").
-export([init/0]).
-export([start/0, start/1, stop/0]).
-export([authenticate/2, get_message/1, create_user/3, send_message/3, 
	 follow/3, unfollow/3, is_follow/2,
	 get_home_timeline/2, get_mentions_timeline/2, get_sent_timeline/2,
	 save_icon/2, get_icon/1]).

start() ->
    message_box_config:load(),
    register(?MODULE, spawn(?MODULE, init, [])).

start(ConfigFilePath) ->
    Path = filename:absname(ConfigFilePath),
    message_box_config:load(Path),
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    crypto:start(),
    UserDbFilePath = message_box_config:get(user_db_file_path),
    user_db:start(UserDbFilePath),
    user_manager:start(),
    user_manager:start_all_users(),
    start_ruby_server(),
    process_flag(trap_exit, true),
    loop({date()}).

stop() ->
    call(stop, []).

start_ruby_server() ->
    Port = message_box_config:get(ruby_port),
    start_ruby_server(Port).

start_ruby_server(Port) ->
    spawn_link(rulang, start_server, [Port]).

%%
%% @doc export functions
%%

authenticate(UserName, Password) ->
    spawn_call(authenticate, [UserName, Password]).    

get_message(MessageId) ->
    spawn_call(get_message, [MessageId]).

create_user(UserName, Mail, Password) ->
    spawn_call(create_user, [UserName, Mail, Password]).

send_message(Id, Password, Message) ->
    spawn_call(send_message, [Id, Password, Message]).

follow(UserId1, Password, UserId2) ->
    spawn_call(follow, [UserId1, Password, UserId2]).

unfollow(UserId1, Password, UserId2) ->
    spawn_call(unfollow, [UserId1, Password, UserId2]).

get_home_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_home_timeline, [UserId_OR_Name, Count]).    

get_mentions_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_mentions_timeline, [UserId_OR_Name, Count]).    

get_sent_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_sent_timeline, [UserId_OR_Name, Count]).    

is_follow(UserId_OR_Name, Id) ->
    spawn_call(is_follow, [UserId_OR_Name, Id]).

save_icon(UserId_OR_Name, Data) when is_binary(Data) ->
    call(save_icon, [UserId_OR_Name, Data]).

get_icon(UserId_OR_Name) ->
    call(get_icon, [UserId_OR_Name]).

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

handle_request(authenticate, [UserName, Password]) ->
    User = user_db:lookup_name(UserName),
    util:authenticate(User, Password);

handle_request(get_message, [MessageId]) ->
    message_db:get_message(MessageId);

handle_request(create_user, [UserName, Mail, Password]) ->
    case user_db:add_user(UserName, Mail, Password) of
	{ok, User} ->
	    Pid = m_user:start(User#user.name),
	    AssignedUser = User#user{pid = Pid},
	    {ok, AssignedUser};
	{error, already_exist} ->
	    {error, already_exist}
    end;

handle_request(send_message, [Id, Password, Message]) ->
    m_user:send_message(Id, Password, Message);

handle_request(follow, [UserId1, Password, UserId2]) ->
    m_user:follow(UserId1, Password, UserId2);

handle_request(unfollow, [UserId1, Password, UserId2]) ->
    m_user:unfollow(UserId1, Password, UserId2);

handle_request(get_home_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_home_timeline(UserId_OR_Name, Count);

handle_request(get_mentions_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_mentions_timeline(UserId_OR_Name, Count);

handle_request(get_sent_timeline, [UserId_OR_Name, Count]) ->
    m_user:get_sent_timeline(UserId_OR_Name, Count);
   
handle_request(is_follow, [UserId_OR_Name, Id]) ->
    m_user:is_follow(UserId_OR_Name, Id);

handle_request(save_icon, [UserId_OR_Name, Data]) ->
    m_user:save_icon(UserId_OR_Name, Data);

handle_request(get_icon, [UserId_OR_Name]) ->
    m_user:get_icon(UserId_OR_Name).


