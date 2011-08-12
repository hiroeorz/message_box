%%
%% @author HIROE Shin <twitter: http://twitter.com/#!/hiroe_orz17>
%% @doc MessageBox is Twitter clone using Erlang (for plactice).
%% @copyright 2011 HIROE Shin
%%

-module(message_box).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("user.hrl").
-include("message.hrl").
-export([init/0]).
-export([start/0, start/1, stop/0]).
-export([authenticate/2, create_user/3, update_user/4, 
	 send_message/3, get_message/1,
	 follow/3, unfollow/3, is_follow/2,
	 get_home_timeline/2, get_mentions_timeline/2, get_sent_timeline/2,
	 save_icon/3, get_icon/1, get_user/1]).

%%
%% @doc stating system using default setting file(conf/message_box.conf).
%%
-spec(start() -> pid()).

start() ->
    message_box_config:load(),
    register(?MODULE, spawn(?MODULE, init, [])).

%%
%% @doc stating system.
%%
-spec(start(string()) -> pid).

start(ConfigFilePath) ->
    Path = filename:absname(ConfigFilePath),
    message_box_config:load(Path),
    register(?MODULE, spawn(?MODULE, init, [])).

%%
%% @doc system initializer.
%%
init() ->
    crypto:start(),
    UserDbFilePath = message_box_config:get(user_db_file_path),

    case node() of
	nonode@nohost -> ok;
	Node -> 
	    Cookie = message_box_config:get(cookie),
	    erlang:set_cookie(Node, Cookie)
    end,

    user_db:start(UserDbFilePath),
    user_manager:start(),
    user_manager:start_all_users(),
    start_ruby_server(),
    process_flag(trap_exit, true),
    loop({date()}).

%%
%% @doc all system shuting down.
%%
stop() ->
    call(stop, []).

start_ruby_server() ->
    Port = message_box_config:get(ruby_port),
    start_ruby_server(Port).

start_ruby_server(Port) ->
    spawn_link(rulang, start_server, [Port]).

%%
%% @doc user authentication.
%%
-spec(authenticate(string()|atom(), string()) -> 
             {ok, #user{}} | {error,unauthenticated}).

authenticate(UserName, Password) when is_list(UserName) ->
    authenticate(list_to_atom(UserName), Password);

authenticate(UserName, Password) when is_atom(UserName) ->
    spawn_call(authenticate, [UserName, Password]).    

%%
%% @doc get message record.
%%
-spec(get_message(integer()) -> {ok, #message{}} | {error, not_found}).

get_message(MessageId) ->
    spawn_call(get_message, [MessageId]).

%%
%% @doc create new user.
%%
-spec(create_user(string()|atom(), string(), string()) -> 
             {ok, #user{}} | {error, already_exist}).

create_user(UserName, Mail, Password) when is_list(UserName) ->
    create_user(list_to_atom(UserName), Mail, Password);

create_user(UserName, Mail, Password) when is_atom(UserName) ->
    spawn_call(create_user, [UserName, Mail, Password]).

%%
%% @doc update user settings.
%%
-spec(update_user(string()|atom(), string(), string(), string()) -> 
             {ok, #user{}} | {error, user_not_exist}).

update_user(UserName, AuthPassword, Mail, Password) when is_list(UserName) ->
    update_user(list_to_atom(UserName), AuthPassword, Mail, Password);

update_user(UserName, AuthPassword, Mail, Password) when is_atom(UserName) ->
    spawn_call(update_user, [UserName, AuthPassword, Mail, Password]).

%%
%% @doc send message to timeline.
%%
-spec(send_message(integer(), binary()|string(), binary()) -> 
             {ok, integer()} | {error,unauthenticated}).

send_message(Id, Password, Message) when is_binary(Message) ->
    spawn_call(send_message, [Id, Password, Message]).

%%
%% @doc follow other user.
%%
-spec(follow(integer(), string(), integer()) -> ok | {error,already_following}).

follow(UserId1, Password, UserId2) ->
    spawn_call(follow, [UserId1, Password, UserId2]).

%%
%% @doc unfollow other user.
%%
-spec(unfollow(integer(), string(), integer()) -> ok | {error,not_following}).

unfollow(UserId1, Password, UserId2) ->
    spawn_call(unfollow, [UserId1, Password, UserId2]).

%%
%% @doc get home timeline list. sorted by recentry time.
%%
-spec(get_home_timeline(integer()|string(), integer()) -> 
             list(message) | {error,not_found}).

get_home_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_home_timeline, [UserId_OR_Name, Count]).    

%%
%% @doc get mentions timeline list. sorted by recentry time.
%%
-spec(get_mentions_timeline(integer()|string(), integer()) -> 
             list(message) | {error,not_found}).

get_mentions_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_mentions_timeline, [UserId_OR_Name, Count]).    

%%
%% @doc get sent timeline list. sorted by recentry time.
%%
-spec(get_sent_timeline(integer()|string(), integer()) -> 
             list(message) | {error,not_found}).

get_sent_timeline(UserId_OR_Name, Count) ->
    spawn_call(get_sent_timeline, [UserId_OR_Name, Count]).    

%%
%% @doc check following other user.
%%
-spec(is_follow(integer()|string(), integer()) -> true | false | {error,not_found}).

is_follow(UserId_OR_Name, Id) ->
    spawn_call(is_follow, [UserId_OR_Name, Id]).

%%
%% @doc save icon to disc.
%%
-spec(save_icon(integer()|string()|atom(), binary(), string()) -> ok).

save_icon(UserId_OR_Name, Data, ContentType) when is_list(UserId_OR_Name) ->
    save_icon(list_to_atom(UserId_OR_Name), Data, ContentType);

save_icon(UserId_OR_Name, Data, ContentType) when is_binary(Data) ->
    spawn_call(save_icon, [UserId_OR_Name, Data, ContentType]).

%%
%% @doc get icon data.
%%
-spec(get_icon(integer()|string()) -> 
             {ok, binary(), string()} | {error,not_found} | {error,not_exist}).

get_icon(UserId_OR_Name) ->
    spawn_call(get_icon, [UserId_OR_Name]).

%%
%% @doc: return {ok,[{id,4},{name,"hoge"},{mail,"hoge@mail.com"}]}
%%
-spec(get_user(string()) -> {ok, list(tuple())} | {error,not_found}).

get_user(UserName) ->
    spawn_call(get_user, [UserName]).

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
    case user_db:lookup_name(UserName) of
    {ok, User} -> 
	    case util:authenticate(User, Password) of
		{ok, authenticated} -> 
		    OneTimePassword = util:get_onetime_password(User, 
								Password),
		    m_user:set_onetime_password(User#user.id, OneTimePassword),
		    {ok, OneTimePassword, User};
		Other -> Other
	    end;

	_Other -> {error, unauthenticated}
    end;

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

handle_request(update_user, [UserName, AuthPassword, Mail, Password]) ->
    case user_db:lookup_name(UserName) of
	{ok, _User} ->
	    m_user:update(UserName, AuthPassword, Mail, Password);
	_Other -> 
	    {error, user_not_exist}
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

handle_request(save_icon, [UserId_OR_Name, Data, ContentType]) ->
    case Data of
	<<>> -> 
	    {error, empty_data};
	_ ->
	    m_user:save_icon(UserId_OR_Name, Data, ContentType)
	end;

handle_request(get_icon, [UserId_OR_Name]) ->
    m_user:get_icon(UserId_OR_Name);

handle_request(get_user, [UserName]) ->
    case user_db:lookup_name(UserName) of
	{ok, User} ->
	    FormattedUser = [{id, User#user.id}, 
			     {name, atom_to_list(User#user.name)},
			     {mail, User#user.mail}],
	    {ok, FormattedUser};
	Other ->
	    Other
    end.
