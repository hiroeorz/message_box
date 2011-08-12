%% File: user.erl
%% Description : user handler module.

-module(m_user).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([update/4,
	 send_message/3, get_message/2, get_sent_timeline/2, 
	 get_home_timeline/2, save_to_home/2, save_to_home/3,
	 follow/3, unfollow/3, add_follower/2, delete_follower/2,
	 get_follower_ids/1, is_follow/2,
	 save_to_mentions/2, get_mentions_timeline/2,
         save_icon/3, get_icon/1, set_onetime_password/2]).

-define(USER_MANAGER, user_manager).
-define(MaxSessionCount, 10).
-define(FILE_EXTENTION_LIST, [".jpg", ".gif", ".png"]).
-define(CONTENT_TYPE_LIST, [{".jpg", "image/jpg"}, 
			    {".png", "image/png"}, 
			    {".gif", "image/gif"}]).

-record(user_state, {user,                     %% user record
                     message_db_pid,           %% pid
                     home_db_pid,              %% pid
                     follower_db_pid,          %% pid
                     follow_db_pid,            %% pid
                     mentions_db_pid,          %% pid
                     one_time_password_list}). %% list

start(UserName) ->
    spawn_link(?MODULE, init, [UserName]).

init(UserName) ->
    process_flag(trap_exit, true),
    ManagerPid = whereis(?USER_MANAGER),
    link(ManagerPid),

    {ok, User} = user_db:lookup_name(UserName),

    {DiscName, Path} = util:db_info(UserName),
    {ok, DBPid} = sqlite3:open(DiscName, [{file, Path}]),
    MessageDB_Pid = message_db:start(UserName, DBPid),
    HomeDB_Pid = home_db:start(UserName, DBPid),
    MentionsDB_Pid = mentions_db:start(UserName, DBPid),
    FollowerDB_Pid = follower_db:start(UserName),
    Follow_DB_Pid = follow_db:start(UserName),

    IconDir = message_box_config:get(icon_dir),
    file:make_dir(IconDir),

    user_db:save_pid(User#user.id, self()),

    State = #user_state{user=User, 
                        message_db_pid=MessageDB_Pid,
                        home_db_pid=HomeDB_Pid,
                        follower_db_pid=FollowerDB_Pid,
                        follow_db_pid=Follow_DB_Pid,
                        mentions_db_pid=MentionsDB_Pid,
                        one_time_password_list=[]},
    loop(State).

stop(UserName) ->
    call(UserName, stop, []).

%%
%% @doc export functions
%%

update(UserName_OR_Id, AuthPassword, Mail, Password) ->
    call(UserName_OR_Id, update, [AuthPassword, Mail, Password]).    

%%
%% @doc send message to message_box.
%%
-spec(send_message(atom()|integer(), binary(), binary()) -> {ok, integer()}).

send_message(UserName_OR_Id, Password, Text) ->
    call(UserName_OR_Id, send_message, [Password, Text]).

get_message(UserName_OR_Id, MessageId) ->
    reference_call(UserName_OR_Id, get_message, [MessageId]).

get_sent_timeline(UserName_OR_Id, Count) ->
    reference_call(UserName_OR_Id, get_sent_timeline, [Count]).

get_home_timeline(UserName_OR_Id, Count) ->
    reference_call(UserName_OR_Id, get_home_timeline, [Count]).

get_mentions_timeline(UserName_OR_Id, Count) ->
    reference_call(UserName_OR_Id, get_mentions_timeline, [Count]).

save_to_home(UserName_OR_Id, MessageId) ->
    save_to_home(UserName_OR_Id, MessageId, {false, nil}).

save_to_home(UserName_OR_Id, MessageId, IsReplyText) ->
    call(UserName_OR_Id, save_to_home, [MessageId, IsReplyText]).    

follow(UserName_OR_Id, Password, UserId) ->
    call(UserName_OR_Id, follow, [Password, UserId]).

unfollow(UserName_OR_Id, Password, UserId) ->
    call(UserName_OR_Id, unfollow, [Password, UserId]).

add_follower(UserName_OR_Id, UserId) ->
    call(UserName_OR_Id, add_follower, [UserId]).        

delete_follower(UserName_OR_Id, UserId) ->
    call(UserName_OR_Id, delete_follower, [UserId]).        

get_follower_ids(UserName_OR_Id) ->
    reference_call(UserName_OR_Id, get_follower_ids, []).    

save_to_mentions(UserName_OR_Id, MessageId) ->
    call(UserName_OR_Id, save_to_mentions, [MessageId]).    

is_follow(UserName_OR_Id, UserId) ->
    reference_call(UserName_OR_Id, is_follow, [UserId]).

save_icon(UserName_OR_Id, Data, ContentType) when is_binary(Data) ->
    call(UserName_OR_Id, save_icon, [Data, ContentType]).

get_icon(UserName_OR_Id) ->
    reference_call(UserName_OR_Id, get_icon, []).

set_onetime_password(UserName_OR_Id, OneTimePassword) ->
    call(UserName_OR_Id, set_onetime_password, [OneTimePassword]).    

%%
%% @doc remote call functions.
%%

call(UserName_OR_Id, Name, Args) when is_list(UserName_OR_Id) ->
    call(list_to_atom(UserName_OR_Id), Name, Args);

call(UserName_OR_Id, Name, Args) ->
    case user_db:get_pid(UserName_OR_Id) of
	{ok, Pid} -> 
	    Pid ! {request, self(), Name, Args},
	    receive
		{Pid, reply, Result} -> Result
	    after 20000 -> {error, timeout}
	    end;
	Other -> Other
    end.

reference_call(UserName_OR_Id, Name, Args) when is_list(UserName_OR_Id) ->
    reference_call(list_to_atom(UserName_OR_Id), Name, Args);

reference_call(UserName_OR_Id, Name, Args)  ->
    case user_db:get_pid(UserName_OR_Id) of
	{ok, Pid} ->
	    Pid ! {ref_request, self(), Name, Args},
	    receive
		{Pid, reply, Result} -> Result
	    after 20000 -> {error, timeout}
	    end;
	Other -> Other
    end.

reply(To, Pid, Result) ->
    To ! {Pid, reply, Result}.

loop(State) ->
    Pid = self(),
    receive
	{request, From, stop, []} ->
	    reply(From, Pid, handle_stop(State));

	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  {Result, _} = handle_request(Name, [State | Args]),
			  reply(From, Pid, Result)
		  end),
	    loop(State);

	{request, From, Name, Args} ->
	    {Result, NewState} = handle_request(Name, [State | Args]),
	    reply(From, Pid, Result),
	    loop(NewState);

	{'EXIT', ExitPid, Reason} ->
	    UserManagerPid = whereis(user_manager),
	    case ExitPid of
		UserManagerPid ->
		    io:format("~p: user_manager is shutdown(Reason:~p).~n", 
			      [?MODULE, Reason]),
		    exit(Reason);
		_ ->
		    io:format("~p: process(~p) is shutdown(Reason:~p).~n", 
			      [?MODULE, ExitPid, Reason]),
		    exit(Reason)
	    end
    end.

handle_stop(State) ->
    message_db:stop(State#user_state.message_db_pid),
    home_db:stop(State#user_state.home_db_pid),
    follower_db:stop(State#user_state.follower_db_pid),
    follow_db:stop(State#user_state.follow_db_pid),
    mentions_db:stop(State#user_state.mentions_db_pid),
    {stop, self()}.

handle_request(set_onetime_password, [State, OneTimePassword]) ->
    OneTimePasswordList = State#user_state.one_time_password_list,
    NewList = add_one_time_password(OneTimePasswordList, 
                                    OneTimePassword),
    NewState = State#user_state{one_time_password_list=NewList},
    {ok, NewState};

handle_request(update, [State, AuthPassword, Mail, Password]) ->
    User = State#user_state.user,
    OneTimePasswordList = State#user_state.one_time_password_list,

    case util:authenticate(User, AuthPassword, OneTimePasswordList) of
	{ok, authenticated} ->
	    User0 = User#user{mail=Mail, pid=self()},
	    User1 = case Password of
			[] -> User0;
			_ ->
			    Md5Password = util:get_md5_password(User0, 
								Password),
			    User0#user{password=Md5Password}
		    end,

	    case user_db:update_user(User1) of
		{ok, User1} ->
                    NewState = State#user_state{user=User1},
		    {{ok, User}, NewState};
		Other ->
		    {Other, State}
	    end;
	Other -> {Other, State}
    end;

handle_request(latest_message, [State]) ->
    User = State#user_state.user,
    Result = message_db:get_latest_message(User#user.name),
    {Result, State};

handle_request(send_message, [State, Password, TextBin]) ->
    User = State#user_state.user,
    OneTimePasswordList = State#user_state.one_time_password_list,
    FollowerDB_Pid = State#user_state.follower_db_pid,
    HomeDB_Pid = State#user_state.home_db_pid,
    MessageDB_Pid = State#user_state.message_db_pid,

    case util:authenticate(User, Password, OneTimePasswordList) of
	{ok, authenticated} ->
	    case message_db:save_message(MessageDB_Pid, TextBin) of
		{ok, MessageId} ->
		    IsReplyTo = util:is_reply_text(TextBin),
		    send_to_followers(MessageId, FollowerDB_Pid, 
				      HomeDB_Pid, IsReplyTo),
		    ReplyToList = util:get_reply_list(TextBin),
		    send_to_replies(MessageId, ReplyToList),
		    {{ok, MessageId}, State};
		Other -> {Other, State}
	    end;
	Other -> {Other, State}
    end;

handle_request(get_sent_timeline, [State, Count]) ->
    MessageDB_Pid = State#user_state.message_db_pid,
    Result = message_db:get_sent_timeline(MessageDB_Pid, Count),
    {Result, State};

handle_request(get_home_timeline, [State, Count]) ->
    HomeDB_Pid = State#user_state.home_db_pid,
    Result = home_db:get_timeline(HomeDB_Pid, Count),
    {Result, State};

handle_request(get_mentions_timeline, [State, Count]) ->
    MentionsDB_Pid = State#user_state.mentions_db_pid,
    Result = mentions_db:get_timeline(MentionsDB_Pid, Count),
    {Result, State};

handle_request(save_to_home, [State, MessageId, IsReplyText]) ->
    HomeDB_Pid = State#user_state.home_db_pid,
    FollowDB_Pid = State#user_state.follow_db_pid,
    User = State#user_state.user,

    case IsReplyText of
	{true, To} ->
	    io:format("IsReplyText:~p", [IsReplyText]),
	    FromUserId = util:get_user_id_from_message_id(MessageId),

            case check_reply_receiver(FollowDB_Pid, 
                                      FromUserId, To#user.id, User) of
		true  -> 
                    Result = home_db:save_message_id(HomeDB_Pid, MessageId), 
                    {Result, State};
                false ->
                    {ok, State}
            end;               

	{false, nil} ->
	    {home_db:save_message_id(HomeDB_Pid, MessageId), State}
    end;                    

handle_request(follow, [State, Password, UserId]) ->
    User = State#user_state.user,
    FollowDB_Pid = State#user_state.follow_db_pid,
    OneTimePasswordList = State#user_state.one_time_password_list,

    case util:authenticate(User, Password, OneTimePasswordList) of
	{ok, authenticated} ->
	    case user_db:lookup_id(UserId) of
		{ok, FollowUser} ->
		    follow_db:save_follow_user(FollowDB_Pid, 
					       FollowUser#user.id),
		    Result = m_user:add_follower(FollowUser#user.id, 
						 User#user.id),
		    {Result, State};
		Other -> {Other, State}
	    end;
	Other -> {Other, State}
    end;

handle_request(unfollow, [State, Password, UserId]) ->
    User = State#user_state.user,
    FollowDB_Pid = State#user_state.follow_db_pid,
    OneTimePasswordList = State#user_state.one_time_password_list,

    case util:authenticate(User, Password, OneTimePasswordList) of
	{ok, authenticated} ->
	    case user_db:lookup_id(UserId) of
		{ok, FollowUser} ->
		    follow_db:delete_follow_user(FollowDB_Pid, 
						 FollowUser#user.id),
		    Result = m_user:delete_follower(FollowUser#user.id, 
						    User#user.id),
		    {Result, State};
		Other -> {Other, State}
	    end;
	Other -> {Other, State}
    end;

handle_request(add_follower, [State, UserId]) ->
    FollowerDB_Pid = State#user_state.follower_db_pid,

    case user_db:lookup_id(UserId) of
	{ok, _User} -> 
	    {follower_db:save_follower(FollowerDB_Pid, UserId), State};
	Other -> 
	    {Other, State}
    end;

handle_request(delete_follower, [State, UserId]) ->
    FollowerDB_Pid = State#user_state.follower_db_pid,
    case user_db:lookup_id(UserId) of
	{ok, _User} -> 
	    {follower_db:delete_follower(FollowerDB_Pid, UserId), State};
	Other -> 
	    {Other, State}
    end;

handle_request(get_follower_ids, [State]) ->
    FollowerDB_Pid = State#user_state.follower_db_pid,
    Result = follower_db:get_follower_ids(FollowerDB_Pid),
    {Result, State};

handle_request(get_message, [State, MessageId]) ->
    MessageDB_Pid = State#user_state.message_db_pid,
    Result = message_db:get_message(MessageDB_Pid, MessageId),
    {Result, State};

handle_request(save_to_mentions, [State, MessageId]) ->
    MentionsDB_Pid = State#user_state.mentions_db_pid,
    Result = mentions_db:save_message_id(MentionsDB_Pid, MessageId),
    {Result, State};    

handle_request(is_follow, [State, FollowId]) ->
    FollowDB_Pid = State#user_state.follow_db_pid,
    Result = follow_db:is_follow(FollowDB_Pid, FollowId),
    {Result, State};

handle_request(save_icon, [State, Data, ContentType]) ->
    User = State#user_state.user,
    delete_icon(User),
    BasePath = util:icon_path(User#user.name),

    Path = case ContentType of
	       "image/jpeg" -> BasePath ++ ".jpg";
	       "image/png" -> BasePath ++ ".png";
	       "image/gif" -> BasePath ++ ".gif";
	       _ -> {error, not_supported_content_type}
	   end,

    {file:write_file(Path, Data), State};

handle_request(get_icon, [State]) ->
    User = State#user_state.user,
    {read_icon(User), State}.

%%
%% @doc search content-type from file extention.
%%
search_content_type(Extention) ->
    search_content_type(Extention, ?CONTENT_TYPE_LIST).

search_content_type(_Extention, []) ->
    {error, not_supported_contet_type};

search_content_type(Extention, ContentTypeList) ->
    case ContentTypeList of
	[{Extention, ContentType} | _Tail] -> {ok, ContentType};
	[_ | Tail] -> search_content_type(Extention, Tail)
    end.

%%
%% @doc read user icon image file.
%%
read_icon(User) ->
    read_icon(User, ?FILE_EXTENTION_LIST).

read_icon(User, ExtList) ->
    case ExtList of
	[] -> {error, not_exist};
	[Ext | Tail] ->
	    BasePath = util:icon_path(User#user.name),
	    Path = BasePath ++ Ext,

	    case file:read_file(Path) of
		{ok, Binary}    ->
		    {ok, ContentType} = search_content_type(Ext),
		    {ok, Binary, ContentType};
		{error, enoent} -> read_icon(User, Tail)
	    end
    end.

%%
%% @doc delete user icon image file.
%%
delete_icon(User) ->
    delete_icon(User, ?FILE_EXTENTION_LIST).

delete_icon(User, ExtList) ->
    case ExtList of
	[] -> ok;
	[Ext | Tail] ->
	    BasePath = util:icon_path(User#user.name),
	    Path = BasePath ++ Ext,
	    file:delete(Path),
	    delete_icon(User, Tail)
    end.

%%
%% @doc send message to followers and saved to there's home_db.
%%
send_to_followers(MessageId, FollowerDB_Pid, HomeDB_Pid, IsReplyTo) ->
    Fun1 = fun(Follower) ->
		   m_user:save_to_home(Follower#follower.id, MessageId, 
				       IsReplyTo),
		   io:format("sent: ~p to ~p~n", 
			     [MessageId, Follower#follower.id])
	   end,
    Fun2 = fun(Follower) -> spawn(fun() -> Fun1(Follower) end) end,
    follower_db:map_do(FollowerDB_Pid, Fun2),
    home_db:save_message_id(HomeDB_Pid, MessageId).

%%
%% @doc send reply to destination user. 
%%    
send_to_replies(MessageId, ReplyToList) ->
    case ReplyToList of
	[] -> ok;
	[ReplyTo | Tail] ->
	    spawn(fun() ->
			  m_user:save_to_mentions(ReplyTo, MessageId),
			  send_to_replies(MessageId, Tail),
			  io:format("reply ~p to ~p~n", [MessageId, ReplyTo])
		  end)
    end.
	    
%%
%% @doc Add onetime password to State list. 
%%      and delete oldest password if list is too long.
%%
add_one_time_password(List, OneTimePassword) ->
    NewList = [OneTimePassword | List],
    ListLength = length(NewList),

    if ListLength > ?MaxSessionCount ->
	    [_Deleted | ResultList] = lists:reverse(NewList),
	    lists:reverse(ResultList);
       true ->
	    NewList
    end.

%%
%% @doc check reply receiver display home.
%%
check_reply_receiver(FollowDB_Pid, SenderId, ReceiverId, User) ->
    ThisUserId = User#user.id,

    case follow_db:is_follow(FollowDB_Pid, SenderId) of
        true ->
            case follow_db:is_follow(FollowDB_Pid, ReceiverId) of
                true -> true;
                _ ->
                    case ReceiverId  of
                        ThisUserId -> true;
                        _ -> false
                    end
            end;
        _  ->
            false
    end.
