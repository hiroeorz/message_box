%% File: user.erl
%% Description : user handler module.

-module(m_user).
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([send_message/3, get_message/2, get_sent_timeline/2, 
	 get_home_timeline/2, save_to_home/2, save_to_home/3,
	 follow/3, unfollow/3, add_follower/2, delete_follower/2,
	 get_follower_ids/1, is_follow/2,
	 save_to_mentions/2, get_mentions_timeline/2]).

-define(USER_MANAGER, user_manager).

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

    user_db:save_pid(User#user.id, self()),
    loop({User, MessageDB_Pid, HomeDB_Pid, FollowerDB_Pid, Follow_DB_Pid,
	  MentionsDB_Pid}).

stop(UserName) ->
    call(UserName, stop, []).

%%
%% @doc export functions
%%

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

%%
%% @doc remote call functions.
%%

call(UserName_OR_Id, Name, Args)  ->
    case user_db:get_pid(UserName_OR_Id) of
	{ok, Pid} -> 
	    Pid ! {request, self(), Name, Args},
	    receive
		{Pid, reply, Result} -> Result
	    after 20000 -> {error, timeout}
	    end;
	Other -> Other
    end.

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
			  Result = handle_request(Name, [State | Args]),
			  reply(From, Pid, Result)
		  end),
	    loop(State);

	{request, From, Name, Args} ->
	    Result = handle_request(Name, [State | Args]),
	    reply(From, Pid, Result),
	    loop(State);

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

handle_stop({_, MessageDB_Pid, HomeDB_Pid, FollowerDB_Pid, FollowDB_Pid, 
	    MentionsDB_Pid}) ->
    message_db:stop(MessageDB_Pid),
    home_db:stop(HomeDB_Pid),
    follower_db:stop(FollowerDB_Pid),
    follow_db:stop(FollowDB_Pid),
    mentions_db:stop(MentionsDB_Pid),
    {stop, self()}.

handle_request(latest_message, [{User, _}]) ->
    message_db:get_latest_message(User#user.name);

handle_request(send_message, 
	       [{User, MessageDB_Pid, HomeDB_Pid, FollowerDB_Pid, _, _}, 
		Password, Text]) ->
    case util:authenticate(User, Password) of
	{ok, authenticated} ->
	    case message_db:save_message(MessageDB_Pid, Text) of
		{ok, MessageId} ->
		    IsReplyTo = util:is_reply_text(Text),
		    send_to_followers(MessageId, FollowerDB_Pid, 
				      HomeDB_Pid, IsReplyTo),
		    ReplyToList = util:get_reply_list(Text),
		    send_to_replies(MessageId, ReplyToList),
		    {ok, MessageId};
		Other -> Other
	    end;
	Other -> Other
    end;

handle_request(get_sent_timeline, [{_, MessageDB_Pid, _, _, _, _}, Count]) ->
    message_db:get_sent_timeline(MessageDB_Pid, Count);

handle_request(get_home_timeline, [{_, _, HomeDB_Pid, _, _, _}, Count]) ->
    home_db:get_timeline(HomeDB_Pid, Count);

handle_request(get_mentions_timeline, 
	       [{_, _, _, _, _, MentionsDB_Pid}, Count]) ->
    mentions_db:get_timeline(MentionsDB_Pid, Count);

handle_request(save_to_home, [{_, _, HomeDB_Pid, _, FollowDB_Pid, _}, 
			      MessageId, IsReplyText]) ->
    case IsReplyText of
	{true, _To} ->
	    io:format("IsReplyText:~p", [IsReplyText]),
	    FromUserId = util:get_user_id_from_message_id(MessageId),
	    case follow_db:is_follow(FollowDB_Pid, FromUserId) of
		true  -> home_db:save_message_id(HomeDB_Pid, MessageId);
		false -> ok
	    end;
	{false, nil} ->
	    home_db:save_message_id(HomeDB_Pid, MessageId)
    end;

handle_request(follow, [{User, _, _, _, FollowDB_Pid, _}, Password, UserId]) ->
    case util:authenticate(User, Password) of
	{ok, authenticated} ->
	    case user_db:lookup_id(UserId) of
		{ok, FollowUser} ->
		    follow_db:save_follow_user(FollowDB_Pid, 
					       FollowUser#user.id),
		    m_user:add_follower(FollowUser#user.id, User#user.id);
		Other -> Other
	    end;
	Other -> Other
    end;

handle_request(unfollow, 
	       [{User, _, _, _, FollowDB_Pid, _}, Password, UserId]) ->
    case util:authenticate(User, Password) of
	{ok, authenticated} ->
	    case user_db:lookup_id(UserId) of
		{ok, FollowUser} ->
		    follow_db:delete_follow_user(FollowDB_Pid, 
						 FollowUser#user.id),
		    m_user:delete_follower(FollowUser#user.id, User#user.id);
		Other -> Other
	    end;
	Other -> Other
    end;

handle_request(add_follower, [{_, _, _, FollowerDB_Pid, _, _}, UserId]) ->
    case user_db:lookup_id(UserId) of
	{ok, _User} -> follower_db:save_follower(FollowerDB_Pid, UserId);
	Other -> Other
    end;

handle_request(delete_follower, [{_, _, _, FollowerDB_Pid, _, _}, UserId]) ->
    case user_db:lookup_id(UserId) of
	{ok, _User} -> follower_db:delete_follower(FollowerDB_Pid, UserId);
	Other -> Other
    end;

handle_request(get_follower_ids, [{_, _, _, FollowerDB_Pid, _, _}]) ->
    follower_db:get_follower_ids(FollowerDB_Pid);

handle_request(get_message, [{_, MessageDB_Pid, _, _, _, _}, MessageId]) ->
    message_db:get_message(MessageDB_Pid, MessageId);

handle_request(save_to_mentions, 
	       [{_, _, _, _, _, MentionsDB_Pid}, MessageId]) ->
    mentions_db:save_message_id(MentionsDB_Pid, MessageId);

handle_request(is_follow, [{_, _, _, _, FollowDB_Pid, _}, FollowId]) ->
    follow_db:is_follow(FollowDB_Pid, FollowId).

%%
%% @doc local functions.
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
	    
