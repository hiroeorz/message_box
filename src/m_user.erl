%% File: user.erl
%% Description : user handler module.

-module(m_user).
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([send_message/2, get_message/2, get_sent_timeline/2, 
	 get_home_timeline/2, save_to_home/2, 
	 follow/2, add_follower/2, get_follower_ids/1]).

-define(USER_MANAGER, user_manager).

start(UserName) ->
    spawn_link(?MODULE, init, [UserName]).

init(UserName) ->
    process_flag(trap_exit, true),
    ManagerPid = whereis(?USER_MANAGER),
    link(ManagerPid),

    {ok, User} = user_db:lookup_name(UserName),
    MessageDB_Pid = message_db:start(UserName),
    HomeDB_Pid = home_db:start(UserName),
    FollowDB_Pid = follow_db:start(UserName),

    user_db:save_pid(User#user.id, self()),
    loop({User, MessageDB_Pid, HomeDB_Pid, FollowDB_Pid}).

stop(UserName) ->
    call(UserName, stop, []).

%%
%% @doc export functions
%%

send_message(UserName_OR_Id, Text) ->
    call(UserName_OR_Id, send_message, [Text]).

get_message(UserName_OR_Id, MessageId) ->
    reference_call(UserName_OR_Id, get_message, [MessageId]).

get_sent_timeline(UserName_OR_Id, Count) ->
    reference_call(UserName_OR_Id, get_sent_timeline, [Count]).

get_home_timeline(UserName_OR_Id, Count) ->
    reference_call(UserName_OR_Id, get_home_timeline, [Count]).

save_to_home(UserName_OR_Id, MessageId) ->
    call(UserName_OR_Id, save_to_home, [MessageId]).    

follow(UserName_OR_Id, UserId) ->
    call(UserName_OR_Id, follow, [UserId]).

add_follower(UserName_OR_Id, UserId) ->
    call(UserName_OR_Id, add_follower, [UserId]).        

get_follower_ids(UserName_OR_Id) ->
    reference_call(UserName_OR_Id, get_follower_ids, []).    

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

loop({_User, MessageDB_Pid, HomeDB_Pid, FollowDB_Pid}=State) ->
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

	{'EXIT', MessageDB_Pid, Reason} ->
	    io:format("~p: message_db process(~p) is shutdown. reason:~p~n", 
		      [?MODULE, MessageDB_Pid, Reason]);

	{'EXIT', HomeDB_Pid, Reason} ->
	    io:format("~p: home_db process(~p) is shutdown. reason:~p~n", 
		      [?MODULE, HomeDB_Pid, Reason]);

	{'EXIT', FollowDB_Pid, Reason} ->
	    io:format("~p: follow_db process(~p) is shutdown. reason:~p~n", 
		      [?MODULE, FollowDB_Pid, Reason]);

	{'EXIT', ExitPid, _Reason} ->
	    io:format("~p: manager process(~p) is shutdown.~n", 
		      [?MODULE, ExitPid])
    end.

handle_stop({_, MessageDB_Pid, HomeDB_Pid, FollowDB_Pid}) ->
    message_db:stop(MessageDB_Pid),
    home_db:stop(HomeDB_Pid),
    follow_db:stop(FollowDB_Pid),
    {stop, self()}.

handle_request(latest_message, [{User, _}]) ->
    message_db:get_latest_message(User#user.name);

handle_request(send_message, [{_, MessageDB_Pid, _, FollowDB_Pid}, Text]) ->
    case message_db:save_message(MessageDB_Pid, Text) of
	{ok, MessageId} ->
	    Fun1 = fun(Follow) ->
			   m_user:save_to_home(Follow#follow.id, MessageId), 
			   io:format("sent: ~p to ~p~n", 
				     [MessageId, Follow#follow.id])
		  end,
	    Fun2 = fun(Follow) -> spawn(fun() -> Fun1(Follow) end) end,
	    follow_db:map_do(FollowDB_Pid, Fun2);
	Other -> Other
    end;

handle_request(get_sent_timeline, [{_, MessageDB_Pid, _, _}, Count]) ->
    message_db:get_sent_timeline(MessageDB_Pid, Count);

handle_request(get_home_timeline, [{_, _, HomeDB_Pid, _}, Count]) ->
    home_db:get_timeline(HomeDB_Pid, Count);

handle_request(save_to_home, [{_, _, HomeDB_Pid, _}, MessageId]) ->
    home_db:save_message_id(HomeDB_Pid, MessageId);

handle_request(follow, [{User, _, _, _}, UserId]) ->
    case user_db:lookup_id(UserId) of
	{ok, FollowUser} ->
	    m_user:add_follower(FollowUser#user.id, User#user.id);
	Other -> Other
    end;

handle_request(add_follower, [{_, _, _, FollowDB_Pid}, UserId]) ->
    case user_db:lookup_id(UserId) of
	{ok, _User} -> follow_db:save_follower(FollowDB_Pid, UserId);
	Other -> Other
    end;

handle_request(get_follower_ids, [{_, _, _, FollowDB_Pid}]) ->
    follow_db:get_follower_ids(FollowDB_Pid);

handle_request(get_message, [{_, MessageDB_Pid}, MessageId]) ->
    message_db:get_message(MessageDB_Pid, MessageId).

%%
%% @doc local functions.
%%
