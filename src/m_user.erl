%% File: user.erl
%% Description : user handler module.

-module(m_user).
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([send_message/2, get_message/2, get_sent_timeline/2]).

-define(USER_MANAGER, user_manager).

start(UserName) ->
    register(process_name(UserName), spawn_link(?MODULE, init, [UserName])).

init(UserName) ->
    process_flag(trap_exit, true),
    ManagerPid = whereis(?USER_MANAGER),
    link(ManagerPid),

    {ok, User} = user_db:lookup_name(UserName),
    MessageDB_Pid = message_db:start(UserName),
    loop({User, MessageDB_Pid}).

stop(UserName) ->
    call(UserName, stop, []).

%%
%% @doc export functions
%%

send_message(UserName, Text) ->
    call(UserName, send_message, [Text]).

get_message(UserName, MessageId) ->
    reference_call(UserName, get_message, [MessageId]).

get_sent_timeline(UserName, Count) ->
    reference_call(UserName, get_sent_timeline, [Count]).

%%
%% @doc remote call functions.
%%

call(UserName, Name, Args) ->
    Pid = whereis(process_name(UserName)),
    Pid ! {request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reference_call(UserName, Name, Args) ->
    Pid = whereis(process_name(UserName)),
    Pid ! {ref_request, self(), Name, Args},
    receive
	{Pid, reply, Result} -> Result
    after 20000 -> {error, timeout}
    end.

reply(To, Pid, Result) ->
    To ! {Pid, reply, Result}.

loop({User, MessageDB_Pid}=State) ->
    Pid = self(),
    receive
	{request, From, stop, []} ->
	    reply(From, Pid, handle_stop({User, MessageDB_Pid}));

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

	{'EXIT', ExitPid, _Reason} ->
	    io:format("~p: manager process(~p) is shutdown.~n", 
		      [?MODULE, ExitPid])
    end.

handle_stop({_, MessageDB_Pid}) ->
    message_db:stop(MessageDB_Pid),
    {stop, self()}.

handle_request(latest_message, [{User, _}]) ->
    message_db:get_latest_message(User#user.name);

handle_request(send_message, [{_, MessageDB_Pid}, Text]) ->
    message_db:save_message(MessageDB_Pid, Text);

handle_request(get_sent_timeline, [{_, MessageDB_Pid}, Count]) ->
    message_db:get_sent_timeline(MessageDB_Pid, Count);

handle_request(get_message, [{_, MessageDB_Pid}, MessageId]) ->
    message_db:get_message(MessageDB_Pid, MessageId).

%%
%% @doc local functions.
%%

process_name(UserName) -> 
    list_to_atom(string:concat("m_user_", atom_to_list(UserName))).

