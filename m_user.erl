%% File: user.erl
%% Description : user handler module.

-module(m_user).
-include("user.hrl").
-export([init/1]).
-export([start/1, stop/1]).
-export([send_message/2, get_message/2]).

start(UserName)->
    register(process_name(UserName), spawn(?MODULE, init, [UserName])).

init(UserName)->
    User = user_db:lookup_name(UserName),
    MessageDB_Pid = message_db:start(UserName),
    loop({User, MessageDB_Pid}).

stop(UserName)->
    call(UserName, stop, []).

%%
%% @doc export functions
%%

send_message(UserName, Text)->
    call(UserName, send_message, [Text]).

get_message(UserName, MessageId)->
    reference_call(UserName, get_message, [MessageId]).

%%
%% @doc remote call functions.
%%

call(UserName, Name, Args)->
    process_name(UserName) ! {request, self(), Name, Args},
    receive
	{reply, Result} -> Result
    end.

reference_call(UserName, Name, Args)->
    process_name(UserName) ! {ref_request, self(), Name, Args},
    receive
	{reply, Result} -> Result
    end.

reply(To, Result)->
    To ! {reply, Result}.

loop({User, MessageDB_Pid}=State) ->
    receive
	{request, From, stop, []} ->
	    reply(From, handle_stop({User, MessageDB_Pid}));

	{ref_request, From, Name, Args} ->
	    spawn(fun()->
			  Result = handle_request(Name, [State | Args]),
			  reply(From, Result)
		  end),
	    loop(State);

	{request, From, Name, Args} ->
	    Result = handle_request(Name, [State | Args]),
	    reply(From, Result),
	    loop(State)
    end.

handle_stop({_, MessageDB_Pid})->
    message_db:stop(MessageDB_Pid),
    {stop, self()}.

handle_request(latest_message, [{User, _}])->
    message_db:get_latest_message(User#user.name);

handle_request(send_message, [{_User, MessageDB_Pid}, Text])->
    message_db:save_message(MessageDB_Pid, Text);

handle_request(get_message, [{_User, MessageDB_Pid}, MessageId]) ->
    message_db:get_message(MessageDB_Pid, MessageId).

%%
%% @doc local functions.
%%

process_name(UserName)-> 
    list_to_atom(string:concat("m_user_", atom_to_list(UserName))).

