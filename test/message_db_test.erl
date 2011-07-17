%% File : message_db_test.erl
%% Description : Test for message_db

-module(message_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-export([setup_server/0]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike, "mike@mail.co.jp", "aaa"),
		       message_db_test:setup_server()
	       end).
-define(Clearnup, fun({Pid, SQLite3Pid}) ->
			  message_db:stop(Pid),
			  sqlite3:close(SQLite3Pid),
			  user_db:stop(),
			  ?cmd("rm -r /tmp/test_db") 
		  end).

save_get_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       fun() -> 
	       Text = "hello world",
	       Pid = whereis(messge_db_server),
	       {ok, SaveMessageId} = message_db:save_message(Pid, Text),
	       {ok, Message} = message_db:get_message(Pid, SaveMessageId),
	       ?assertEqual(-1, Message#message.id),
	       ?assertEqual(SaveMessageId, Message#message.message_id),
	       ?assertEqual(Text, Message#message.text),
	       ?assertMatch({{_, _, _}, {_, _, _}}, Message#message.datetime)
       end
      ]
     }
    }.

get_sent_timeline_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       fun() -> 
	       Text = "hello world ",
	       Pid = whereis(messge_db_server),	       
	       {ok, MessageId1} = message_db:save_message(Pid, Text ++ "1"),
	       {ok, MessageId2} = message_db:save_message(Pid, Text ++ "2"),
	       {ok, MessageId3} = message_db:save_message(Pid, Text ++ "3"),

	       [M3, M2, M1] = message_db:get_sent_timeline(Pid, 10),
	       ?assertEqual(-1, M1#message.id),
	       ?assertEqual(-2, M2#message.id),
	       ?assertEqual(-3, M3#message.id),
	       ?assertEqual(MessageId1, M1#message.message_id),
	       ?assertEqual(MessageId2, M2#message.message_id),
	       ?assertEqual(MessageId3, M3#message.message_id),
	       ?assertEqual(Text ++ "1", M1#message.text),
	       ?assertEqual(Text ++ "2", M2#message.text),
	       ?assertEqual(Text ++ "3", M3#message.text),
	       ?assertMatch({{_, _, _}, {_, _, _}}, M1#message.datetime),
	       ?assertMatch({{_, _, _}, {_, _, _}}, M2#message.datetime),
	       ?assertMatch({{_, _, _}, {_, _, _}}, M3#message.datetime)
       end
      ]
     }
    }.
    
get_latest_message_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       fun() -> 
	       Text = "hello world ",
	       Pid = whereis(messge_db_server),	       
	       {ok, _MessageId1} = message_db:save_message(Pid, Text ++ "1"),
	       {ok, _MessageId2} = message_db:save_message(Pid, Text ++ "2"),
	       {ok, MessageId3} = message_db:save_message(Pid, Text ++ "3"),

	       Message = message_db:get_latest_message(Pid),
	       ?assertEqual(-3, Message#message.id),
	       ?assertEqual(MessageId3, Message#message.message_id),
	       ?assertEqual(Text ++ "3", Message#message.text),
	       ?assertMatch({{_, _, _}, {_, _, _}}, Message#message.datetime)
       end
      ]
     }
    }.
    

%%
%% @doc helper functions
%%
setup_server() ->
    {ok, SQLitePid} = sqlite3:open(sqlite3_test_server,
				   [{file, "/tmp/test_db/test_message_db"}]),
    register(messge_db_server, message_db:start(mike, SQLitePid)),
    {whereis(messge_db_server), SQLitePid}.
