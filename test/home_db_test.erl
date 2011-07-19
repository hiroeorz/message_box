%% File : message_db_test.erl
%% Description : Test for message_db

-module(home_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-include("../src/message_box.hrl").
-export([setup_server/0, get_message_id/2]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike, "mike@mail.co.jp", "aaa"),
		       user_db:add_user(tom, "tom@mail.co.jp", "aaa"),
		       home_db_test:setup_server()
	       end).
-define(Clearnup, fun({Pid, SQLite3Pid}) ->
			  home_db:stop(Pid),
			  sqlite3:close(SQLite3Pid),
			  user_db:stop(),
			  ?cmd("rm -r /tmp/test_db") 
		  end).

save_get_timeline_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,

	       MessageId1 = home_db_test:get_message_id(TomId, -1),
	       MessageId2 = home_db_test:get_message_id(TomId, -2),
	       MessageId3 = home_db_test:get_message_id(MikeId, -1),

	       ?assertEqual({ok, MessageId1}, 
			    home_db:save_message_id(Pid, MessageId1)),

	       ?assertEqual({ok, MessageId2}, 
			    home_db:save_message_id(Pid, MessageId2)),

	       ?assertEqual({ok, MessageId3}, 
			    home_db:save_message_id(Pid, MessageId3))
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
    register(db_server, home_db:start(mike, SQLitePid)),
    {whereis(db_server), SQLitePid}.

get_message_id(UserId, Id) ->
    FormattedUserId = util:formatted_number(UserId, 9),
    FormattedId = util:formatted_number(abs(Id), 9),
    list_to_integer(string:concat(FormattedUserId, FormattedId)).
