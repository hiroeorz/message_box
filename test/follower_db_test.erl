%% File : message_db_test.erl
%% Description : Test for message_db

-module(follower_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-export([setup_server/0]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike),
		       user_db:add_user(tom),
		       follower_db_test:setup_server()
	       end).
-define(Clearnup, fun({Pid, SQLite3Pid}) ->
			  follower_db:stop(Pid),
			  sqlite3:close(SQLite3Pid),
			  user_db:stop(),
			  ?cmd("rm -r /tmp/test_db") 
		  end).

save_get_ids_test_() ->
    {spawn,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(follower_db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,
	       ?assertEqual(ok, follower_db:save_follower(Pid, TomId)),
	       ?assertEqual([TomId], follower_db:get_follower_ids(Pid)),
	       ?assertEqual(ok, follower_db:save_follower(Pid, MikeId)),
	       ?assertEqual([TomId, MikeId], follower_db:get_follower_ids(Pid))
       end
      ]
     }
    }.

map_do_test_() ->
    {spawn,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(follower_db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,

	       follower_db:save_follower(Pid, TomId),
	       follower_db:save_follower(Pid, MikeId),

	       TestPid = self(),

	       follower_db:map_do(Pid,
				fun(Follower) ->
					TestPid ! {test, Follower#follower.id}
				end),

	       TestPid ! {ok, finish},
	       Results = collect_results([]),
	       SortedIds = lists:sort([MikeId, TomId]),
	       ?assertEqual(SortedIds, Results)
       end
      ]
     }
    }.

%%
%% @doc helper functions
%%

collect_results(Results) ->
    receive
	{test, SomeId} ->
	    collect_results([SomeId | Results]);
	{ok, finish} ->
	    lists:sort(Results)
    end.    

setup_server() ->
    {ok, SQLitePid} = sqlite3:open(sqlite3_test_server,
				   [{file, "/tmp/test_db/test_message_db"}]),
    register(follower_db_server, follower_db:start(mike)),
    {whereis(follower_db_server), SQLitePid}.
