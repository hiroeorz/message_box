%% File : message_db_test.erl
%% Description : Test for message_db

-module(follow_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-export([setup_server/0]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike),
		       user_db:add_user(tom),
		       follow_db_test:setup_server()
	       end).
-define(Clearnup, fun({Pid, SQLite3Pid}) ->
			  follow_db:stop(Pid),
			  sqlite3:close(SQLite3Pid),
			  user_db:stop(),
			  ?cmd("rm -r /tmp/test_db") 
		  end).

save_get_test_() ->
    {spawn,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(follow_db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,
	       ?assertEqual(ok, follow_db:save_follow_user(Pid, TomId)),
	       ?assertEqual(true, follow_db:is_follow(Pid, TomId)),
	       ?assertEqual(false, follow_db:is_follow(Pid, MikeId))
       end
      ]
     }
    }.

get_ids_test_() ->
    {spawn,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(follow_db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,

	       follow_db:save_follow_user(Pid, TomId),
	       ?assertEqual([TomId], follow_db:get_follow_ids(Pid)),

	       follow_db:save_follow_user(Pid, MikeId),
	       ?assertEqual([TomId, MikeId], follow_db:get_follow_ids(Pid))
       end
      ]
     }
    }.

map_do_test_() ->
    {spawn,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       Pid = whereis(follow_db_server),
	       {ok, TomUser} = user_db:lookup_name(tom),
	       {ok, MikeUser} = user_db:lookup_name(mike),
	       TomId = TomUser#user.id,
	       MikeId = MikeUser#user.id,

	       follow_db:save_follow_user(Pid, TomId),
	       follow_db:save_follow_user(Pid, MikeId),

	       TestPid = self(),

	       follow_db:map_do(Pid,
				fun(Follow) ->
					TestPid ! {test, Follow#follow.id}
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
    register(follow_db_server, follow_db:start(mike)),
    {whereis(follow_db_server), SQLitePid}.
