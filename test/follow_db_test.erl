%% File : message_db_test.erl
%% Description : Test for message_db

-module(follow_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-include("../src/app_config.hrl").
-export([setup_server/0]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike, "mike@mail.co.jp", "aaa"),
		       user_db:add_user(tom, "tom@mail.co.jp", "aaa"),
		       follow_db_test:setup_server()
	       end).
-define(Clearnup, fun({Pid, SQLite3Pid}) ->
			  follow_db:stop(Pid),
			  sqlite3:close(SQLite3Pid),
			  user_db:stop(),
			  ?cmd("rm -r /tmp/test_db") 
		  end).

all_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       { "フォローしたユーザを保存する",
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
       },

       { "フォローしているユーザをフォロー対象から外す",
	 fun() ->
		 Pid = whereis(follow_db_server),
		 {ok, TomUser} = user_db:lookup_name(tom),
		 {ok, MikeUser} = user_db:lookup_name(mike),
		 TomId = TomUser#user.id,
		 MikeId = MikeUser#user.id,

		 ?assertEqual({ok, deleted}, 
			      follow_db:delete_follow_user(Pid, TomId)),

		 ?assertEqual({error, not_following}, 
			      follow_db:delete_follow_user(Pid, MikeId)),

		 ?assertEqual(false, follow_db:is_follow(Pid, TomId)),
		 ?assertEqual(false, follow_db:is_follow(Pid, MikeId))
	 end
       },

       { "フォローユーザのIDのリストを取得する",
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
       },

       { "全てのフォロワーに特定の処理を実行する",
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
       }
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
