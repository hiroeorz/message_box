%% File : user_db_test.erl
%% Description : Test for user_db

-module(user_db_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/app_config.hrl").

-define(Setup1, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike, "mike@mail.co.jp", "aaa")
	       end).
-define(Setup2, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       user_db:start("/tmp/test_db/user_db"),
		       user_db:add_user(mike, "mike@mail.co.jp", "aaa"),
		       user_db:add_user(tom, "tom@mail.co.jp", "aaa"),
		       user_db:add_user(tanaka, "tanaka@mail.co.jp", "aaa")
	       end).

-define(RowPassword, "aaa").
-define(Clearnup, fun(_) -> user_db:stop(), ?cmd("rm -r /tmp/test_db") end).
-define(TestUser1, #user{id=1, status=true, pid=undefined, name=mike,
			 mail="mike@mail.co.jp", password=undefined}).

-define(TestUser2, #user{id=2, status=true, pid=undefined, name=tom,
			 mail="tom@mail.co.jp", password=undefined}).

-define(TestUser3, #user{id=3, status=true, pid=undefined, name=tanaka,
			 mail="tanaka@mail.co.jp", password=undefined}).

start_test_() ->
    {inorder,
     {setup, ?Setup1, ?Clearnup,
      [
       ?_assertEqual(pang, net_adm:ping(user_db))
      ]
     }
    }.

create_user_test_() ->
    {inorder,
     {setup, ?Setup1, ?Clearnup,
      [
       fun() ->
	       User1Template = ?TestUser1,
	       User2Template = ?TestUser2,
	       MD5Password1 = util:get_md5_password(User1Template, 
						    ?RowPassword),
	       MD5Password2 = util:get_md5_password(User2Template, 
						    ?RowPassword),
	       User1 = User1Template#user{password=MD5Password1},
	       User2 = User2Template#user{password=MD5Password2},
	       
	       ?assertEqual({error, already_exist}, 
			    user_db:add_user(mike, "mike@mail.co.jp", "aaa")),
	       
	       ?assertEqual({ok, User2},
			    user_db:add_user(tom, "tom@mail.co.jp", "aaa")),
	       
	       ?assertEqual({ok, User1}, user_db:lookup_id(1)),
	       ?assertEqual({ok, User2}, user_db:lookup_id(2)),
	       ?assertEqual({ok, User1}, user_db:lookup_name(mike)),
	       ?assertEqual({ok, User2}, user_db:lookup_name(tom))
       end
      ]
     }
    }.
    
delete_user_test_() ->
    {inorder,
     {setup, ?Setup1, ?Clearnup,
      [
       fun() ->
	       User1Template = ?TestUser1,
	       User2Template = ?TestUser2,
	       MD5Password1 = util:get_md5_password(User1Template, 
						    ?RowPassword),
	       MD5Password2 = util:get_md5_password(User2Template, 
						    ?RowPassword),
	       User1 = User1Template#user{password=MD5Password1},
	       User2 = User2Template#user{password=MD5Password2},
	       
	       ?assertEqual({error, already_exist}, 
			    user_db:add_user(mike, "mike@mail.co.jp", "aaa")),
	       
	       ?assertEqual({ok, User2}, 
			    user_db:add_user(tom, "tom@mail.co.jp", "aaa")),
	       
	       ?assertEqual({ok, User1}, user_db:lookup_id(1)),
	       ?assertEqual({ok, User2}, user_db:lookup_id(2)),
	       ?assertEqual(ok, user_db:delete_user(2)),
	       ?assertEqual({error, not_found}, user_db:lookup_id(2)),
	       ?assertEqual({ok, User1}, user_db:lookup_id(1))
       end
      ]
     }
    }.
    
update_user_test_() ->
    {inorder,
     {setup, ?Setup2, ?Clearnup,
      [
       fun() ->
	       User = ?TestUser1,
	       UpdatedUser = User#user{pid = self()},
	       ?assertEqual({ok, UpdatedUser}, user_db:update_user(UpdatedUser)),
	       {ok, GettedFromDB} = user_db:lookup_id(1),
	       ?assertEqual(self(), GettedFromDB#user.pid),
	       ?assertEqual({ok, UpdatedUser}, user_db:lookup_pid(self())),

	       UpdatedUser2 = User#user{id = -1, pid = self()},
	       ?assertEqual({error, not_found}, 
			    user_db:update_user(UpdatedUser2))
       end
      ]
     }
    }.
    
save_pid_get_pid_test_() ->
    {inorder,
     {setup, ?Setup2, ?Clearnup,
      [
       fun() ->
	       User = ?TestUser1,
	       MD5Password = util:get_md5_password(User, ?RowPassword),
	       UpdatedUser = User#user{pid = self(), password=MD5Password},
	       
	       ?assertEqual(ok, user_db:save_pid(?TestUser1#user.id, self())),
	       ?assertEqual({ok, UpdatedUser}, user_db:lookup_pid(self())),
	       ?assertEqual({ok, self()}, user_db:get_pid(mike)),
	       ?assertEqual({ok, self()}, user_db:get_pid("mike")),
	       ?assertEqual({ok, self()}, user_db:get_pid(1)),

	       ?assertEqual({error, not_found}, user_db:get_pid(-1)),
	       ?assertEqual({error, not_found}, 
			    user_db:get_pid(not_exit_user)),
	       ?assertEqual({error, not_found}, 
			    user_db:get_pid("not_exit_user")),
	       ?assertEqual({error, not_found}, 
			    user_db:save_pid(-1, self()))
       end
      ]
     }
    }.
    
map_do_test_() ->
    {inorder,
     {setup, ?Setup2, ?Clearnup,
      [
       fun() ->
	       ets:new(userTest, [named_table, public, {keypos, #user.id}]),

	       Fun = fun(User) ->
			     NewName = 
				 list_to_atom(atom_to_list(User#user.name) ++ 
						  "_updated"),
			     Updated = User#user{name = NewName},
			     ets:insert(userTest, Updated)
		     end,

	       user_db:map_do(Fun),

	       [NewTestUser1] = ets:lookup(userTest, 1),
	       [NewTestUser2] = ets:lookup(userTest, 2),
	       [NewTestUser3] = ets:lookup(userTest, 3),

	       ?assertEqual(mike_updated, NewTestUser1#user.name),
	       ?assertEqual(tom_updated, NewTestUser2#user.name),
	       ?assertEqual(tanaka_updated, NewTestUser3#user.name)
       end
      ]
     }
    }.
    
