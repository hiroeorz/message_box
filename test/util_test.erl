%% File : util_test.erl
%% Description : Test for util

-module(util_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message_box.hrl").

-define(Setup, fun() -> crypto:start() end).
-define(Cleanup, fun(_) -> crypto:stop() end).

all_test_() ->
    {inorder,
     {setup, ?Setup, ?Cleanup,
      [

       { "MD5パスワードを生成する",
	 fun() ->
		 User = #user{id=1, name=user1, pid=undefined, status=true,
			      mail="user1@messabebox.com",
			      password=undefined},
		 RawPassword = "aabbccddeeffgg",
		 MD5Password = util:get_md5_password(User, RawPassword),
		 ?assertEqual(true, is_binary(MD5Password))
	 end
       },

       { "ユーザ認証",
	 fun() ->
		 User = #user{id=1, name=user1, pid=undefined, status=true,
			      mail="user1@messabebox.com",
			      password=undefined},
		 RawPassword = "aabbccddeeffgg",
		 FailurePassword = "aabbccddeeffg",
		 MD5Password = util:get_md5_password(User, RawPassword),
		 NewUser = User#user{password=MD5Password},

		 ?assertEqual({ok, authenticated}, 
			      util:authenticate(NewUser, RawPassword)),
		 ?assertEqual({error, unauthenticated}, 
			      util:authenticate(NewUser, "")),
		 ?assertEqual({error, unauthenticated}, 
			      util:authenticate(NewUser, FailurePassword))
	 end
       }

      ]
     }
    }.
