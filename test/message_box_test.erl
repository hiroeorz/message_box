%% File : message_db_test.erl
%% Description : Test for message_db

-module(message_box_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/user.hrl").
-include("../src/message.hrl").
-export([wait/0]).

-define(Setup, fun() -> 
		       ?cmd("mkdir -p /tmp/test_db"), 
		       message_box:start("/tmp/test_db/user_db")
	       end).

-define(Clearnup, fun(_) ->
			  util:sleep(1000),
			  message_box:stop(), 
			  ?cmd("rm -r /tmp/test_db") 
		  end).

all_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       fun() ->
	       % ユーザを登録
	       message_box_test:wait(),
	       ?assertMatch({ok, _AssignedUser}, 
			    message_box:create_user(shin)),
	       ?assertMatch({ok, _AssignedUser}, 
			    message_box:create_user(user1)),
	       ?assertMatch({ok, _AssignedUser}, 
			    message_box:create_user(user2)),

	       % メッセージをポストする
	       message_box_test:wait(),
	       {ok, ShinUser} = user_db:lookup_name(shin),
	       ShinId = ShinUser#user.id,
	       Message1 = "hello world",
	       {ok, SavedMessageId_0} = message_box:send_message(ShinId,  
								 Message1),

	       % 送信したメッセージを取得して内容を照合する
	       {ok, SavedMessage_0} = message_box:get_message(SavedMessageId_0),
	       ?assertEqual(Message1, SavedMessage_0#message.text),

	       % この後のテストのために情報を変数に格納
	       message_box_test:wait(),
	       {ok, User1} = user_db:lookup_name(user1),
	       {ok, User2} = user_db:lookup_name(user2),
	       User1Id = User1#user.id,
	       User2Id = User2#user.id,

	       % 他ユーザをフォローし、フォローした事を確認
	       message_box:follow(ShinId, User1Id),
	       ?assertEqual(true, message_box:is_follow(ShinId, User1Id)),
	       ?assertEqual(false, message_box:is_follow(ShinId, User2Id)),

	       % フォロワーが居ない状態では自分のメッセージは自分のHOMEのみにある
	       message_box_test:wait(),
	       Message2 = "hello everyone!! :)",
	       {ok, _Message2Id} = message_box:send_message(ShinId, Message2),
	       message_box_test:wait(),

	       ShinHome_0 = message_box:get_home_timeline(ShinId, 10),
	       ?assertEqual(2, length(ShinHome_0)),
	       User1Home_0 = message_box:get_home_timeline(User1Id, 10),
	       ?assertEqual(0, length(User1Home_0)),
	       User2Home_0 = message_box:get_home_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Home_0)),

	       % 内容を確認
	       Latest_00 = lists:nth(1, ShinHome_0),
	       ?assertEqual(Message2, Latest_00#message.text),

	       % フォロワーがいるユーザのポストはフォロワーのホームにも出現
	       Message3 = "I am tired :<",
	       message_box:send_message(User1Id,  Message3),
	       message_box_test:wait(),

	       ShinHome_1 = message_box:get_home_timeline(ShinId, 10),
	       ?assertEqual(3, length(ShinHome_1)),
	       User1Home_1 = message_box:get_home_timeline(User1Id, 10),
	       ?assertEqual(1, length(User1Home_1)),
	       User2Home_1 = message_box:get_home_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Home_1)),

	       % 内容を確認
	       Latest_01 = lists:nth(1, ShinHome_1),
	       ?assertEqual(Message3, Latest_01#message.text),
	       ?assertEqual(lists:nth(1, ShinHome_1), 
			    lists:nth(1, User1Home_1)),

	       % この時点で全員のメンションが空であることを確認
	       ShinMentions_0 = message_box:get_mentions_timeline(ShinId, 10),
	       ?assertEqual(0, length(ShinMentions_0)),
	       User1Mention_0 = message_box:get_mentions_timeline(User1Id, 10),
	       ?assertEqual(0, length(User1Mention_0)),
	       User2Mention_0 = message_box:get_mentions_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Mention_0)),

	       % User1からShinへリプライを送信する
	       Message4 = "@shin Thank you for your follow!! :-)",
	       message_box:send_message(User1Id,  Message4),
	       message_box_test:wait(),

	       % Shinがリプライを受信した
	       ShinMentions_1 = message_box:get_mentions_timeline(ShinId, 10),
	       ?assertEqual(1, length(ShinMentions_1)),
	       User1Mention_1 = message_box:get_mentions_timeline(User1Id, 10),
	       ?assertEqual(0, length(User1Mention_1)),
	       User2Mention_1 = message_box:get_mentions_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Mention_1)),

	       % リプライは送り主と受け取り主のホームに出現する
	       ShinHome_2 = message_box:get_home_timeline(ShinId, 10),
	       ?assertEqual(4, length(ShinHome_2)),
	       User1Home_2 = message_box:get_home_timeline(User1Id, 10),
	       ?assertEqual(2, length(User1Home_2)),
	       User2Home_2 = message_box:get_home_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Home_2)),

	       % 内容を確認
	       Latest_02 = lists:nth(1, ShinMentions_1),
	       ?assertEqual(Message4, Latest_02#message.text),
	       ?assertEqual(lists:nth(1, ShinMentions_1), 
			    lists:nth(1, ShinHome_2)),
	       ?assertEqual(lists:nth(1, ShinMentions_1), 
			    lists:nth(1, User1Home_2)),

	       % ShinがUser2をフォローする
	       message_box:follow(ShinId, User2Id),
	       
	       % User1がUser2にリプライを送信する
	       Message5 = "@user2 hello! user2 :-)",
	       message_box:send_message(User1Id,  Message5),
	       message_box_test:wait(),

	       % User2がリプライを受信した
	       ShinMentions_2 = message_box:get_mentions_timeline(ShinId, 10),
	       ?assertEqual(1, length(ShinMentions_2)),
	       User1Mention_2 = message_box:get_mentions_timeline(User1Id, 10),
	       ?assertEqual(0, length(User1Mention_2)),
	       User2Mention_2 = message_box:get_mentions_timeline(User2Id, 10),
	       ?assertEqual(1, length(User2Mention_2)),

	       % リプライは両者をフォローしているユーザのホームにも出現する
	       ShinHome_3 = message_box:get_home_timeline(ShinId, 10),
	       ?assertEqual(5, length(ShinHome_3)),
	       User1Home_3 = message_box:get_home_timeline(User1Id, 10),
	       ?assertEqual(3, length(User1Home_3)),
	       User2Home_3 = message_box:get_home_timeline(User2Id, 10),
	       ?assertEqual(0, length(User2Home_3)),

	       % 内容を確認
	       Latest_03 = lists:nth(1, ShinHome_3),
	       ?assertEqual(Message5, Latest_03#message.text),
	       ?assertEqual(lists:nth(1, ShinHome_3), 
			    lists:nth(1, User1Home_3)),
	       ?assertEqual(lists:nth(1, ShinHome_3), 
			    lists:nth(1, User2Mention_2))
       end
       
      ]
     }
    }.

wait() ->
    util:sleep(300).
    
