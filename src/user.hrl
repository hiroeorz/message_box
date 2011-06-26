%% File : usr.hrl
%% Description : Include file for user_db

-define(MESSAGE_ID_LENGTH, 18).
-define(USER_ID_LENGTH, 9).
-define(DB_DIR, "./db/").
-define(USER_DB_FILENAME, "./db/user_db").

-record(user, {id,               %int()
	       status = true,    %atom()
	       pid,              %atom()
	       name}).           %term()


-record(follower, {id,           %int()
		   datetime}).   %term()

-record(follow, {id,           %int()
		 datetime}).   %term()

