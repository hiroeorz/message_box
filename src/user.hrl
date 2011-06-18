%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id,               %int()
	       status = enabled, %atom()
	       pid,              %atom()
	       name}).           %term()


-record(follower, {id,           %int()
		   datetime}).   %term()

-record(follow, {id,           %int()
		 datetime}).   %term()

