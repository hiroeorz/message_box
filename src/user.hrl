%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id,               %int()
	       status = enabled, %atom()
	       pid,              %atom()
	       name}).           %term()


-record(follow, {id,               %int()
		 datetime}).           %term()

