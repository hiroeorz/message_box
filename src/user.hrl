%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id,               %int()
	       status = true,    %atom()
	       pid,              %atom()
	       name,             %term()
	       mail,             %string
	       password          %string
	      }).           


-record(follower, {id,           %int()
		   datetime}).   %term()

-record(follow, {id,           %int()
		 datetime}).   %term()

