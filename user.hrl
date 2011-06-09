%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id,               %int()
	       status = enabled, %atom()
	       name}).           %term()
