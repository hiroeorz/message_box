%% File : message.hrl
%% Description : Include file for message_db

-define(DatetimeFormat, 
	"~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").

-record(message, {id,         %% int()
		  message_id, %% int()
		  text,       %% string()
		  datetime}). %%      

-record(message_index, {id,           %% int()
			message_id}). %% int()
