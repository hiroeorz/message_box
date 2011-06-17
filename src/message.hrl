%% File : message.hrl
%% Description : Include file for message_db

-record(message, {id,         %% int()
		  message_id, %% int()
		  text}).     %% string()

-record(message_index, {id,           %% int()
			message_id}). %% int()
