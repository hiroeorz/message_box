%% File : util.erl
%% Description : utilities used by other module

-module(util).
-include("user.hrl").
-include("message.hrl").
-export([get_user_from_message_id/1, get_user_id_from_message_id/1, 
	 formatted_number/2, formatted_number/3, get_timeline_ids/4,
	 get_reply_list/1, is_reply_text/1,
	 db_info/1, sleep/1]).

-define(SEPARATOR, "\s\n").

get_user_id_from_message_id(MessageId) ->
    IdStr = util:formatted_number(MessageId, ?MESSAGE_ID_LENGTH),
    {UserId, _Rest} = string:to_integer(string:substr(IdStr, 1, 
						      ?USER_ID_LENGTH)),
    UserId.

get_user_from_message_id(MessageId) ->
    IdStr = util:formatted_number(MessageId, ?MESSAGE_ID_LENGTH),
    {UserId, _Rest} = string:to_integer(string:substr(IdStr, 1, 
						      ?USER_ID_LENGTH)),
    user_db:lookup_id(UserId).

formatted_number(Num, Len)->
    formatted_number(Num, Len, "0").

formatted_number(Num, Len, EmptyChar)->
    Result = integer_to_list(Num),
    ResultLen = string:len(Result),
    if 
	ResultLen > Len -> Result;
	true -> add_string(before, Len, EmptyChar, Result)
    end.

add_string(before, Len, EmptyChar, Result)->
    case string:len(Result) of
	Len -> Result;
	_Other -> 
	    NewResult = string:concat(EmptyChar, Result),
	    add_string(before, Len, EmptyChar, NewResult)
    end.
	    
get_timeline_ids(Device, Count, Before, Result)->
    if
	length(Result) >= Count -> lists:reverse(Result);
	true -> case ets:next(Device, Before) of
		    '$end_of_table' -> lists:reverse(Result);
		    Id -> get_timeline_ids(Device, Count, Id, 
					   [Id | Result])
		end
    end.

%%
%% @doc create reply name list from tweet text.
%%
get_reply_list(Text) when is_list(Text) ->
    Tokens = string:tokens(Text, ?SEPARATOR),
    get_reply_list(Tokens, []).

get_reply_list([], List) -> lists:usort(List);

get_reply_list(Tokens, List) when is_list(Tokens) ->
    [Token | Tail] = Tokens,
    case string:sub_string(Token, 1, 1) of
	"@" ->
	    UserNameStr = string:sub_string(Token, 2, length(Token)),
	    get_reply_list(Tail, [list_to_atom(UserNameStr) | List]);
	_Other ->
	    get_reply_list(Tail, List)
    end.

is_reply_text(Text) ->
    case string:sub_string(Text, 1, 1) of
	"@" ->
	    [ToToken | _Tail] = string:tokens(Text, ?SEPARATOR),
	    case string:sub_string(ToToken, 2, length(ToToken)) of
		"" -> {false, nil};
		To -> {true, list_to_atom(To)}
	    end;
	_Other ->
	    {false, nil}
    end.

%%
%% @doc sqlite3 database file name
%%
db_info(UserName)->
    DiscName = list_to_atom(atom_to_list(UserName) ++ "_disk"),
    FileName = atom_to_list(UserName) ++ ".db",
    Path = ?DB_DIR ++ FileName,
    {DiscName, Path}.

%%
%% @doc sleep function
%%

sleep(Msec) when is_integer(Msec) ->
    receive
    after Msec -> ok
    end.

 
