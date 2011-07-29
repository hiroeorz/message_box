%% File : util.erl
%% Description : utilities used by other module

-module(util).
-include_lib("eunit/include/eunit.hrl").
-include("message_box.hrl").
-include("user.hrl").
-include("message.hrl").
-export([get_user_from_message_id/1, get_user_id_from_message_id/1, 
	 formatted_number/2, formatted_number/3, get_timeline_ids/4,
	 get_reply_list/1, is_reply_text/1,
	 db_info/1, sleep/1, icon_path/1,
	 get_md5_password/2, get_onetime_password/2, 
	 authenticate/2, authenticate/3,
         shurink_ets/2]).

-define(SEPARATOR, "\s\n").
-define(MD5_KEY1, "message_box").
-define(MD5_KEY2, "garigarikunnashiaji").
-define(MD5_KEY3, "goronekogorousan").

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
    DB_DIR = message_box_config:get(database_dir),
    Path = DB_DIR ++ FileName,
    {DiscName, Path}.

%%
%% @doc sleep function
%%

sleep(Msec) when is_integer(Msec) ->
    receive
    after Msec -> ok
    end.

%%
%% @doc create md5 password
%%

get_md5_password(User, RawPassword) when is_list(RawPassword) ->
    Context =  crypto:md5_init(), 
    NewContext0 = crypto:md5_update(Context, RawPassword),
    NewContext1 = crypto:md5_update(NewContext0, atom_to_list(User#user.name)),
    NewContext2 = crypto:md5_update(NewContext1, User#user.mail),
    NewContext3 = crypto:md5_update(NewContext2, ?MD5_KEY1),
    NewContext4 = crypto:md5_update(NewContext3, ?MD5_KEY2),
    NewContext5 = crypto:md5_update(NewContext4, ?MD5_KEY3),
    crypto:md5_final(NewContext5).
    
get_onetime_password(User, RawPassword) when is_list(RawPassword) ->
    Context =  crypto:md5_init(), 
    NewContext0 = crypto:md5_update(Context, RawPassword),
    NewContext1 = crypto:md5_update(NewContext0, atom_to_list(User#user.name)),
    NewContext2 = crypto:md5_update(NewContext1, User#user.mail),
    NewContext3 = crypto:md5_update(NewContext2, ?MD5_KEY1),
    NewContext4 = crypto:md5_update(NewContext3, ?MD5_KEY2),
    NewContext5 = crypto:md5_update(NewContext4, ?MD5_KEY3),

    {Year, Month, Day} = date(),
    {Hour, Min, Sec} = time(),
    DateTimeStr = 
	integer_to_list(Year) ++ 
	integer_to_list(Month) ++ 
	integer_to_list(Day) ++
	integer_to_list(Hour) ++
	integer_to_list(Min) ++
	integer_to_list(Sec),

    NewContext6 = crypto:md5_update(NewContext5, DateTimeStr),
    crypto:md5_final(NewContext6).

exist_in_list(List, Elem) ->
    case List of
	[] -> false;
	[E | Tail] -> case E of
			  Elem -> true;
			  _ -> exist_in_list(Tail, Elem)
		      end
    end.

authenticate(User, Password, OneTimePasswordList) ->
    case exist_in_list(OneTimePasswordList, Password) of
	true -> {ok, authenticated};
	false -> authenticate(User, Password)
    end. 

authenticate(User, RawPassword) ->
    Md5Password = get_md5_password(User, RawPassword),
    case User#user.password of
	Md5Password -> {ok, authenticated};
	_ ->           {error, unauthenticated}
    end.
	    
icon_path(Name) when is_atom(Name) -> 
    Dir = message_box_config:get(icon_dir),
    Dir ++ atom_to_list(Name).

%%
%% @doc ets shurink function
%%
shurink_ets(Device, MaxCount) ->
    case ets:first(Device) of
        '$end_of_table' -> 
            ok;
        First ->
            shurink_ets(Device, MaxCount, First, 1)
    end.

shurink_ets(Device, MaxCount, Index, Count) ->
    case ets:next(Device, Index) of
        '$end_of_table' -> 
            ok;
        Next ->
            if Count > MaxCount -> ets:delete(Device, Next);
               true -> ok
            end,
            shurink_ets(Device, MaxCount, Next, Count + 1)
    end.

