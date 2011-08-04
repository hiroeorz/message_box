Module util
===========


<h1>Module util</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td></td></tr><tr><td valign="top"><a href="#authenticate-3">authenticate/3</a></td><td></td></tr><tr><td valign="top"><a href="#db_info-1">db_info/1</a></td><td>sqlite3 database file name.</td></tr><tr><td valign="top"><a href="#formatted_number-2">formatted_number/2</a></td><td></td></tr><tr><td valign="top"><a href="#formatted_number-3">formatted_number/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_md5_password-2">get_md5_password/2</a></td><td>create md5 password.</td></tr><tr><td valign="top"><a href="#get_onetime_password-2">get_onetime_password/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_reply_list-1">get_reply_list/1</a></td><td>create reply name list from tweet text.</td></tr><tr><td valign="top"><a href="#get_timeline_ids-4">get_timeline_ids/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_user_from_message_id-1">get_user_from_message_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_user_id_from_message_id-1">get_user_id_from_message_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#icon_path-1">icon_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_reply_text-1">is_reply_text/1</a></td><td></td></tr><tr><td valign="top"><a href="#shurink_ets-2">shurink_ets/2</a></td><td>ets shurink function.</td></tr><tr><td valign="top"><a href="#sleep-1">sleep/1</a></td><td>sleep function.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="authenticate-2"></a>

<h3>authenticate/2</h3>





`authenticate(User, RawPassword) -> any()`

<a name="authenticate-3"></a>

<h3>authenticate/3</h3>





`authenticate(User, Password, OneTimePasswordList) -> any()`

<a name="db_info-1"></a>

<h3>db_info/1</h3>





`db_info(UserName) -> any()`



sqlite3 database file name
<a name="formatted_number-2"></a>

<h3>formatted_number/2</h3>





`formatted_number(Num, Len) -> any()`

<a name="formatted_number-3"></a>

<h3>formatted_number/3</h3>





`formatted_number(Num, Len, EmptyChar) -> any()`

<a name="get_md5_password-2"></a>

<h3>get_md5_password/2</h3>





`get_md5_password(User, RawPassword) -> any()`



create md5 password
<a name="get_onetime_password-2"></a>

<h3>get_onetime_password/2</h3>





`get_onetime_password(User, RawPassword) -> any()`

<a name="get_reply_list-1"></a>

<h3>get_reply_list/1</h3>





`get_reply_list(Text) -> any()`



create reply name list from tweet text.
<a name="get_timeline_ids-4"></a>

<h3>get_timeline_ids/4</h3>





`get_timeline_ids(Device, Count, Before, Result) -> any()`

<a name="get_user_from_message_id-1"></a>

<h3>get_user_from_message_id/1</h3>





`get_user_from_message_id(MessageId) -> any()`

<a name="get_user_id_from_message_id-1"></a>

<h3>get_user_id_from_message_id/1</h3>





`get_user_id_from_message_id(MessageId) -> any()`

<a name="icon_path-1"></a>

<h3>icon_path/1</h3>





`icon_path(Name) -> any()`

<a name="is_reply_text-1"></a>

<h3>is_reply_text/1</h3>





`is_reply_text(Text) -> any()`

<a name="shurink_ets-2"></a>

<h3>shurink_ets/2</h3>





`shurink_ets(Device, MaxCount) -> any()`



ets shurink function
<a name="sleep-1"></a>

<h3>sleep/1</h3>





`sleep(Msec) -> any()`



sleep function
