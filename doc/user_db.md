Module user_db
==============


<h1>Module user_db</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_user-3">add_user/3</a></td><td>add new user to system.</td></tr><tr><td valign="top"><a href="#call-2">call/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_user-1">delete_user/1</a></td><td>delete user.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>lookup user from pid.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_id-1">lookup_id/1</a></td><td>lookup user from id.</td></tr><tr><td valign="top"><a href="#lookup_name-1">lookup_name/1</a></td><td>lookup user from name(string).</td></tr><tr><td valign="top"><a href="#lookup_pid-1">lookup_pid/1</a></td><td>lookup user from pid.</td></tr><tr><td valign="top"><a href="#map_do-1">map_do/1</a></td><td>exec fun to each element of user list.</td></tr><tr><td valign="top"><a href="#reply-3">reply/3</a></td><td></td></tr><tr><td valign="top"><a href="#save_pid-2">save_pid/2</a></td><td>lookup user from pid.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>start user_db.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>start user_db.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#update_user-1">update_user/1</a></td><td>add new user to system.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_user-3"></a>

<h3>add_user/3</h3>





<pre>add_user(Name::atom(), Mail::string(), Password::string()) -> {ok, #user{}} | {error, already_exist}</pre>
<br></br>




add new user to system.
<a name="call-2"></a>

<h3>call/2</h3>





`call(Name, Args) -> any()`

<a name="delete_user-1"></a>

<h3>delete_user/1</h3>





<pre>delete_user(Id::integer()) -> ok</pre>
<br></br>




delete user.
<a name="get_pid-1"></a>

<h3>get_pid/1</h3>





<pre>get_pid(UserName_OR_Id::integer() | atom()) -> {ok, #user{}} | {error, not_found}</pre>
<br></br>




lookup user from pid.
<a name="init-1"></a>

<h3>init/1</h3>





`init(FileName) -> any()`

<a name="lookup_id-1"></a>

<h3>lookup_id/1</h3>





<pre>lookup_id(Id::integer()) -> {ok, #user{}} | {error, not_found}</pre>
<br></br>




lookup user from id.
<a name="lookup_name-1"></a>

<h3>lookup_name/1</h3>





<pre>lookup_name(Name::atom() | string()) -> {ok, #user{}} | {error, not_found}</pre>
<br></br>




lookup user from name(string).
<a name="lookup_pid-1"></a>

<h3>lookup_pid/1</h3>





<pre>lookup_pid(Pid::pid()) -> {ok, #user{}} | {error, not_found}</pre>
<br></br>




lookup user from pid.
<a name="map_do-1"></a>

<h3>map_do/1</h3>





<pre>map_do(Fun::function()) -> any()</pre>
<br></br>




exec fun to each element of user list.
<a name="reply-3"></a>

<h3>reply/3</h3>





`reply(To, Pid, Result) -> any()`

<a name="save_pid-2"></a>

<h3>save_pid/2</h3>





<pre>save_pid(Id::integer(), Pid::pid()) -> ok | {error, not_found}</pre>
<br></br>




lookup user from pid.
<a name="start-0"></a>

<h3>start/0</h3>





<pre>start() -> pid()</pre>
<br></br>




start user_db.
<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(FileName::string()) -> pid()</pre>
<br></br>




start user_db.
<a name="stop-0"></a>

<h3>stop/0</h3>





`stop() -> any()`

<a name="update_user-1"></a>

<h3>update_user/1</h3>





<pre>update_user(User::#user{}) -> {ok, #user{}} | {error, not_found}</pre>
<br></br>




add new user to system.
