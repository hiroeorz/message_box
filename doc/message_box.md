Module message_box
==================


<h1>Module message_box</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


MessageBox is Twitter clone using Erlang (for plactice).



Copyright (c) 2011 HIROE Shin


__Authors:__ HIROE Shin ([`twitter: http://twitter.com/#!/hiroe_orz17`](mailto:twitter: http://twitter.com/#!/hiroe_orz17)).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authenticate-2">authenticate/2</a></td><td>user authentication.</td></tr><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td>create new user.</td></tr><tr><td valign="top"><a href="#follow-3">follow/3</a></td><td>follow other user.</td></tr><tr><td valign="top"><a href="#get_home_timeline-2">get_home_timeline/2</a></td><td>get home timeline list.</td></tr><tr><td valign="top"><a href="#get_icon-1">get_icon/1</a></td><td>get icon data.</td></tr><tr><td valign="top"><a href="#get_mentions_timeline-2">get_mentions_timeline/2</a></td><td>get mentions timeline list.</td></tr><tr><td valign="top"><a href="#get_message-1">get_message/1</a></td><td>get message record.</td></tr><tr><td valign="top"><a href="#get_sent_timeline-2">get_sent_timeline/2</a></td><td>get sent timeline list.</td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td> return {ok,[{id,4},{name,"hoge"},{mail,"hoge@mail.com"}]}.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>system initializer.</td></tr><tr><td valign="top"><a href="#is_follow-2">is_follow/2</a></td><td>check following other user.</td></tr><tr><td valign="top"><a href="#save_icon-3">save_icon/3</a></td><td>save icon to disc.</td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td>send message to timeline.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>stating system using default setting file(conf/message_box.conf).</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>stating system.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>all system shuting down.</td></tr><tr><td valign="top"><a href="#unfollow-3">unfollow/3</a></td><td>unfollow other user.</td></tr><tr><td valign="top"><a href="#update_user-4">update_user/4</a></td><td>update user settings.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="authenticate-2"></a>

<h3>authenticate/2</h3>





<pre>authenticate(UserName::string() | atom(), Password::string()) -> {ok, #user{}} | {error, unauthenticated}</pre>
<br></br>




user authentication.
<a name="create_user-3"></a>

<h3>create_user/3</h3>





<pre>create_user(UserName::string() | atom(), Mail::string(), Password::string()) -> {ok, #user{}} | {error, already_exist}</pre>
<br></br>




create new user.
<a name="follow-3"></a>

<h3>follow/3</h3>





<pre>follow(UserId1::integer(), Password::string(), UserId2::integer()) -> ok | {error, already_following}</pre>
<br></br>




follow other user.
<a name="get_home_timeline-2"></a>

<h3>get_home_timeline/2</h3>





<pre>get_home_timeline(UserId_OR_Name::integer() | string(), Count::integer()) -> [message] | {error, not_found}</pre>
<br></br>




get home timeline list. sorted by recentry time.
<a name="get_icon-1"></a>

<h3>get_icon/1</h3>





<pre>get_icon(UserId_OR_Name::integer() | string()) -> {ok, binary(), string()} | {error, not_found} | {error, not_exist}</pre>
<br></br>




get icon data.
<a name="get_mentions_timeline-2"></a>

<h3>get_mentions_timeline/2</h3>





<pre>get_mentions_timeline(UserId_OR_Name::integer() | string(), Count::integer()) -> [message] | {error, not_found}</pre>
<br></br>




get mentions timeline list. sorted by recentry time.
<a name="get_message-1"></a>

<h3>get_message/1</h3>





<pre>get_message(MessageId::integer()) -> {ok, #message{}} | {error, not_found}</pre>
<br></br>




get message record.
<a name="get_sent_timeline-2"></a>

<h3>get_sent_timeline/2</h3>





<pre>get_sent_timeline(UserId_OR_Name::integer() | string(), Count::integer()) -> [message] | {error, not_found}</pre>
<br></br>




get sent timeline list. sorted by recentry time.
<a name="get_user-1"></a>

<h3>get_user/1</h3>





<pre>get_user(UserName::string()) -> {ok, [tuple()]} | {error, not_found}</pre>
<br></br>




 return {ok,[{id,4},{name,"hoge"},{mail,"hoge@mail.com"}]}
<a name="init-0"></a>

<h3>init/0</h3>





`init() -> any()`



system initializer.
<a name="is_follow-2"></a>

<h3>is_follow/2</h3>





<pre>is_follow(UserId_OR_Name::integer() | string(), Id::integer()) -> true | false | {error, not_found}</pre>
<br></br>




check following other user.
<a name="save_icon-3"></a>

<h3>save_icon/3</h3>





<pre>save_icon(UserId_OR_Name::integer() | string() | atom(), Data::binary(), ContentType::string()) -> ok</pre>
<br></br>




save icon to disc.
<a name="send_message-3"></a>

<h3>send_message/3</h3>





<pre>send_message(Id::integer(), Password::string(), Message::string()) -> {ok, integer()} | {error, unauthenticated}</pre>
<br></br>




send message to timeline.
<a name="start-0"></a>

<h3>start/0</h3>





<pre>start() -> pid()</pre>
<br></br>




stating system using default setting file(conf/message_box.conf).
<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(ConfigFilePath::string()) -> pid</pre>
<br></br>




stating system.
<a name="stop-0"></a>

<h3>stop/0</h3>





`stop() -> any()`



all system shuting down.
<a name="unfollow-3"></a>

<h3>unfollow/3</h3>





<pre>unfollow(UserId1::integer(), Password::string(), UserId2::integer()) -> ok | {error, not_following}</pre>
<br></br>




unfollow other user.
<a name="update_user-4"></a>

<h3>update_user/4</h3>





<pre>update_user(UserName::string() | atom(), AuthPassword::string(), Mail::string(), Password::string()) -> {ok, #user{}} | {error, user_not_exist}</pre>
<br></br>




update user settings.
