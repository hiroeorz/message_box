Module m_user
=============


<h1>Module m_user</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_follower-2">add_follower/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_follower-2">delete_follower/2</a></td><td></td></tr><tr><td valign="top"><a href="#follow-3">follow/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_follower_ids-1">get_follower_ids/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_home_timeline-2">get_home_timeline/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_icon-1">get_icon/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_mentions_timeline-2">get_mentions_timeline/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_message-2">get_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_sent_timeline-2">get_sent_timeline/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_follow-2">is_follow/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_icon-3">save_icon/3</a></td><td></td></tr><tr><td valign="top"><a href="#save_to_home-2">save_to_home/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_to_home-3">save_to_home/3</a></td><td></td></tr><tr><td valign="top"><a href="#save_to_mentions-2">save_to_mentions/2</a></td><td></td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td></td></tr><tr><td valign="top"><a href="#set_onetime_password-2">set_onetime_password/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#unfollow-3">unfollow/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td>export functions.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_follower-2"></a>

<h3>add_follower/2</h3>





`add_follower(UserName_OR_Id, UserId) -> any()`

<a name="delete_follower-2"></a>

<h3>delete_follower/2</h3>





`delete_follower(UserName_OR_Id, UserId) -> any()`

<a name="follow-3"></a>

<h3>follow/3</h3>





`follow(UserName_OR_Id, Password, UserId) -> any()`

<a name="get_follower_ids-1"></a>

<h3>get_follower_ids/1</h3>





`get_follower_ids(UserName_OR_Id) -> any()`

<a name="get_home_timeline-2"></a>

<h3>get_home_timeline/2</h3>





`get_home_timeline(UserName_OR_Id, Count) -> any()`

<a name="get_icon-1"></a>

<h3>get_icon/1</h3>





`get_icon(UserName_OR_Id) -> any()`

<a name="get_mentions_timeline-2"></a>

<h3>get_mentions_timeline/2</h3>





`get_mentions_timeline(UserName_OR_Id, Count) -> any()`

<a name="get_message-2"></a>

<h3>get_message/2</h3>





`get_message(UserName_OR_Id, MessageId) -> any()`

<a name="get_sent_timeline-2"></a>

<h3>get_sent_timeline/2</h3>





`get_sent_timeline(UserName_OR_Id, Count) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(UserName) -> any()`

<a name="is_follow-2"></a>

<h3>is_follow/2</h3>





`is_follow(UserName_OR_Id, UserId) -> any()`

<a name="save_icon-3"></a>

<h3>save_icon/3</h3>





`save_icon(UserName_OR_Id, Data, ContentType) -> any()`

<a name="save_to_home-2"></a>

<h3>save_to_home/2</h3>





`save_to_home(UserName_OR_Id, MessageId) -> any()`

<a name="save_to_home-3"></a>

<h3>save_to_home/3</h3>





`save_to_home(UserName_OR_Id, MessageId, IsReplyText) -> any()`

<a name="save_to_mentions-2"></a>

<h3>save_to_mentions/2</h3>





`save_to_mentions(UserName_OR_Id, MessageId) -> any()`

<a name="send_message-3"></a>

<h3>send_message/3</h3>





`send_message(UserName_OR_Id, Password, Text) -> any()`

<a name="set_onetime_password-2"></a>

<h3>set_onetime_password/2</h3>





`set_onetime_password(UserName_OR_Id, OneTimePassword) -> any()`

<a name="start-1"></a>

<h3>start/1</h3>





`start(UserName) -> any()`

<a name="stop-1"></a>

<h3>stop/1</h3>





`stop(UserName) -> any()`

<a name="unfollow-3"></a>

<h3>unfollow/3</h3>





`unfollow(UserName_OR_Id, Password, UserId) -> any()`

<a name="update-4"></a>

<h3>update/4</h3>





`update(UserName_OR_Id, AuthPassword, Mail, Password) -> any()`



export functions
