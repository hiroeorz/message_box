Module message_db
=================


<h1>Module message_db</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_latest_message-1">get_latest_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_message-1">get_message/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_message-2">get_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_sent_timeline-2">get_sent_timeline/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_message-2">save_message/2</a></td><td>export functions.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>initial setup functions.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_latest_message-1"></a>

<h3>get_latest_message/1</h3>





`get_latest_message(Pid) -> any()`

<a name="get_message-1"></a>

<h3>get_message/1</h3>





`get_message(Id) -> any()`

<a name="get_message-2"></a>

<h3>get_message/2</h3>





`get_message(Pid, Id) -> any()`

<a name="get_sent_timeline-2"></a>

<h3>get_sent_timeline/2</h3>





`get_sent_timeline(Pid, Count) -> any()`

<a name="init-2"></a>

<h3>init/2</h3>





`init(UserName, DBPid) -> any()`

<a name="save_message-2"></a>

<h3>save_message/2</h3>





`save_message(Pid, Text) -> any()`



export functions
<a name="start-2"></a>

<h3>start/2</h3>





`start(UserName, DBPid) -> any()`



initial setup functions
<a name="stop-1"></a>

<h3>stop/1</h3>





`stop(Pid) -> any()`

