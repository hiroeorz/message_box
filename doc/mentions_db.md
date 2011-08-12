Module mentions_db
==================


<h1>Module mentions_db</h1>

* [Function Index](#index)
* [Function Details](#functions)






<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_timeline-2">get_timeline/2</a></td><td>create mentions timeline and return list of message.</td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#save_message_id-2">save_message_id/2</a></td><td>save message_id to ets and sqlite3 database.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>start process.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>stop process.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_timeline-2"></a>

<h3>get_timeline/2</h3>





<pre>get_timeline(Pid::pid(), Count::integer()) -> [#message{}]</pre>
<br></br>




create mentions timeline and return list of message.
<a name="init-2"></a>

<h3>init/2</h3>





`init(UserName, DBPid) -> any()`

<a name="save_message_id-2"></a>

<h3>save_message_id/2</h3>





<pre>save_message_id(Pid::pid(), Message::#message{}) -> ok</pre>
<br></br>




save message_id to ets and sqlite3 database.
<a name="start-2"></a>

<h3>start/2</h3>





<pre>start(UserName::atom(), DBPid::pid()) -> pid()</pre>
<br></br>




start process
<a name="stop-1"></a>

<h3>stop/1</h3>





<pre>stop(Pid::pid()) -> ok</pre>
<br></br>




stop process
