%% -*- erlang -*-
% RBridge - A Ruby to Erlang bridge allowing the use of Erlang commands within 
% normal Ruby code.
% Copyright (C) 2008 Toshiyuki Hirooka <toshi.hirooka@gmail.com>, Chuck Vose <vosechu@gmail.com>
% 
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-module(rulang).
-export([start_server/1, stop_server/0, loop/1, handle_connection/1, main/1, usage/0]).

main([Port]) ->
  try
    start_server(Port)
  catch
    _:_ ->
    % TODO: This should report the error as well as the usage
      usage()
  end;
main(_) ->
  usage().
  
usage() ->
  io:format("./rulang [port number]").

% Start a server and connect to the port specified by the bound variable Port.
% If all goes according to plan start the rulang loop and display a happy message.
start_server(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
	io:format("[RBridge Server Started]~n"),
	loop(ListenSocket).

% Just kill the parent thread.
stop_server() ->
  io:format("[RBridge Server Stopped]~n"),
	exit(self()).

% Loop forever waiting for connections to wake us and either spawn a thread to
% deal with commands or die if the connection is broken.
loop(ListenSocket) ->
  % Block until we have a connection waiting
	case gen_tcp:accept(ListenSocket) of
	  
    % If the connection is valid read the commands and attempt to eval them.
    {ok, Socket} -> 
      spawn(fun() ->
        handle_connection(Socket)
      end),
    
    % Continue the loop
  	loop(ListenSocket)
  end.
	
% Wrapper that catches errors and closes the socket after we're done with it.
handle_connection(Socket) ->
  % Try to execute the commands in the tcp socket.
  try communication(Socket)
  % But catch any errors and pass them along to the tcp socket so we can see 
  % them in ruby.
  catch
    error:Reason ->
      {gen_tcp:send(Socket, io_lib:format("Error: ~p~n", [Reason]))}
  end,
  % Finally make sure the socket gets closed.
  ok = gen_tcp:close(Socket).

% Try to evaluate the code submitted throwing an exception if the evaluation
% doesn't work. 
communication(Socket) ->
  % Read the commands from the tcp socket
	{ok, Binary} = gen_tcp:recv(Socket, 0),
  % Run the commands through the erlang eval function
	{ok, Result} = eval(binary_to_list(Binary)),
  % Send the results back through the tcp pipe attempting to reconstitute the
  % Erlang values back into Ruby data structures along the way. 
	gen_tcp:send(Socket, io_lib:format("~p~n", [to_ruby(Result)])).
	
% Parse the commands into Erlang terms and run them. Return the result of the
% evaluation.
eval(Expression) ->
	{done, {ok, Scanned, _}, _} = erl_scan:tokens([], Expression, 0),
	{ok, Parsed} = erl_parse:parse_exprs(Scanned),
	{value, Result, _} = erl_eval:exprs(Parsed, []),
	{ok, Result}.

% Change Erlang values into Ruby values.
to_ruby(Expression) when is_list(Expression) ->
	lists:map(fun(X) -> to_ruby(X) end, Expression);
	%list_to_binary("'" ++ Expression ++ "'");
to_ruby(Expression) when is_tuple(Expression) ->
	to_ruby(tuple_to_list(Expression));
to_ruby(Expression) when is_binary(Expression) ->
	to_ruby(binary_to_list(Expression));
to_ruby(Expression) when is_integer(Expression) ->
	Expression;
to_ruby(Expression) when is_float(Expression) ->
	Expression;
to_ruby(Expression) when is_atom(Expression) ->
	atom_to_list(Expression).