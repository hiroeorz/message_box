%% File : message_box_rpc.erl
%% Description : call mesage_box functions over rpc

-module(message_box_rpc).
-export([call/2]).

call(Fun, Args) ->
    Node = message_box_config:get(message_box_node),
    rpc:call(Node, message_box, Fun, Args).
