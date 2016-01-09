%% ====================================================================
%%
%% Copyright (c) Protofy GmbH & Co. KG, Kaiser-Wilhelm-Straße 85, 20355 Hamburg/Germany and individual contributors.
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%% 
%%     1. Redistributions of source code must retain the above copyright notice,
%%        this list of conditions and the following disclaimer.
%% 
%%     2. Redistributions in binary form must reproduce the above copyright
%%        notice, this list of conditions and the following disclaimer in the
%%        documentation and/or other materials provided with the distribution.
%% 
%%     3. Neither the name of Protofy GmbH & Co. KG nor the names of its contributors may be used
%%        to endorse or promote products derived from this software without
%%        specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% ====================================================================
%%
%% @author Bjoern Kortuemm (@uuid0) <bjoern@protofy.com>
%% @doc Tests for module graylog_tcp_sender.
-module(graylog_tcp_sender_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("protofy_common/include/protofy_common.hrl").

-export([]).

%% ====================================================================
%% Test API functions
%% ====================================================================

%% Test open/1
%% ====================================================================
open_variants_test_() ->
	F = fun graylog_tcp_sender:open/1,
	O = [{addr, {127,0,0,1}}, {port, 12345}],
	T = fun(E, Opts) -> {ok, {mock_socket, _Addr, _Port, AOpts}} = F(Opts),
						?_assert(proplists:is_defined(E, AOpts)) end,
	{setup,
	 fun() ->
			 Self = self(),
			 SendCb = fun(Packet) -> Self ! {mock_tcp, Packet} end,
			 mock_gen_tcp(SendCb)
	 end,
	 fun(_) ->
			 unmock(gen_tcp)
	 end,
	 fun() -> [
			   {"ipv4", T(inet, [{ip_version, ipv4} | O])},
			   {"ipv6", T(inet6, [{ip_version, ipv6} | O])}, 
			   {"default ip version", T(inet, [{ip_version, default} | O])}, 
			   {"auto ip version", T(inet, O)},
			   {"addr port", ?_assertMatch({ok, {mock_socket, {127,0,0,1}, 12345, _}},F(O))}
			  ] end}.
	

%% Test open/1, send/1, close/1
%% ====================================================================
open_send_close_test() ->
	O = [{addr, {127,0,0,1}}, {port, 12345}],
	Self = self(),
	TestPacket = crypto:rand_bytes(20),
	SendCb = fun(Packet) -> Self ! {mock_tcp, Packet} end,
	mock_gen_tcp(SendCb),
	{ok, Ref} = graylog_tcp_sender:open(O),
	graylog_tcp_sender:send(Ref, TestPacket),
	R = rcv(100),
	graylog_tcp_sender:close(Ref),
	unmock(gen_tcp),
	?assertEqual({mock_tcp, <<TestPacket/binary, 0>>}, R).

%% ====================================================================
%% Internal functions
%% ====================================================================
rcv(0) ->
	{error, timeout};
rcv(N) ->
	receive
		X -> X
	after 10 -> rcv(N-1)
	end.

mock_gen_tcp(SendCb) ->
	ok = meck:new(gen_tcp, [unstick]),
	ok = meck:expect(gen_tcp, connect, fun(Addr, Port, Opts) -> {ok, {mock_socket, Addr, Port,Opts}} end),
	ok = meck:expect(gen_tcp, send,
					 fun(_Socket, Packet) -> SendCb(Packet), ok end),
	ok = meck:expect(gen_tcp, close, fun({mock_socket, _,_,_}) -> ok end).

unmock(Mod) ->
	try
		meck:unload(Mod),
		code:ensure_loaded(Mod)
	catch
		error:{not_mocked, Mod} -> ok;
		error:Reason ->
			error_logger:error_report({?MODULE, unmock, Mod, Reason}),
			ok
	end.
