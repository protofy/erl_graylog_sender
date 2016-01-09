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
%% @doc Tests for module graylog_udp_sender.
-module(graylog_udp_sender_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("protofy_common/include/protofy_common.hrl").

%% ====================================================================
%% Test API functions
%% ====================================================================
-define(CHUNK(N, X), list_to_binary(lists:duplicate(N, X))).
-define(CHUNK(N), ?CHUNK(N, 1)).


open_variants_test_() ->
	F = fun graylog_udp_sender:open/1,
	Addr = {127,0,0,1},
	Port = 12345,
	O = [{addr, Addr}, {port, Port}],
	T = fun(E, Opts) -> {ok, {{mock_socket, _Port, AOpts}, Addr, Port}} = F(Opts),
						?_assert(proplists:is_defined(E, AOpts)) end,
	{setup,
	 fun() ->
			 Self = self(),
			 SendCb = fun(Packet) -> Self ! {mock_udp, Packet} end,
			 mock_gen_udp(SendCb)
	 end,
	 fun(_) ->
			 unmock(gen_udp)
	 end,
	 fun() -> [
			   {"ipv4", T(inet, [{ip_version, ipv4} | O])},
			   {"ipv6", T(inet6, [{ip_version, ipv6} | O])}, 
			   {"default ip version", T(inet, [{ip_version, default} | O])}, 
			   {"auto ip version", T(inet, O)}
			  ] end}.
	

open_send_close_test() ->
	O = [{addr, {127,0,0,1}}, {port, 12345}],
	Self = self(),
	TestPacket = ?CHUNK(100),
	SendCb = fun(_Addr, _Port, Packet) -> Self ! {mock_udp, Packet} end,
	mock_gen_udp(SendCb),
	{ok, Ref} = graylog_udp_sender:open(O),
	graylog_udp_sender:send(Ref, TestPacket),
	R = rcv(100),
	graylog_udp_sender:close(Ref),
	unmock(gen_udp),
	?assertEqual({mock_udp, TestPacket}, R).

make_chunk_4_test_() ->
	F = fun graylog_udp_sender:make_chunk/4,
	C = fun(Data, MsgId, Seq, Num) -> BinSeq = binary:encode_unsigned(Seq),
									  <<16#1e, 16#0f, MsgId:8/binary, BinSeq:1/binary, Num:1/binary, Data/binary>> end,
	Ok = fun(Data, MsgId, Seq, Num) -> ?_assertEqual(C(Data, MsgId, Seq, Num), F(Data, MsgId, Seq, Num)) end,
	ErrBA = fun(Data, MsgId, Seq, Num) -> ?_assertError(badarg, F(Data, MsgId, Seq, Num)) end,
	[
	 {"ok full", Ok(<<"Data">>, <<"12345678">>, 1, <<5>>)},
	 {"error payload list", ErrBA("Data", <<"12345678">>, 1, <<5>>)},
	 {"error payload atom", ErrBA(data, <<"12345678">>, 1, <<5>>)},
	 {"error payload integer", ErrBA(1, <<"12345678">>, 1, <<5>>)},
	 {"error messageid list", ErrBA(<<"Data">>, "12345678", 1, <<5>>)},
	 {"error messageid atom", ErrBA(<<"Data">>, msgid, 1, <<5>>)},
	 {"error messageid integer", ErrBA(<<"Data">>, 1234, 1, <<5>>)}
	].

send_chunked_one_test() ->
	TestPacket = ?CHUNK(8192),
	R = send_recv_chunked(TestPacket, 1),
	?assertEqual([{mock_udp, TestPacket}], R).
	

send_chunked_two_test() ->
	R = send_recv_chunked(?CHUNK(8193), 2),
	Chunk1 = ?CHUNK(8180),
	Chunk2 = ?CHUNK(13),
	AChunks = [{Seq, Chunk, Num, MsgId} || {mock_udp, <<16#1e, 16#0f, MsgId:8/binary, Seq:1/binary, Num:1/binary, Chunk/binary>>} <- R],
	?assertMatch({<<0>>, Chunk1, <<2>>, _}, lists:keyfind(<<0>>, 1, AChunks)),
	?assertMatch({<<1>>, Chunk2, <<2>>, _}, lists:keyfind(<<1>>, 1, AChunks)).

send_chunked_exactly_two_test() ->
	R = send_recv_chunked(?CHUNK(2*8180), 2),
	Chunk1 = ?CHUNK(8180),
	Chunk2 = ?CHUNK(8180),
	AChunks = [{Seq, Chunk, Num, MsgId} || {mock_udp, <<16#1e, 16#0f, MsgId:8/binary, Seq:1/binary, Num:1/binary, Chunk/binary>>} <- R],
	?assertMatch({<<0>>, Chunk1, <<2>>, _}, lists:keyfind(<<0>>, 1, AChunks)),
	?assertMatch({<<1>>, Chunk2, <<2>>, _}, lists:keyfind(<<1>>, 1, AChunks)).

send_chunked_three_test() ->
	Chunk1 = ?CHUNK(8180),
	Chunk2 = ?CHUNK(8180),
	Chunk3 = <<3>>,
	R = send_recv_chunked(<<Chunk1/binary, Chunk2/binary, Chunk3/binary>>, 3),
	AChunks = [{Seq, Chunk, Num, MsgId} || {mock_udp, <<16#1e, 16#0f, MsgId:8/binary, Seq:1/binary, Num:1/binary, Chunk/binary>>} <- R],
	?assertMatch({<<0>>, Chunk1, <<3>>, _}, lists:keyfind(<<0>>, 1, AChunks)),
	?assertMatch({<<1>>, Chunk2, <<3>>, _}, lists:keyfind(<<1>>, 1, AChunks)),
	?assertMatch({<<2>>, Chunk3, <<3>>, _}, lists:keyfind(<<2>>, 1, AChunks)).

send_chunked_exactly_three_test() ->
	Chunk1 = ?CHUNK(8180),
	Chunk2 = ?CHUNK(8180),
	Chunk3 = ?CHUNK(8180),
	R = send_recv_chunked(<<Chunk1/binary, Chunk2/binary, Chunk3/binary>>, 3),
	AChunks = [{Seq, Chunk, Num, MsgId} || {mock_udp, <<16#1e, 16#0f, MsgId:8/binary, Seq:1/binary, Num:1/binary, Chunk/binary>>} <- R],
	?assertMatch({<<0>>, Chunk1, <<3>>, _}, lists:keyfind(<<0>>, 1, AChunks)),
	?assertMatch({<<1>>, Chunk2, <<3>>, _}, lists:keyfind(<<1>>, 1, AChunks)),
	?assertMatch({<<2>>, Chunk3, <<3>>, _}, lists:keyfind(<<2>>, 1, AChunks)).

send_chunked_max_test() ->
	Packet = ?CHUNK(8180*128),
	R = send_recv_chunked(Packet, 128),
	AChunks = [{Seq, Chunk, Num, MsgId} || {mock_udp, <<16#1e, 16#0f, MsgId:8/binary, Seq:1/binary, Num:1/binary, Chunk/binary>>} <- R],
	?assertEqual(128, length(AChunks)). 

send_too_big_test_() ->
	?_assertException(throw, {message_too_big, _},
					  send_chunked(list_to_binary([1 || _ <- lists:seq(1, 128*8180+1)]))).

%% ====================================================================
%% Internal functions
%% ====================================================================
send_recv_chunked(TestPacket, N) ->
	Ref = send_chunked(TestPacket),
	R = rcv(100, N, []),
	graylog_udp_sender:close(Ref),
	unmock(gen_udp),
	R.

send_chunked(TestPacket) ->
	O = [{addr, {127,0,0,1}}, {port, 12345}],
	Self = self(),
	SendCb = fun(_Addr, _Port, Packet) -> Self ! {mock_udp, Packet} end,
	mock_gen_udp(SendCb),
	{ok, Ref} = graylog_udp_sender:open(O),
	graylog_udp_sender:send(Ref, TestPacket),
	Ref.
	

rcv(0) ->
	{error, timeout};
rcv(N) ->
	receive
		X -> X
	after 10 -> rcv(N-1)
	end.

rcv(0, _, A) ->
	{error, [{reason, timeout}, {ok, A}]};
rcv(_, 0, A) ->
	lists:reverse(A);
rcv(Tries, Expected, A) ->
	receive
		X -> rcv(Tries, Expected - 1, [X|A])
	after
			10 -> rcv(Tries-1, Expected, A)
	end.

mock_gen_udp(SendCb) ->
	ok = meck:new(gen_udp, [unstick]),
	ok = meck:expect(gen_udp, open, fun(Port, Opts) -> {ok, {mock_socket, Port, Opts}} end),
	ok = meck:expect(gen_udp, send,
					 fun(_Socket, Addr, Port, Packet) -> SendCb(Addr,Port,Packet), ok end),
	ok = meck:expect(gen_udp, close, fun({mock_socket,_,_}) -> ok end).

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
