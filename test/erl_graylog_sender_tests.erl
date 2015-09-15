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
%% @doc @todo Add description to erl_graylod_sender_tests.


-module(erl_graylog_sender_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("protofy_common/include/protofy_common.hrl").

-define(TEST_PORT, 12200).
-define(TEST_CONNECTION_TCP, [{type, tcp}, {addr, {127,0,0,1}}, {port, ?TEST_PORT}, {ip_version, ipv4}]).
-define(TEST_CONNECTION_UDP, [{type, udp}, {addr, {127,0,0,1}}, {port, ?TEST_PORT}, {ip_version, ipv4}]).

-export([]).

-record(state, {compression, count, sender_ref, sender_mod, format, host}).

%% ====================================================================
%% API functions
%% ====================================================================

%% Test start_link/1, stop/0, stop/1
%% ====================================================================
start_stop_unnamed_test_() ->
	{ok, Pid} = start_srv(undefined),
	R0 = is_process_alive(Pid),
	erl_graylog_sender:stop(Pid),
	R1 = protofy_test_util:wait_for_stop(Pid, 10),
	R2 = rcv(100),
	unmock(erl_graylog_tcp_sender),
	[{"started", ?_assert(R0)},
	 {"stopped", ?_assertEqual(ok, R1)},
	 {"close received", ?_assertEqual(mocked_tcp_close, R2)}
	].
	

start_stop_named_test_() ->
	{ok, Pid1} = start_srv(test_ref),
	Pid2 = whereis(test_ref),
	R0 = is_process_alive(Pid1),
	erl_graylog_sender:stop(test_ref),
	R1 = protofy_test_util:wait_for_stop(Pid1, 10),
	R2 = rcv(100),
	unmock(erl_graylog_tcp_sender),
	[{"started", ?_assert(R0)},
	 {"stopped", ?_assertEqual(ok, R1)},
	 {"close received", ?_assertEqual(mocked_tcp_close, R2)},
	 {"pid is server", ?_assertEqual(Pid1, Pid2)}
	].


start_stop_singleton_test_() ->
	{ok, Pid1} = start_srv(singleton),
	Pid2 = whereis(erl_graylog_sender),
	R0 = is_process_alive(Pid1),
	erl_graylog_sender:stop(),
	R1 = protofy_test_util:wait_for_stop(Pid1, 10),
	R2 = rcv(100),
	unmock(erl_graylog_tcp_sender),
	[{"started", ?_assert(R0)},
	 {"stopped", ?_assertEqual(ok, R1)},
	 {"close received", ?_assertEqual(mocked_tcp_close, R2)},
	 {"pid is server", ?_assertEqual(Pid1, Pid2)}
	].

%% Test set_opt/3 and get_opt/2
%% ====================================================================
set_opt_3_get_opt_2_test_() ->
	{ok, Pid} = start_srv(undefined),
	G = fun(K) -> erl_graylog_sender:get_opt(Pid, K) end,
	S = fun(K, V) -> erl_graylog_sender:set_opt(Pid, K, V) end,
	R1 = G(compression),
	R2 = G(format),
	R3 = S(compression, gzip),
	R4 = S(format, gelf),
	R5 = G(compression),
	R6 = G(format),
	unmock(erl_graylog_tcp_sender),
	[
	 {"before compression none", ?_assertEqual(none, R1)},
	 {"before format raw", ?_assertEqual(raw, R2)},
	 {"set compression gzip", ?_assertEqual(ok, R3)}, 
	 {"set format gelf", ?_assertEqual(ok, R4)}, 
	 {"after compression gzip", ?_assertEqual(gzip, R5)},
	 {"after format gelf", ?_assertEqual(gelf, R6)}
	].
	

%% Test send/2
%% ====================================================================
send_2_test_() ->
	{ok, Pid} = start_srv(undefined),
	R1 = erl_graylog_sender:send(Pid, <<"send_2_test">>),
	R = rcv(100),
	unmock(erl_graylog_tcp_sender),
	[
	 {"started", ?_assertEqual(ok, R1)},
	 {"received", ?_assertMatch({mocked_tcp_sent, _}, R)}
	].


%% Test singleton: send/1, set_opt/2, get_opt/1
%% ====================================================================
singleton_test_() ->
	{ok, Pid1} = start_srv(singleton),
	Pid2 = whereis(erl_graylog_sender),
	R0 = erl_graylog_sender:send(<<"singleton_test">>),
	G = fun(K) -> erl_graylog_sender:get_opt(K) end,
	S = fun(K, V) -> erl_graylog_sender:set_opt(K, V) end,
	R1 = G(compression),
	R2 = G(format),
	R3 = S(compression, gzip),
	R4 = S(format, gelf),
	R5 = G(compression),
	R6 = G(format),
	R7 = rcv(100),
	unmock(erl_graylog_tcp_sender),
	[
	 {"started", ?_assertEqual(ok, R0)},
	 {"process is singleton", ?_assertEqual(Pid1, Pid2)},
	 {"before set compression none", ?_assertEqual(none, R1)},
	 {"before set format raw", ?_assertEqual(raw, R2)},
	 {"set compression gzip", ?_assertEqual(ok, R3)}, 
	 {"set format gelf", ?_assertEqual(ok, R4)}, 
	 {"after set compression gzip", ?_assertEqual(gzip, R5)},
	 {"after set format gelf", ?_assertEqual(gelf, R6)},
	 {"received", ?_assertMatch({mocked_tcp_sent, _}, R7)}
	].


%% ====================================================================
%% Test behavioural functions
%% ====================================================================

%% Test init/1
%% ====================================================================
init_tcp_host_test() ->
	mock_erl_graylog_tcp_sender(),
	ConnOpts = [{type, tcp}],
	Opts = [{connection, ConnOpts}, {compression, none}, {format, raw}, {host, <<"testhost">>}],
	R = erl_graylog_sender:init([Opts]),
	unmock(erl_graylog_tcp_sender),
	?assertMatch({ok, #state{}}, R),
	{ok, S} = R,
	?assertEqual(none, S#state.compression),
	?assertMatch(I when is_integer(I), S#state.count),
	?assertMatch({mocked_tcp, _}, S#state.sender_ref),
	?assertEqual(erl_graylog_tcp_sender, S#state.sender_mod),
	?assertEqual(raw, S#state.format),
	?assertEqual(<<"testhost">>, S#state.host).

init_udp_auto_host_test() ->
	mock_erl_graylog_udp_sender(),
	ConnOpts = [{type, udp}],
	Opts = [{connection, ConnOpts}, {compression, none}, {format, raw}],
	{ok, Host} = inet:gethostname(), 
	R = erl_graylog_sender:init([Opts]),
	unmock(erl_graylog_udp_sender),
	?assertMatch({ok, #state{}}, R),
	{ok, S} = R,
	?assertEqual(none, S#state.compression),
	?assertMatch(I when is_integer(I), S#state.count),
	?assertMatch({mocked_udp, _}, S#state.sender_ref),
	?assertEqual(erl_graylog_udp_sender, S#state.sender_mod),
	?assertEqual(raw, S#state.format),
	?assertEqual(list_to_binary(Host), S#state.host).


%% Test handle_call/3
%% ====================================================================
handle_call_test_() ->
	S = #state{compression = none, format = raw},
	F = fun(Msg) -> erl_graylog_sender:handle_call(Msg, undefined, S) end,
	[
	 {"get_opt compression", ?_assertEqual({reply, none, S}, F({get_opt, compression}))},
	 {"get_opt format", ?_assertEqual({reply, raw, S}, F({get_opt, format}))},
	 {"set_opt compression", ?_assertEqual({reply, ok, S#state{compression = gzip}}, F({set_opt, compression, gzip}))},
	 {"set_opt format", ?_assertEqual({reply, ok, S#state{format = gelf}}, F({set_opt, format, gelf}))},
	 {"not implemented", ?_assertEqual({reply, {error, not_implemented}, S}, F(something))}
	].
	
	
handle_call_stop_test() ->
	Ref = {echo, self()},
	S = #state{compression = none, format = raw, sender_ref = Ref, sender_mod = ?MODULE},
	?assertEqual({stop, normal, ok, S}, erl_graylog_sender:handle_call(stop, undefined, S)),
	R = rcv(100),
	?assertEqual({closed, Ref}, R).
	

%% Test handle_cast/2
%% ====================================================================
handle_cast_test() ->
	Ref = {echo, self()},
	State = #state{ compression = none,
					count = 0,
					sender_ref = Ref,
					sender_mod = ?MODULE,
					format = raw,
					host = <<"testhost">> },
	Msg = <<"Casted">>,
	F = fun erl_graylog_sender:handle_cast/2,
	?assertEqual({noreply, State}, F({send, Msg}, State)),
	?assertEqual({noreply, State}, F(something, State)),
	R = rcv(100),
	?assertMatch({sent, Ref, Bin} when is_binary(Bin), R).
	

%% Test handle_info/2
%% ====================================================================
handle_info_test_() ->
	?_assertEqual({noreply, test}, erl_graylog_sender:handle_info(info, test)).


%% Test terminate/2
%% ====================================================================
terminate_test_() ->
	?_assertEqual(ok, erl_graylog_sender:terminate(undefined, test)).


%% Test code_change/3
%% ====================================================================
code_change_test_() ->
	?_assertEqual({ok, test}, erl_graylog_sender:code_change(undefined, test, undefined)).


%% ====================================================================
%% Test internal functions
%% ====================================================================

%% Tests for do_send/5
%% ====================================================================
do_send_binary_gelf_test() ->
	Msg1 = <<"Test Message 1">>,
	D = do_send_gelf_and_process(Msg1),
	?assertEqual(Msg1, ?GV(<<"full_message">>, D)),
	?assertEqual(Msg1, ?GV(<<"short_message">>, D)),
	?assertEqual(<<"testhost">>, ?GV(<<"host">>, D)).

do_send_list_gelf_no_host_test() ->
	Msg1 = [{full_message, <<"Test Message 1">>}, {short_message, <<"Shrt Msg">>}, {level, 1}],
	D = do_send_gelf_and_process(Msg1),
	?assertEqual(<<"Test Message 1">>, ?GV(<<"full_message">>, D)),
	?assertEqual(<<"Shrt Msg">>, ?GV(<<"short_message">>, D)),
	?assertEqual(1, ?GV(<<"level">>, D)),
	?assertEqual(<<"testhost">>, ?GV(<<"host">>, D)).
	
do_send_list_gelf_with_host_test() ->
	Msg1 = [{full_message, <<"Test Message 1">>}, {short_message, <<"Shrt Msg">>}, {level, 1}, {host, <<"some">>}],
	D = do_send_gelf_and_process(Msg1),
	?assertEqual(<<"Test Message 1">>, ?GV(<<"full_message">>, D)),
	?assertEqual(<<"Shrt Msg">>, ?GV(<<"short_message">>, D)),
	?assertEqual(1, ?GV(<<"level">>, D)),
	?assertEqual(<<"some">>, ?GV(<<"host">>, D)).

do_send_raw_test_() ->
	F = fun(Msg) -> erl_graylog_sender:do_send(Msg, none, return, ?MODULE, raw, <<"testhost">>) end,
	Msg1 = <<"Raw Test">>,
	[
	 {"binary", ?_assertEqual({sent, return, Msg1}, F(Msg1))},
	 {"atom", ?_assertEqual({sent, return, <<"some">>}, F(some))},
	 {"integer", ?_assertEqual({sent, return, <<"42">>}, F(42))},
	 {"tuple", ?_assertEqual({sent, return, <<"{4,2}">>}, F({4,2}))}
	].

do_send_gelf_and_process(Msg) ->
	R = erl_graylog_sender:do_send(Msg, none, return, ?MODULE, gelf, <<"testhost">>),
	?assertMatch({sent, return, Bin} when is_binary(Bin), R),
	{sent, return, Bin} = R,
	{D} = jiffy:decode(Bin),
	?assert(proplists:is_defined(<<"timestamp">>, D)),
	?assertEqual(<<"1.1">>, ?GV(<<"version">>, D)),
	D.


%% Test valid_compression/1
%% ====================================================================
valid_compression_test_() ->
	F = fun erl_graylog_sender:valid_compression/1,
	T = fun(E, X) -> ?_assertEqual(E, F(X)) end,
	[
	 {"default", T(gzip, default)},
	 {"none", T(none, none)},
	 {"gzip", T(gzip, gzip)},
	 {"zlib", T(zlib, zlib)},
	 {"invalid", ?_assertException(throw, {invalid, {compression, _}}, F(undefined))}
	].
	 

%% Test valid_format/1
%% ====================================================================
valid_format_test_() ->
	F = fun erl_graylog_sender:valid_format/1,
	T = fun(E, X) -> ?_assertEqual(E, F(X)) end,
	[
	 {"default", T(gelf, default)},
	 {"gelf", T(gelf, gelf)},
	 {"raw", T(raw, raw)},
	 {"invalid", ?_assertException(throw, {invalid, {format, _}}, F(undefined))}
	].

%% ====================================================================
%% Helpers
%% ====================================================================

%% Pseudo sender that can be used for testing 
%% ====================================================================
mock_erl_graylog_tcp_sender() ->
	ok = meck:new(erl_graylog_tcp_sender, []),
	ok = meck:expect(erl_graylog_tcp_sender, open, fun(Opts) -> {ok, {mocked_tcp, Opts}} end),
	Self = self(),
	ok = meck:expect(erl_graylog_tcp_sender, send, fun({mocked_tcp, _}, Msg) -> Self ! {mocked_tcp_sent, Msg}, ok end),
	ok = meck:expect(erl_graylog_tcp_sender, close, fun({mocked_tcp, _}) -> Self ! mocked_tcp_close, ok end),
	ok.

mock_erl_graylog_udp_sender() ->
	ok = meck:new(erl_graylog_udp_sender, []),
	ok = meck:expect(erl_graylog_udp_sender, open, fun(Opts) -> {ok, {mocked_udp, Opts}} end),
	ok.

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
	
start_srv_already_mocked(Ref) ->
	ConnOpts = [{type, tcp}],
	Opts = [{connection, ConnOpts}, {compression, none}, {format, raw}, {host, <<"testhost">>}, {server_ref, Ref}],
	erl_graylog_sender:start_link(Opts).

start_srv(Ref) ->
	mock_erl_graylog_tcp_sender(),
	start_srv_already_mocked(Ref).

send(return, Msg) ->
	{sent, return, Msg};
send({echo, To} = Ref, Msg) ->
	To ! {sent, Ref, Msg},
	{sent, Ref, Msg}.

close({echo, To} = Ref) ->
	To ! {closed, Ref}.

open(Opts) ->
	{opened, Opts}.

rcv(0) ->
	{error, timeout};
rcv(N) ->
	receive
		X -> X
	after 10 -> rcv(N-1)
	end.

