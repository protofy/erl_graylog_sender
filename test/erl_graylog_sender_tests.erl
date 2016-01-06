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

-record(state, {compression, count, sender_ref, sender_mod, format, host, send_mode}).

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
  R7 = G(send_mode),
  R8 = S(send_mode, ensure_delivery),
  R9 = G(send_mode),
	unmock(erl_graylog_tcp_sender),
	[
	 {"before compression none", ?_assertEqual(none, R1)},
	 {"before format raw", ?_assertEqual(raw, R2)},
	 {"set compression gzip", ?_assertEqual(ok, R3)}, 
	 {"set format gelf", ?_assertEqual(ok, R4)}, 
	 {"after compression gzip", ?_assertEqual(gzip, R5)},
	 {"after format gelf", ?_assertEqual(gelf, R6)},
   {"before send_mode fire_and_forget", ?_assertEqual(fire_and_forget, R7)},
   {"set send_mode ensure_delivery", ?_assertEqual(ok, R8)},
   {"after send_mode ensure_delivery", ?_assertEqual(ensure_delivery, R9)}
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

send_2_gelf_fire_and_forget_test_() ->
  {R1, R2, R3, R4} = send_2_gelf_test_helper(fire_and_forget),
  [
    {"sent OK", ?_assertEqual(ok, R1)},
    {"received OK", ?_assertMatch({mocked_tcp_sent, _}, R2)},
    {"sent invalid", ?_assertEqual(ok, R3)},
    {"received invalid", ?_assertMatch({error, timeout}, R4)}
  ].

send_2_gelf_validate_message_test_() ->
  {R1, R2, R3, R4} = send_2_gelf_test_helper(validate_message),
  [
    {"sent OK", ?_assertEqual(ok, R1)},
    {"received OK", ?_assertMatch({mocked_tcp_sent, _}, R2)},
    {"sent invalid", ?_assertMatch({error, {invalid, {key, _}, _}}, R3)},
    {"received invalid", ?_assertMatch({error, timeout}, R4)}
  ].

send_2_gelf_ensure_delivery_test_() ->
  {R1, R2, R3, R4} = send_2_gelf_test_helper(ensure_delivery),
  [
    {"sent OK", ?_assertEqual({ok, sent}, R1)},
    {"received OK", ?_assertMatch({mocked_tcp_sent, _}, R2)},
    {"sent invalid", ?_assertMatch({error, {invalid, {key, _}, _}}, R3)},
    {"received invalid", ?_assertMatch({error, timeout}, R4)}
  ].


send_2_gelf_test_helper(Mode) ->
  mock_erl_graylog_tcp_sender({ok, sent}),
  ConnOpts = [{type, tcp}],
  Opts = [{connection, ConnOpts}, {compression, none}, {format, gelf}, {host, <<"testhost">>}],
  {ok, Pid} = erl_graylog_sender:start_link(Opts),
  erl_graylog_sender:set_opt(Pid, send_mode, Mode),
  R1 = erl_graylog_sender:send(Pid, [{full_message, <<"1">>}]),
  R2 = rcv(100),
  R3 = erl_graylog_sender:send(Pid, [{invalid, <<"invalid">>}]),
  R4 = rcv(100),
  unmock(erl_graylog_tcp_sender),
  {R1, R2, R3, R4}.


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
handle_cast_test_() ->
  ?_assertEqual({noreply, test}, erl_graylog_sender:handle_cast(cast, test)).


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

%% Tests for do_send/2
%% ====================================================================
do_send_2_test_() ->
  S = #state{
    compression = none,
    sender_ref = return,
    sender_mod = ?MODULE,
    format = gelf,
    host = <<"testhost">>,
    send_mode = validate_message
  },
  F = fun(Msg) -> erl_graylog_sender:do_send(Msg, S) end,
  Msg = [{full_message, <<"Full Message">>}],
  [
    {"ok", ?_assertEqual(ok, F(Msg))},
    {"invalid", ?_assertMatch({error, {invalid, {key, _}, prefix}}, F([{invalid_additional, <<"Invalid">>}|Msg]))}
  ].

%% Tests for send_sendable/4
%% ====================================================================
send_sendable_test_() ->
  {ok, Sendable} = erl_graylog_sender:sendable(<<"body">>, none, raw, undefined),
  F = fun(Mode) -> erl_graylog_sender:send_sendable(?MODULE, return, Sendable, Mode) end,
  [
    {"fire_and_forget", ?_assertEqual(ok, F(fire_and_forget))},
    {"validate_message", ?_assertEqual(ok, F(validate_message))},
    {"ensure_delivery", ?_assertEqual({sent, return, Sendable}, F(ensure_delivery))}
  ].

%% Tests for sendable/4
%% ====================================================================
%% The actual conversion is tested in erl_graylog_gelf_tests
sendable_test_() ->
  Host = <<"sendable_4_host">>,
  Body = <<"body">>,
  Msg = [{full_message, Body}, {<<"_extra1">>, <<"extra1">>}],
  MsgWithHost = [{host, <<"set host">>}|Msg],
  F = fun(X, Fmt) -> erl_graylog_sender:sendable(X, none, Fmt, Host) end,
  T = fun(X, Fmt) -> ?_assertMatch({ok, B} when is_binary(B), F(X, Fmt)) end,
  THost = fun(X, Fmt, E) ->
    {ok, R} = F(X, Fmt),
    {Data} = jiffy:decode(R),
    A = ?GV(<<"host">>, Data),
    ?_assertEqual(E, A)
  end,
  [
    {"binary no host gelf", T(Body, gelf)},
    {"list no host gelf", T(Msg, gelf)},
    {"host set", THost(Msg, gelf, Host)},
    {"list host gelf", T(MsgWithHost, gelf)},
    {"binary raw", T(Body, raw)},
    {"list raw", T(Msg, raw)},
    {"invalid", ?_assertMatch({error, {invalid, _}}, F(Msg, invalid))}
  ].

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

%% Test valid_mode/1
%% ====================================================================
valid_mode_test_() ->
  F = fun erl_graylog_sender:valid_send_mode/1,
  T = fun(E, X) -> ?_assertEqual(E, F(X)) end,
  [
    {"default", T(fire_and_forget, default)},
    {"fire_and_forget", T(fire_and_forget, fire_and_forget)},
    {"validate_message", T(validate_message, validate_message)},
    {"ensure_delivery", T(ensure_delivery, ensure_delivery)},
    {"invalid", ?_assertException(throw, {invalid, {send_mode, _}}, F(undefined))}
  ].


%% ====================================================================
%% Helpers
%% ====================================================================

%% Pseudo sender that can be used for testing 
%% ====================================================================
mock_erl_graylog_tcp_sender() ->
  mock_erl_graylog_tcp_sender(ok).

mock_erl_graylog_tcp_sender(SendReturnValue) ->
	ok = meck:new(erl_graylog_tcp_sender, []),
	ok = meck:expect(erl_graylog_tcp_sender, open, fun(Opts) -> {ok, {mocked_tcp, Opts}} end),
	Self = self(),
	ok = meck:expect(erl_graylog_tcp_sender, send, fun({mocked_tcp, _}, Msg) -> Self ! {mocked_tcp_sent, Msg}, SendReturnValue end),
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

rcv(N) ->
  protofy_test_util:rcv(N).


%% rcv(0) ->
%% 	{error, timeout};
%% rcv(N) ->
%% 	receive
%% 		X -> X
%% 	after 10 -> rcv(N-1)
%% 	end.

