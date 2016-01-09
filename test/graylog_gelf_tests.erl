%% coding: utf-8
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
%% @doc Tests for module graylog_gelf.
-module(graylog_gelf_tests).

-export([]).

-define(CURRENT_GELF_VER, <<"1.1">>).

-include_lib("eunit/include/eunit.hrl").
-include_lib("protofy_common/include/protofy_common.hrl").

%% ====================================================================
%% Tests for API functions
%% ====================================================================

%% Test new/0
%% ====================================================================
new_0_test_() ->
	Msg = graylog_gelf:new(),
	[
	 {"version", ?_assertEqual(?CURRENT_GELF_VER, graylog_gelf:get_version(Msg))},
	 {"has timestamp", ?_assertMatch(Ts when is_number(Ts), graylog_gelf:get_timestamp(Msg))}
	].

%% Test new/1
%% ====================================================================
new_1_decimal_test_() ->
	Msg = graylog_gelf:new(1.1),
	[
	 {"version", ?_assertEqual(?CURRENT_GELF_VER, graylog_gelf:get_version(Msg))},
	 {"has timestamp", ?_assertMatch(Ts when is_number(Ts), graylog_gelf:get_timestamp(Msg))}
	].

new_1_atom_test_() ->
	Msg = graylog_gelf:new('1.1'),
	[
	 {"version", ?_assertEqual(?CURRENT_GELF_VER, graylog_gelf:get_version(Msg))},
	 {"has timestamp", ?_assertMatch(Ts when is_number(Ts), graylog_gelf:get_timestamp(Msg))}
	].

new_1_binary_test_() ->
	Msg = graylog_gelf:new(<<"1.1">>),
	[
	 {"version", ?_assertEqual(?CURRENT_GELF_VER, graylog_gelf:get_version(Msg))},
	 {"has timestamp", ?_assertMatch(Ts when is_number(Ts), graylog_gelf:get_timestamp(Msg))}
	].

new_1_list_test_() ->
	Msg = graylog_gelf:new("1.1"),
	[
	 {"version", ?_assertEqual(?CURRENT_GELF_VER, graylog_gelf:get_version(Msg))},
	 {"has timestamp", ?_assertMatch(Ts when is_number(Ts), graylog_gelf:get_timestamp(Msg))}
	].

new_1_invalid_test_() ->
	A = fun(V) -> ?_assertException(throw, {invalid, {version, _}}, graylog_gelf:new(V)) end,
	[
	 {"<<\"0.1\">>", A(<<"0.1">>)},
	 {"list", A("0.1")},
	 {"integer", A(1)}
	].


%% Test from_list/2
%% ====================================================================
from_list_2_test_() ->
	F = fun graylog_gelf:from_list/2,
	Ts = 1393240854.00053,
	D = [
		   {timestamp, Ts},
		   {level, 8},
		   {short_message, <<"Nthr shrt msg">>},
		   {full_message, <<"Another full message">>},
		   {'_test_field', <<"Additional">>}
		  ],
	R = F(D, []),
	A = fun (K) -> ?_assertEqual(?GV(K, D), ?GV(K, R)) end,
	[
	 {"has timestamp", ?_assertMatch(X when is_number(X), ?GV(timestamp, F([], [])))},
	 {"has version", ?_assertEqual(<<"1.1">>, ?GV(version, F([], [])))},
	 {"timestamp preseved", A(timestamp)},
	 {"level", A(level)},
	 {"short_message", A(short_message)},
	 {"full_message", A(full_message)},
	 {"_test_field", A('_test_field')},
	 {"invalid version", ?_assertException(throw, {invalid, {version, _}}, F(D, [{version, 0.5}]))}
	].
	

%% Test from_list/2 auto-generate short_message
%% ====================================================================
from_list_2_auto_generate_short_message_test_() ->
	F = fun graylog_gelf:from_list/2,
	Ts = 1393240854.00053,
	D = [
		   {timestamp, Ts},
		   {level, 8},
		   {full_message, <<"Another full message">>},
		   {'_test_field', <<"Additional">>}
		  ],
	R = F(D, [{short_message_length, 10}]),
	A = fun (K) -> ?_assertEqual(?GV(K, D), ?GV(K, R)) end,
	[
	 {"has timestamp", ?_assertMatch(X when is_number(X), ?GV(timestamp, F([], [])))},
	 {"has version", ?_assertEqual(<<"1.1">>, ?GV(version, F([], [])))},
	 {"timestamp preseved", A(timestamp)},
	 {"level", A(level)},
	 {"short_message", ?_assertEqual(<<"Another fu">>, ?GV(short_message, R))},
	 {"full_message", A(full_message)},
	 {"_test_field", A('_test_field')},
	 {"invalid version", ?_assertException(throw, {invalid, {version, _}}, F(D, [{version, 0.5}]))}
	].

%% Test set_host/2 and get_host/1
%% ====================================================================
set_host_2_get_host_1_test_() ->
	Msg = graylog_gelf:new(),
	Msg2 = graylog_gelf:set_host(<<"example.com">>, Msg),
	[
	 {"undefined at first", ?_assertEqual(undefined, graylog_gelf:get_host(Msg))},
	 {"example.com", ?_assertEqual(<<"example.com">>, graylog_gelf:get_host(Msg2))}
	].

%% Test set_short_message/2, get_short_message/2
%% ====================================================================
set_short_message_2_get_short_message_1_test_() ->
	Msg = graylog_gelf:new(),
	Msg2 = graylog_gelf:set_short_message(<<"A shrt msg">>, Msg),
	[
	 {"undefined at first", ?_assertEqual(undefined, graylog_gelf:get_short_message(Msg))},
	 {"A shrt msg", ?_assertEqual(<<"A shrt msg">>, graylog_gelf:get_short_message(Msg2))}
	].

%% Test set_full_message/2, get_full_message/2
%% ====================================================================
set_full_message_2_get_full_message_1_test_() ->
	Msg = graylog_gelf:new(),
	Msg2 = graylog_gelf:set_full_message(<<"A full message">>, Msg),
	[
	 {"undefined at first", ?_assertEqual(undefined, graylog_gelf:get_full_message(Msg))},
	 {"A full message", ?_assertEqual(<<"A full message">>, graylog_gelf:get_full_message(Msg2))}
	].

%% Test set_level/2, get_level/2
%% ====================================================================
set_level_2_get_level_1_test_() ->
	Msg = graylog_gelf:new(),
	Msg2 = graylog_gelf:set_level(4, Msg),
	[
	 {"undefined at first", ?_assertEqual(undefined, graylog_gelf:get_level(Msg))},
	 {"A shrt msg", ?_assertEqual(4, graylog_gelf:get_level(Msg2))}
	].

set_level_2_invalid_test_() ->
	Msg = graylog_gelf:new(),
	[
	 {"Binary", ?_assertError(function_clause, graylog_gelf:set_level(<<"1">>, Msg))},
	 {"Double", ?_assertError(function_clause, graylog_gelf:set_level(1.0, Msg))},
	 {"List", ?_assertError(function_clause, graylog_gelf:set_level("4", Msg))}
	].

%% Test set_additional/3, get_additional/2
%% ====================================================================
set_additional_3_invalid_test_() ->
	Msg = graylog_gelf:new(),
	[
	 {"'_id'", ?_assertException(throw, "_id must not be used by client", graylog_gelf:set_additional('_id', <<"test">>, Msg))},
	 {"\"_id\"", ?_assertException(throw, "_id must not be used by client", graylog_gelf:set_additional("_id", <<"test">>, Msg))},
	 {"<<\"_id\">>", ?_assertException(throw, "_id must not be used by client", graylog_gelf:set_additional(<<"_id">>, <<"test">>, Msg))},
	 {"not underscore prefixed binary", ?_assertException(throw, {invalid, {key, _}, prefix}, graylog_gelf:set_additional(<<"additional">>, <<"test">>, Msg))},
	 {"not underscore prefixed list", ?_assertException(throw, {invalid, {key, _}, prefix}, graylog_gelf:set_additional("additional", <<"test">>, Msg))},
	 {"invalid value type atom", ?_assertError(function_clause, graylog_gelf:set_additional("_t", test, Msg))},
	 {"invalid value type tuple", ?_assertError(function_clause, graylog_gelf:set_additional("_t", {<<"1">>, <<"2">>}, Msg))}
	].

set_additional_3_get_additional_2_test_() ->
	Msg = graylog_gelf:new(),
	[
	 {"number 10", ?_assertEqual(10,
								 graylog_gelf:get_additional(
								   '_t', graylog_gelf:set_additional('_t', 10, Msg)))},
	 {"number 4.2", ?_assertEqual(4.2,
								  graylog_gelf:get_additional(
									"_x", graylog_gelf:set_additional("_x", 4.2, Msg)))},
	 {"list", ?_assertEqual("test",
							graylog_gelf:get_additional(
							  <<"_y">>, graylog_gelf:set_additional(<<"_y">>, "test", Msg)))},
	 {"binary", ?_assertEqual(<<"test">>,
							  graylog_gelf:get_additional(
								<<"_z">>, graylog_gelf:set_additional(<<"_z">>, <<"test">>, Msg)))}
	].


%% Test to_sendable/2
%% ====================================================================
to_sendable_1_test_() ->
	Msg0 = graylog_gelf:new(),
	MsgFull = [{host, <<"example.com">>}, {short_message, <<"A shrt π msg"/utf8>>},
			   {full_message, <<"A full message">>}, {level, 2}, {'_a', <<"test">>} | Msg0],
	F = fun graylog_gelf:to_sendable/2,
	[
	 {"ok gzip", ?_assertMatch(B when is_binary(B), F(MsgFull, gzip))},
	 {"ok zlib", ?_assertMatch(B when is_binary(B), F(MsgFull, zlib))},
	 {"ok uncompressed", ?_assertMatch(B when is_binary(B), F(MsgFull, none))},
	 {"invalid gelf", ?_assertException(throw, {invalid, {version, _}}, F([{version, <<"0.5">>}], gzip))}
	].


%% ====================================================================
%% Tests for internal functions
%% ====================================================================

%% Test valid_version/1
%% ====================================================================
valid_version_1_test_() ->
	F = fun graylog_gelf:valid_version/1,
	A = fun(V) -> ?_assertEqual(<<"1.1">>, F(V)) end,
	AE = fun(V) -> ?_assertException(throw, {invalid, {version, _}}, F(V)) end,
	[
	 {"<<\"1.1\">>", A(<<"1.1">>)},
	 {"\"1.1\"", A("1.1")},
	 {"'1.1'", A('1.1')},
	 {"1.1", A(1.1)},
	 {"invalid {1,1}", AE({1,1})},
	 {"invalid 0.5", AE(0.5)}
	].


%% Test valid_key/1
%% ====================================================================
valid_key_1_test_() ->
	F = fun graylog_gelf:valid_key/1,
	[
	 {"'some'", ?_assertEqual(some, F(some))},
	 {"\"some\"", ?_assertEqual(<<"some">>, F("some"))},
	 {"utf8", ?_assertEqual(<<"åß∂"/utf8>>, F("åß∂"))},
	 {"<<\"some\">>", ?_assertEqual(<<"some">>, F(<<"some">>))},
	 {"123", ?_assertException(throw, {invalid, {key, _}}, F(123))}
	].


%% Test valid_timestamp/1
%% ====================================================================
valid_timestamp_1_test_() ->
	F = fun graylog_gelf:valid_timestamp/1,
	T = protofy_time:now_timestamp_micros() / 1000000,
	T0 = trunc(T), 
	AE = fun(X) -> ?_assertException(throw, {invalid, {timestamp, _}}, F(X)) end,
	[
	 {"ok now decimal", ?_assertEqual(T, F(T))},
	 {"ok now integer", ?_assertEqual(T0, F(T0))},
	 {"invalid negative", AE(T * (-1))},
	 {"invalid binary", AE(<<"123">>)},
	 {"invalid list", AE("123")}
	].


%% Test valid_level/1
%% ====================================================================
valid_level_1_test_() ->
	F = fun graylog_gelf:valid_level/1,
	Ok = fun(X) -> ?_assertEqual(X, F(X)) end,
	Invalid = fun(X) -> ?_assertException(throw, {invalid, {level, _}}, F(X)) end,
	[
	 {"ok 0", Ok(0)},
	 {"ok 1", Ok(1)},
	 {"ok 1000", Ok(1000)},
	 {"invalid -1", Invalid(-1)},
	 {"invalid \"1\"", Invalid("1")},
	 {"invalid <<\"10\">>", Invalid(<<"10">>)},
	 {"invalid 1.1", Invalid(1.1)}
	].


%% Test valid_string/1
%% ====================================================================
valid_string_test_() ->
	F = fun graylog_gelf:valid_string/1,
	[
	 {"ok \"abc\"", ?_assertEqual(<<"abc">>, F("abc"))},
	 {"ok <<\"abc\">>", ?_assertEqual(<<"abc">>, F(<<"abc">>))},
	 {"ok utf8", ?_assertEqual(<<"åß∂"/utf8>>, F(<<"åß∂"/utf8>>))},
	 {"invalid 'abc'", ?_assertException(throw,{invalid, {string_field, _}}, F('abc'))},
	 {"invalid 123", ?_assertException(throw, {invalid, {string_field, _}}, F(123))}
	].
	 

%% Test now_gelf_micros/0
%% ====================================================================
now_gelf_micros_0_test_() ->
	?_assertMatch(X when is_number(X), graylog_gelf:now_gelf_micros()).


%% Test throw_if_not_additional_key/1
%% ====================================================================
throw_if_not_additional_key_1_test_() ->
	F = fun graylog_gelf:throw_if_not_additional_key/1,
	Reserved = fun(X) ->
					   ?_assertException(throw, {invalid, {key, _}, reserved}, F(X))
			   end,
	Prefix = fun(X) ->
					 ?_assertException(throw, {invalid, {key, _}, prefix}, F(X))
			 end,
	[
	 {"ok _... binary", ?_assertEqual(ok, F(<<"_test">>))},
	 {"ok _... list", ?_assertEqual(ok, F("_test"))},
	 {"ok _... atom", ?_assertEqual(ok, F('_test'))},
	 {"throw ... binary", Prefix(<<"test">>)},
	 {"throw ... list", Prefix("test")},
	 {"throw ... atom", Prefix(test)},
	 {"throw integer", Prefix(1)},
	 {"throw '_id'", Reserved('_id')},
	 {"throw '_ttl'", Reserved('_ttl')},
	 {"throw '_source'", Reserved('_source')},
	 {"throw '_all'", Reserved('_all')},
	 {"throw '_index'", Reserved('_index')},
	 {"throw '_type'", Reserved('_type')},
	 {"throw '_score'", Reserved('_score')},
	 {"throw <<\"_id\">>", Reserved(<<"_id">>)},
	 {"throw <<\"_ttl\">>", Reserved(<<"_ttl">>)},
	 {"throw <<\"_source\">>", Reserved(<<"_source">>)},
	 {"throw <<\"_all\">>", Reserved(<<"_all">>)},
	 {"throw <<\"_index\">>", Reserved(<<"_index">>)},
	 {"throw <<\"_type\">>", Reserved(<<"_type">>)},
	 {"throw <<\"_score\">>", Reserved(<<"_score">>)},
	 {"throw \"_id\"", Reserved("_id")},
	 {"throw \"_ttl\"", Reserved("_ttl")},
	 {"throw \"_source\"", Reserved("_source")},
	 {"throw \"_all\"", Reserved("_all")},
	 {"throw \"_index\"", Reserved("_index")},
	 {"throw \"_type\"", Reserved("_type")},
	 {"throw \"_score\"", Reserved("_score")}
	].


%% Test throw_if_not_present/2
%% ====================================================================
throw_if_not_present_2_test_() ->
	F = fun graylog_gelf:throw_if_not_present/2,
	Msg0 = graylog_gelf:new(),
	Msg1 = graylog_gelf:set_additional('_test', <<"test">>, Msg0),
	Msg2 = graylog_gelf:set_host(<<"example.com">>, Msg1),
	[
	 {"host present", ?_assertEqual(ok, F(host, Msg2))},
	 {"_test present", ?_assertEqual(ok, F('_test', Msg2))},
	 {"level missing", ?_assertException(throw, {missing, {level, _}}, F(level, Msg2))}
	].


%% Test set_number_field/3
%% ====================================================================
set_number_field_3_test_() ->
	Msg = graylog_gelf:new(),
	F = fun graylog_gelf:set_number_field/3,
	[
	 {"10 ok", ?_assertEqual(10, ?GV(test, F(test, 10, Msg)))},
	 {"10.1 ok", ?_assertEqual(10.1, ?GV(test, F(test, 10.1, Msg)))},
	 {"invalid binary", ?_assertError(function_clause, F(test, <<"test">>, Msg))},
	 {"invalid list", ?_assertError(function_clause, F(test, "test", Msg))},
	 {"invalid atom", ?_assertError(function_clause, F(test, test, Msg))}
	].


%% Test set_string_field/3
%% ====================================================================
set_string_field_3_test_() ->
	Msg = graylog_gelf:new(),
	F = fun graylog_gelf:set_string_field/3,
	[
	 {"ok binary", ?_assertEqual(<<"test">>, ?GV(test, F(test, <<"test">>, Msg)))},
	 {"ok list", ?_assertEqual("test", ?GV(test, F(test, "test", Msg)))},
	 {"invalid atom", ?_assertError(function_clause, F(test, test, Msg))},
	 {"invalid 10", ?_assertError(function_clause, F(test, 10, Msg))},
	 {"invalid 10.1", ?_assertError(function_clause, F(test, 10.1, Msg))}
	].


%% Test set_field/3
%% ====================================================================
set_field_3_test_() ->
	Msg = graylog_gelf:new(),
	F = fun graylog_gelf:set_field/3,
	[
	 {"ok binary key", ?_assertEqual(<<"value">>, ?GV(<<"test">>, F(<<"test">>, <<"value">>, Msg)))},
	 {"ok list key", ?_assertEqual(<<"value">>, ?GV("test", F("test", <<"value">>, Msg)))},
	 {"ok atom key", ?_assertEqual(<<"value">>, ?GV(test, F(test, <<"value">>, Msg)))},
	 {"invalid integer", ?_assertError(function_clause, F(10, <<"value">>, Msg))}
	].


%% Test to_sendable_1_1/2
%% ====================================================================
to_sendable_1_1_2_test_() ->
	F = fun graylog_gelf:to_sendable_1_1/2,
	Msg0 = graylog_gelf:new(),
	MsgFull = [{host, <<"example.com">>}, {short_message, <<"A shrt msg">>},
			   {full_message, <<"A full message">>}, {level, 2}, {<<"_a">>, <<"test">>} | Msg0],
	No = fun(Key) -> proplists:delete(Key, MsgFull) end,
	TestNotPresent = fun(Key) -> {"Not present " ++ lists:flatten(io_lib:format("~p", [Key])),
								  ?_assertException(throw, {missing, {Key, _}}, F(No(Key), gzip))} end,
	[
	 TestNotPresent(host),
	 TestNotPresent(short_message),
	 TestNotPresent(full_message),
	 TestNotPresent(timestamp),
	 {"binary", ?_assertMatch(Bin when is_binary(Bin), F(MsgFull, gzip))},
	 {"gunzip", ?_assertMatch(Bin when is_binary(Bin), zlib:gunzip(F(MsgFull, gzip)))},
	 {"json", ?_assertEqual(jiffy:encode({MsgFull}),
							zlib:gunzip(F(MsgFull, gzip)))}
	].

to_sendable_1_1_2_reversed_test_() ->
	F = fun graylog_gelf:to_sendable_1_1/2,
	ShrtMsg = <<"A π shrt msg"/utf8>>,
	FllMsg = <<"A full ∑ message"/utf8>>,
	Add = <<"test å test"/utf8>>,
	Msg0 = graylog_gelf:new(),
	MsgFull = [{host, <<"example.com">>}, {short_message, ShrtMsg},
			   {full_message, FllMsg}, {level, 2}, {<<"_a">>, Add} | Msg0],
	{Rev} = jiffy:decode(zlib:gunzip(F(MsgFull, gzip))),
	[
	 {"host", ?_assertEqual(<<"example.com">>, ?GV(<<"host">>, Rev))},
	 {"level", ?_assertEqual(2, ?GV(<<"level">>, Rev))},
	 {"short_message", ?_assertEqual(ShrtMsg, ?GV(<<"short_message">>, Rev))},
	 {"full_message", ?_assertEqual(FllMsg, ?GV(<<"full_message">>, Rev))},
	 {"timestamp", ?_assertNotEqual(undefined, ?GV(<<"timestamp">>, Rev))},
	 {"additional", ?_assertEqual(Add, ?GV(<<"_a">>, Rev))}
	].


%% Test compress/2
%% ====================================================================
compress_2_test_() ->
	F = fun graylog_gelf:compress/2,
	[
	 {"gzip", ?_assertEqual(zlib:gzip(<<"gzip test">>), F(<<"gzip test">>, gzip))},
	 {"zlib", ?_assertEqual(zlib:compress(<<"zlib test">>), F(<<"zlib test">>, zlib))},
	 {"uncompressed binary", ?_assertEqual(<<"uncompressed binary ∑∑∑ test"/utf8>>, F(<<"uncompressed binary ∑∑∑ test"/utf8>>, none))},
	 {"uncompressed list", ?_assertEqual(<<"uncompressed list ππ∑ test"/utf8>>, F("uncompressed list ππ∑ test", none))}
	].


%% Test do_from_list/2
%% ====================================================================
do_from_list_2_test_() ->
	F = fun graylog_gelf:do_from_list/2,
	Ts = 1393240854.00053,
	D = [
		   {timestamp, Ts},
		   {level, 8},
		   {short_message, <<"Nthr shrt msg">>},
		   {full_message, <<"Another full message">>},
		   {'_test_field', <<"Additional">>}
		  ],
	R = F(D, []),
	A = fun (K) -> ?_assertEqual(?GV(K, D), ?GV(K, R)) end,
	[
	 {"has timestamp", ?_assertMatch([{timestamp, X}] when is_number(X), F([], []))},
	 {"timestamp preseved", A(timestamp)},
	 {"level", A(level)},
	 {"short_message", A(short_message)},
	 {"full_message", A(full_message)},
	 {"_test_field", A('_test_field')},
	 {"previous list preseved", ?_assertEqual(exists, ?GV(before, F(D, [{before, exists}])))}
	].


%% Test from_list_field/3
%% ====================================================================
from_list_field_3_test_() ->
	A = fun(K, V) -> R = graylog_gelf:from_list_field(K, V, [{'_dummy', 123}]),
					 ?_assertEqual(V, ?GV(K, R)) end,
	[
	 {"timestamp ok", A(timestamp, 1393240854.00053)},
	 {"level ok", A(level, 8)},
	 {"short_message", A(short_message, <<"Nthr shrt msg µ∑π"/utf8>>)},
	 {"full_message", A(full_message, <<"Another full message µ∑π"/utf8>>)},
	 {"version", A(version, <<"1.1">>)},
	 {"host", A(host, <<"127.0.0.1">>)},
	 {"additional", A('_test_field', <<"Additional µ∑π"/utf8>>)}
	].


%% Test add_additional/3
%% ====================================================================
add_additional_test_() ->
	F = fun graylog_gelf:add_additional/3,
	Ok = fun(K, V, E) -> ?_assertEqual(E, ?GV(K, F(K, V, [{host, "test"}]))) end, 
	[
	 {"ok '_abc',\"bcd\"", Ok('_abc', "bcd", <<"bcd">>)},
	 {"ok '_abc',<<\"bcd\">>", Ok('_abc', <<"bcd">>, <<"bcd">>)},
	 {"ok '_abc',1", Ok('_abc', 1, 1)},
	 {"ok \"_abc\",\"bcd\"", Ok("_abc", "bcd", <<"bcd">>)},
	 {"ok \"_abc\",<<\"bcd\">>", Ok("_abc", <<"bcd">>, <<"bcd">>)},
	 {"ok \"_abc\",1", Ok("_abc", 3, 3)},
	 {"ok <<\"_abc\">>,2", Ok(<<"_abc">>, 2, 2)},
	 {"ok <<\"_abc\">>,\"bcd\"", Ok(<<"_abc">>, "bcd", <<"bcd">>)},
	 {"ok <<\"_abc\">>,<<\"bcd\">>", Ok(<<"_abc">>, <<"bcd">>, <<"bcd">>)},
	 {"ok uft8,<<\"bcd\">>", Ok(<<"_µ∑π"/utf8>>, <<"∑∑∑"/utf8>>, <<"∑∑∑"/utf8>>)}
	].


%% Test to_list/1
%% ====================================================================
to_list_test_() ->
	Msg = graylog_gelf:from_list(
			[{host, <<"example.com">>}, {short_message, <<"A shrt msg">>},
			 {full_message, <<"A full message">>}, {level, 2}, {<<"_a">>, <<"test">>}]),
	List = graylog_gelf:to_list(Msg),
	T = fun(K, V) -> ?_assertEqual(V, ?GV(K, List)) end,
	[
	 {"version", T(version, <<"1.1">>)},
	 {"host", T(host, <<"example.com">>)},
	 {"short_message", T(short_message, <<"A shrt msg">>)},
	 {"full_message", T(full_message, <<"A full message">>)},
	 {"level", T(level, 2)},
	 {"<<\"_a\">>", T(<<"_a">>, <<"test">>)}
	].
	 

shorten_test_() ->
	F = fun graylog_gelf:shorten/2,
	U = fun(X) -> unicode:characters_to_binary(X, utf8, utf8) end,
	[
	 {"binary", ?_assertEqual(<<"abc">>, F(<<"abcdef">>, 3))},
	 {"binary utf8", ?_assertEqual(<<"µµ"/utf8>>, F(<<"µµµ"/utf8>>, 2))},
	 {"binary utf8", ?_assertEqual(<<"µ∑"/utf8>>, F(<<"µ∑π"/utf8>>, 2))},
	 {"list", ?_assertEqual(<<"abcd">>, F("abcd", 4))},
	 {"list utf8", ?_assertEqual(<<"µµ"/utf8>>, F("µµµ", 2))},
	 {"list utf8", ?_assertEqual(U("µ∑"), F("µ∑µ", 2))},
	 {"atom", ?_assertEqual(<<"ab">>, F(ab, 2))},
	 {"tuple", ?_assertEqual(<<"{1,">>, F({1,2}, 3))}
	].