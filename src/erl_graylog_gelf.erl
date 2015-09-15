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
%% @doc GELF message formatter using version 1.1 of GELF (http://graylog2.org/gelf#specs)
%%
%% == Create a message ==
%% 
%% - from_list(List) or from_list(List, Opts) with Opts:
%%
%%   - short_message_length: Used to auto-generate a short_message from full_message if no short_message is given. Defaults to ?DEFAULT_SHORT_MESSAGE_LENGTH (80)
%%  
%% - new/0, set_*/2 and set_additional/3:
%%
%% == Convert to sendable ==
%%
%% to_sendable(Msg, Compression) with Compression being none, gzip or zlib.
%% 
%% NOTE: You should not use compression if you send the messages to  GELF TCP input. You might have zero-bytes in your compressed message and GELF TCP input uses \0 as message separator.
%%

-module(erl_graylog_gelf).

-include_lib("protofy_common/include/protofy_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0,
		 from_list/1, from_list/2,
		 to_list/1,
		 get_version/1,
		 get_timestamp/1,
		 set_host/2,
		 get_host/1,
		 set_short_message/2,
		 get_short_message/1,
		 set_full_message/2,
		 get_full_message/1,
		 set_level/2,
		 get_level/1,
		 set_additional/3,
		 get_additional/2,
		 to_sendable/2]).

-define(CURRENT_GELF_VER, <<"1.1">>).
-define(DEFAULT_SHORT_MESSAGE_LENGTH, 80).

-export_type([msg/0, compression/0]).
-type msg() :: list().
-type compression() :: none | gzip | zlib.

-type version() :: binary() | list() | number() | atom().

%% new/0
%% ====================================================================
%% @doc Message having only version and timestamp
-spec new() -> msg().
%% ====================================================================
new() ->
	new(?CURRENT_GELF_VER).


%% from_list/1
%% ====================================================================
%% @doc Message from list
-spec from_list(list()) -> msg().
%% ====================================================================
from_list(List) ->
	from_list(List, []).


%% from_list/2
%% ====================================================================
%% @doc Create GELF message from list.
%%
%% This will not check for duplicates
-spec from_list(version(), [Opt]) -> msg() when
	Opt :: {version, version()} %% currently only 1.1
		 | {short_message_length, non_neg_integer()}.
%% ====================================================================
from_list(List, Opts) ->
	Version = valid_version(?GV(version, Opts, ?CURRENT_GELF_VER)),
	Add = case ?GV(short_message, List) of
			  undefined -> [{version, Version},
							{short_message, shorten(?GV(full_message, List),
													?GV(short_message_length, Opts, ?DEFAULT_SHORT_MESSAGE_LENGTH))}];
			  _ -> [{version, Version}]
		  end,
	do_from_list(List, Add).


%% to_list/1
%% ====================================================================
%% @doc Message to list
-spec to_list(msg()) -> list().
%% ====================================================================
to_list(Msg) ->
	Msg.


%% get_version/1
%% ====================================================================
%% @doc Get version of message
-spec get_version(msg()) -> binary() | undefined.
%% ====================================================================
get_version(Msg) ->
	?GV(version, Msg).


%% get_timestamp/1
%% ====================================================================
%% @doc Get timestamp of message
-spec get_timestamp(msg()) -> number() | undefined.
%% ====================================================================
get_timestamp(Msg) ->
	?GV(timestamp, Msg).


%% set_host/2
%% ====================================================================
%% @doc Set host field of message
-spec set_host(Host :: binary() | list(), msg()) -> msg().
%% ====================================================================
set_host(Host, Msg)  ->
	set_string_field(host, Host, Msg).


%% get_host/1
%% ====================================================================
%% @doc Get host field of message
-spec get_host(msg()) -> binary() | list().
%% ====================================================================
get_host(Msg) ->
	?GV(host, Msg).


%% set_short_message/2
%% ====================================================================
%% @doc Set short message field of message
-spec set_short_message(ShortMessage :: binary() | list(), msg()) -> msg().
%% ====================================================================
set_short_message(ShortMessage, Msg) ->
	set_string_field(short_message, ShortMessage, Msg).


%% get_short_message/1
%% ====================================================================
%% @doc Get short message field of message
-spec get_short_message(msg()) -> binary() | list().
%% ====================================================================
get_short_message(Msg) ->
	?GV(short_message, Msg).


%% set_full_message/2
%% ====================================================================
%% @doc Set full message field of message
-spec set_full_message(FullMessage :: binary() | list(), msg()) -> msg().
%% ====================================================================
set_full_message(FullMessage, Msg) ->
	set_string_field(full_message, FullMessage, Msg).


%% get_full_message/1
%% ====================================================================
%% @doc Get full message field of message
-spec get_full_message(msg()) -> binary() | list().
%% ====================================================================
get_full_message(Msg) ->
	?GV(full_message, Msg).


%% set_level/2
%% ====================================================================
%% @doc Set level field of message
-spec set_level(Level :: integer(), msg()) -> msg().
%% ====================================================================
set_level(Level, Msg) when is_integer(Level) ->
	set_number_field(level, Level, Msg).


%% get_level/1
%% ====================================================================
%% @doc Get level field of message
-spec get_level(msg()) -> integer().
%% ====================================================================
get_level(Msg) ->
	?GV(level, Msg).


%% set_additional/3
%% ====================================================================
%% @doc Set additional field of message
%%
%% Keys must begin with _ (underscore) and key must not be _id
-spec set_additional(Key :: atom() | binary() | list(), Value :: term(), msg()) -> msg().
%% ====================================================================
set_additional('_id', _Value, _Msg) ->
	throw("_id must not be used by client");
set_additional(<<"_id">>, _Value, _Msg) ->
	throw("_id must not be used by client");
set_additional("_id", _Value, _Msg) ->
	throw("_id must not be used by client");
set_additional(Key, Value, Msg) when is_binary(Value) or is_list(Value) ->
	throw_if_not_additional_key(Key),
	set_string_field(Key, Value, Msg);
set_additional(Key, Value, Msg) when is_number(Value) ->
	throw_if_not_additional_key(Key),
	set_number_field(Key, Value, Msg).
	

%% get_additional/2
%% ====================================================================
%% @doc Get additional field of message
-spec get_additional(Key :: atom() | binary() | list(), msg()) -> msg().
%% ====================================================================
get_additional(Key, Msg) when is_list(Key) or is_binary(Key) or is_atom(Key) ->
	?GV(Key, Msg).


%% to_sendable/2
%% ====================================================================
%% @doc Convert to sendable GELF message
-spec to_sendable(msg(), compression()) -> term().
%% ====================================================================
to_sendable(Msg, Compression) ->
	case ?GV(version, Msg) of
		<<"1.1">> -> to_sendable_1_1(Msg, Compression);
		Ver -> throw({invalid, {version, Ver}})
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% new/1
%% ====================================================================
%% @doc Message having given version an timestamp.
%%
%% private for now until we support other versions.
-spec new(version()) -> msg().
%% ====================================================================
new(Ver) ->
	[{version, valid_version(Ver)}, {timestamp, now_gelf_micros()}].


%% valid_version/1
%% ====================================================================
%% @doc Return valid version. Throws if not valid
-spec valid_version(term()) -> binary().
%% ====================================================================
valid_version(<<"1.1">>) -> <<"1.1">>;
valid_version("1.1") -> <<"1.1">>;
valid_version('1.1') -> <<"1.1">>;
valid_version(1.1) -> <<"1.1">>;
valid_version(Ver) -> throw({invalid, {version, Ver}}).


%% valid_level/1
%% ====================================================================
%% @doc Return valid level. Throws if not valid
-spec valid_level(term()) -> integer().
%% ====================================================================
valid_level(Level) when is_integer(Level), Level >= 0 ->	Level;
valid_level(Level) ->										throw({invalid, {level, Level}}).


%% valid_string/1
%% ====================================================================
%% @doc Return valid string field. Throws if not valid
-spec valid_string(term()) -> binary().
%% ====================================================================
valid_string(String) when is_list(String) ->	unicode:characters_to_binary(String, utf8, utf8);
valid_string(Bin) when is_binary(Bin) ->		Bin;
valid_string(Term) ->							throw({invalid, {string_field, Term}}).


%% valid_key/1
%% ====================================================================
%% @doc Return valid key. Throws if not valid
-spec valid_key(term()) -> binary().
%% ====================================================================
valid_key(K) when is_list(K) ->					unicode:characters_to_binary(K, utf8, utf8);
valid_key(K) when is_binary(K) or is_atom(K) ->	K;
valid_key(K) ->									throw({invalid, {key, K}}).


%% valid_timestamp/1
%% ====================================================================
%% @doc Return valid timestamp. Throws if not valid
-spec valid_timestamp(term()) -> number().
%% ====================================================================
valid_timestamp(T) when is_number(T), T >= 0 -> T;
valid_timestamp(T) -> throw({invalid, {timestamp, T}}).


%% now_gelf_micros/0
%% ====================================================================
%% @doc Return now timestamp in GELF microseconds (secs.microsecs)
-spec now_gelf_micros() -> number().
%% ====================================================================
now_gelf_micros() ->
	protofy_time:now_timestamp_micros() / 1000000.


%% throw_if_not_additional_key/1
%% ====================================================================
%% @doc Throw if not valid additional key.
-spec throw_if_not_additional_key(term()) -> ok.
%% ====================================================================
throw_if_not_additional_key('_id') ->					throw({invalid, {key, '_id'}, reserved});
throw_if_not_additional_key('_ttl') ->					throw({invalid, {key, '_ttl'}, reserved});
throw_if_not_additional_key('_source') ->				throw({invalid, {key, '_source'}, reserved});
throw_if_not_additional_key('_all') ->					throw({invalid, {key, '_all'}, reserved});
throw_if_not_additional_key('_index') ->				throw({invalid, {key, '_index'}, reserved});
throw_if_not_additional_key('_type') ->					throw({invalid, {key, '_type'}, reserved});
throw_if_not_additional_key('_score') ->				throw({invalid, {key, '_score'}, reserved});
throw_if_not_additional_key(Key) when is_atom(Key) ->	throw_if_not_additional_key(atom_to_list(Key));
throw_if_not_additional_key(<<"_id">>) ->				throw({invalid, {key, <<"_id">>}, reserved});
throw_if_not_additional_key(<<"_ttl">>) ->				throw({invalid, {key, <<"_ttl">>}, reserved});
throw_if_not_additional_key(<<"_source">>) ->			throw({invalid, {key, <<"_source">>}, reserved});
throw_if_not_additional_key(<<"_all">>) ->				throw({invalid, {key, <<"_all">>}, reserved});
throw_if_not_additional_key(<<"_index">>) ->			throw({invalid, {key, <<"_index">>}, reserved});
throw_if_not_additional_key(<<"_type">>) ->				throw({invalid, {key, <<"_type">>}, reserved});
throw_if_not_additional_key(<<"_score">>) ->			throw({invalid, {key, <<"_score">>}, reserved});
throw_if_not_additional_key(<<$_,_/binary>>) ->			ok;
throw_if_not_additional_key("_id") ->					throw({invalid, {key, "_id"}, reserved});
throw_if_not_additional_key("_ttl") ->					throw({invalid, {key, "_ttl"}, reserved});
throw_if_not_additional_key("_source") ->				throw({invalid, {key, "_source"}, reserved});
throw_if_not_additional_key("_all") ->					throw({invalid, {key, "_all"}, reserved});
throw_if_not_additional_key("_index") ->				throw({invalid, {key, "_index"}, reserved});
throw_if_not_additional_key("_type") ->					throw({invalid, {key, "_type"}, reserved});
throw_if_not_additional_key("_score") ->				throw({invalid, {key, "_score"}, reserved});
throw_if_not_additional_key([$_|_]) ->					ok;
throw_if_not_additional_key(Key) ->						throw({invalid, {key, Key}, prefix}).


%% throw_if_not_present/2
%% ====================================================================
%% @doc Throw if Key is not present in Msg
-spec throw_if_not_present(Key :: atom() | binary() | list(), msg()) -> ok.
%% ====================================================================
throw_if_not_present(Key, Msg) ->
	case ?GV(Key, Msg) of
		undefined -> throw({missing, {Key, Msg}});
		_ -> ok
	end.


%% set_number_field/3
%% ====================================================================
%% @doc Set numerical field Key of Msg to Value
-spec set_number_field(Key :: atom() | binary() | list(), Value :: number(), msg()) -> msg().
%% ====================================================================
set_number_field(Key, Value, Msg) when is_number(Value) ->
	set_field(Key, Value, Msg).

%% set_string_field/3
%% ====================================================================
%% @doc Set string field Key of Msg to Value
-spec set_string_field(Key :: atom() | binary() | list(), Value :: binary() | list(), msg()) -> msg().
%% ====================================================================
set_string_field(Key, Value, Msg) when is_list(Value) or is_binary(Value) ->
	set_field(Key, Value, Msg).


%% set_field/3
%% ====================================================================
%% @doc Set field Key of Msg to Value
-spec set_field(Key :: atom() | binary() | list(), Value :: term(), msg()) -> msg().
%% ====================================================================
set_field(Key, Value, Msg) when is_list(Key) ->
	try list_to_existing_atom(Key) of
		AtomKey ->
			[{Key, Value} | [P || {K, _}=P <- Msg, (K /= AtomKey) and (K /= Key)]]
	catch
		error:badarg ->
			[{Key, Value} | proplists:delete(Key, Msg)]
	end;
set_field(Key, Value, Msg) when is_atom(Key) ->
	BinKey = atom_to_binary(Key, utf8),
	[{Key, Value} | [P || {K, _}=P <- Msg, (K /= BinKey) and (K /= Key)]];
set_field(Key, Value, Msg) when is_binary(Key) ->
	try binary_to_existing_atom(Key, utf8) of
		AtomKey ->
			[{Key, Value} | [P || {K, _}=P <- Msg, (K /= AtomKey) and (K /= Key)]]
	catch
		error:badarg ->
			[{Key, Value} | proplists:delete(Key, Msg)]
	end.


%% to_sendable_1_1/2
%% ====================================================================
%% @doc Worker function for to_sendable/1 for version 1.1
-spec to_sendable_1_1(msg(), compression()) -> binary().
%% ====================================================================
to_sendable_1_1(Msg, Compression) ->
	throw_if_not_present(host, Msg),
	throw_if_not_present(short_message, Msg),
	throw_if_not_present(full_message, Msg),
	throw_if_not_present(timestamp, Msg),
	compress(jiffy:encode({Msg}, [force_utf8]), Compression).


%% compress/2
%% ====================================================================
%% @doc Compress encoded message
-spec compress(Encoded :: term(), compression()) -> binary().
%% ====================================================================
compress(Encoded, gzip) ->
	zlib:gzip(Encoded);
compress(Encoded, zlib) ->
	zlib:compress(Encoded);
compress(Encoded, none) when is_list(Encoded) ->
	unicode:characters_to_binary(Encoded, utf8, utf8);
compress(Encoded, none) when is_binary(Encoded) ->
	Encoded.


%% do_from_list/2
%% ====================================================================
%% @doc Worker function for from_list/2
-spec do_from_list(Input :: list(), A :: list()) -> msg(). 
%% ====================================================================
do_from_list([], A) ->
	case ?GV(timestamp, A) of
		undefined -> [{timestamp, now_gelf_micros()}|A];
		_ -> A
	end;
do_from_list([{K,V}|T], A) ->
	do_from_list(T, from_list_field(K, V, A)).


%% from_list_field/3
%% ====================================================================
%% @doc Create field Key with Value and prepend it to A
-spec from_list_field(Key :: atom() | binary() | list(), Value :: term(), A :: list()) -> list().
%% ====================================================================
from_list_field(Key, Value, A) ->
	case valid_key(Key) of
		timestamp -> [{timestamp, valid_timestamp(Value)} | A];
		level -> [{level, valid_level(Value)} | A];
		short_message -> [{short_message, valid_string(Value)} | A];
		full_message -> [{full_message, valid_string(Value)} | A];
		version -> [{version, valid_version(Value)}|A];
		host -> [{host, valid_string(Value)} | A];
		_ -> add_additional(Key, Value, A)
	end.


%% add_additional/3
%% ====================================================================
%% @doc Add additional field to message without checking for duplicates.
-spec add_additional(Key :: atom() | binary() | list(), Value :: term(), Msg :: msg()) -> msg().
%% ====================================================================
add_additional(Key, Value, Msg) when is_binary(Value) or is_list(Value) ->
	throw_if_not_additional_key(Key),
	[{Key, valid_string(Value)} | Msg];
add_additional(Key, Value, Msg) when is_number(Value) ->
	throw_if_not_additional_key(Key),
	[{Key, Value} | Msg].


%% shorten/2
%% ====================================================================
%% @doc Shorten Full message to a maximum Length
-spec shorten(Full :: binary() | list() | atom(), Length :: non_neg_integer()) -> binary().
%% ====================================================================
shorten(Full, Length) when is_binary(Full) ->
	shorten(unicode:characters_to_list(Full), Length);
shorten(Full, Length) when is_list(Full) ->
	unicode:characters_to_binary(string:sub_string(Full, 1, Length), utf8,  utf8);
shorten(Full, Length) when is_atom(Full) ->
	shorten(atom_to_list(Full), Length);
shorten(Full, Length) ->
	shorten(lists:flatten(io_lib:format("~p", [Full])), Length).


