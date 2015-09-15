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
%% @doc Graylog sender server supporting GELF TCP, GELF UDP, Raw TCP and Raw UDP inputs.
%%
%% == Start-up options ==
%% ```
%% - connection - mandatory
%%   - {type, udp | tcp} - mandatory
%%   - {addr, inet:ip_address() | inet:hostname()} - mandatory
%%   - {port, inet:port_number()} - mandatory
%%   - {ip_version, ipv4 | ipv6} - optional, default: ipv4
%%   - {socket_opts, [gen_tcp:option() | gen_udp:option()]} - optional, default: []
%%
%% - server_ref - optional, default: undefined
%%   - {server_ref, singleton} : Sender will be started in singleton mode. You can access it via stop/0, get_opt/1, set_opt/2, send/1. This is useful if you want only one sender and you want to be able to access it node-wide without having to know its name or pid.
%%   - no server_ref or {server_ref, undefined} : Sender will be started unnamed. Use stop/1, get_opt/2, set_opt/3, send/2 with its pid.
%%   - {server_ref, atom()} : Sender will be started in named mode having given atom as process name. Use stop/1, get_opt/2, set_opt/3, send/2 with its pid or name.
%%
%% - compression - optional, default: gzip
%%   - values : default | none | gzip | zlib
%%   - note : Don't use gzip or zlib together with TCP inputs. You might have zero-bytes in your compressed message and GELF TCP input uses \0 as message separator.
%%
%% - format - optional, default: gelf
%%   - values : default | raw | gelf. 
%%
%% - host - optional, default: hostname of this machine
%% 
%% You can change compression and format of a running sender by using set_opt/2, set_opt/3. 
%% '''
%%
%% == Sending ==
%% If you've started the sender as singleton use send/1, otherwise use send/2.
%% === Notes ===
%% - If you're using GELF, the host field will be set by the sender unless you add it to your message.
%% 
%% - If you're using GELF and a binary as message this binary will be set as value of the full_message field.
%%
%% == Usage example ==
%% ```
%% > Connection = [{type, udp}, {addr, {127,0,0,1}}, {port, 12202}].
%% > Opts = [{connection, Connection}, {compression, gzip}, {format, gelf}].
%% > {ok, Pid} = erl_graylog_sender:start_link(Opts).
%% > Msg = [{full_message, <<"Test Message">>}, {level, 1}].
%% > erl_graylog_sender:send(Msg).
%% '''


-module(erl_graylog_sender).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("protofy_common/include/protofy_common.hrl").

%% ====================================================================
%% Types
%% ====================================================================
-export_type([start_opt/0]).
-type start_opt() :: {server_ref, undefined | singleton | atom()}
  				   | {compression, default | compression()}
				   | {connection, [connection_opt()]}
				   | {format, default | format()}
				   | {host, binary()}.
-type connection_opt() :: {type, tcp | udp}
						| {addr, inet:ip_address() | inet:hostname()}
						| {port, inet:port_number()}
						| {ip_version, ipv4 | ipv6}
						| {socket_opts, [gen_tcp:option() | gen_udp:option()]}.
-type compression() :: none | gzip | zlib.
-type format() :: raw | gelf.
-type msg() :: term(). % @todo json term 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 stop/0, stop/1,
		 get_opt/1, get_opt/2,
		 set_opt/2, set_opt/3,
		 send/1, send/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_COMPRESSION, gzip).
-define(DEFAULT_FORMAT, gelf).

%% start_link/1
%% ====================================================================
%% @doc Start named, unnamed or singleton server.
-spec start_link([start_opt()]) -> {ok, pid()} | {error, reason()}.
%% ====================================================================
start_link(Opts) ->
	case ?GV(server_ref, Opts) of
		undefined -> gen_server:start_link(?MODULE, [Opts], []);
		singleton -> gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []);
		Ref -> gen_server:start_link({local, Ref}, ?MODULE, [Opts], [])
	end.


%% stop/0
%% ====================================================================
%% @doc Stop singleton server
-spec stop() -> ok.
%% ====================================================================
stop() ->
	stop(?SERVER).


%% stop/1
%% ====================================================================
%% @doc Stop server identified by Ref
-spec stop(server_ref()) -> ok.
%% ====================================================================
stop(Ref) ->
	gen_server:call(Ref, stop).


%% get_opt/1
%% ====================================================================
%% @doc Get option from running singleton server
-spec get_opt(key()) -> term().
%% ====================================================================
get_opt(Key) ->
	get_opt(?SERVER, Key).


%% get_opt/2
%% ====================================================================
%% @doc Get option from server identified by Ref
-spec get_opt(server_ref(), key()) -> term().
%% ====================================================================
get_opt(Ref, Key) ->
	gen_server:call(Ref, {get_opt, Key}).


%% set_opt/2
%% ====================================================================
%% @doc Set option of running singleton server
-spec set_opt(Key, term()) -> ok when
	Key :: compression
		 | format.
%% ====================================================================
set_opt(Key, Value) ->
	set_opt(?SERVER, Key, Value).


%% set_opt/3
%% ====================================================================
%% @doc Set option of running server identified by Ref
-spec set_opt(server_ref(), Key, term()) -> ok when
	Key :: compression
		 | format.
%% ====================================================================
set_opt(Ref, compression, Compression) ->
	gen_server:call(Ref, {set_opt, compression, valid_compression(Compression)});
set_opt(Ref, format, Format) ->
	gen_server:call(Ref, {set_opt, format, valid_format(Format)}).


%% send/1
%% ====================================================================
%% @doc Send message via singleton server
-spec send(msg()) -> ok.
%% ====================================================================
send(Msg) ->
	send(?SERVER, Msg).


%% send/2
%% ====================================================================
%% @doc Send message via server identified by Ref
-spec send(server_ref(), msg()) -> ok.
%% ====================================================================
send(Ref, Msg) ->
	gen_server:cast(Ref, {send, Msg}).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {compression, count, sender_ref, sender_mod, format, host}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Opts]) ->
	ConnectionOpts = ?GV(connection, Opts),
	SenderMod = case ?GV(type, ConnectionOpts) of
					tcp -> erl_graylog_tcp_sender;
					udp -> erl_graylog_udp_sender
				end,
	{ok, SenderRef} = SenderMod:open(ConnectionOpts),
    {ok, #state{
				compression = valid_compression(?GV(compression, Opts, ?DEFAULT_COMPRESSION)),
				count = 0,
				sender_ref = SenderRef,
				sender_mod = SenderMod,
				format = valid_format(?GV(format, Opts, ?DEFAULT_FORMAT)),
				host = case ?GV(host, Opts) of
						   undefined -> {ok, Hostname} = inet:gethostname(), list_to_binary(Hostname);
						   HostOpt -> HostOpt
					   end}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({get_opt, compression}, _From, #state{compression = Comp} = S) ->
	{reply, Comp, S};
handle_call({get_opt, format}, _From, #state{format = Fmt} = S) ->
	{reply, Fmt, S};
handle_call({set_opt, compression, Comp}, _From, S) ->
	{reply, ok, S#state{compression = Comp}};
handle_call({set_opt, format, Fmt}, _From, S) ->
	{reply, ok, S#state{format = Fmt}};
handle_call(stop, _From, #state{sender_mod = Mod, sender_ref = SenderRef}=S) ->
	Mod:close(SenderRef),
	{stop, normal, ok, S};
handle_call(_Request, _From, S) ->
	{reply, {error, not_implemented}, S}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({send, Msg}, #state{compression=Comp, sender_ref=SenderRef, sender_mod=Mod, format=Fmt, host=Host}=S) ->
	do_send(Msg, Comp, SenderRef, Mod, Fmt, Host),
	{noreply, S};
	
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% valid_compression/1
%% ====================================================================
%% @doc Return valid compression setting. Throws if not valid
-spec valid_compression(atom()) -> atom().
%% ====================================================================
valid_compression(default) ->	?DEFAULT_COMPRESSION;
valid_compression(none) ->		none;
valid_compression(gzip) ->		gzip;
valid_compression(zlib) ->		zlib;
valid_compression(X) ->			throw({invalid, {compression, X}}).


%% valid_format/1
%% ====================================================================
%% @doc Return valid format setting. Throws if not valid.
-spec valid_format(atom()) -> atom().
%% ====================================================================
valid_format(gelf) ->		gelf;
valid_format(raw) ->		raw;
valid_format(default) ->	?DEFAULT_FORMAT;
valid_format(X) ->			throw({invalid, {format, X}}).


%% do_send/5
%% ====================================================================
%% @doc Worker function for send/1 and send/2
-spec do_send(Msg, Comp, SenderRef, SenderMod, Format, Host) -> term() when
	Msg :: binary()
		 | term(),
	Comp :: compression(),
	SenderRef :: term(),
	SenderMod :: module(),
	Format :: format(),
	Host :: binary().
%% ====================================================================
do_send(Msg, Comp, SenderRef, SenderMod, gelf, Host) when is_binary(Msg) ->
	do_send([{full_message, Msg}], Comp, SenderRef, SenderMod, gelf, Host);
do_send(Msg, Comp, SenderRef, SenderMod, gelf, Host) ->
	Msg1 = case ?GV(host, Msg) of
			   undefined -> [{host, Host}|Msg];
			   _ -> Msg
		   end,
	Encoded = erl_graylog_gelf:to_sendable(erl_graylog_gelf:from_list(Msg1), Comp),
	SenderMod:send(SenderRef, Encoded);
do_send(Msg, _Comp, SenderRef, SenderMod, raw, _Host) when is_binary(Msg) ->
	SenderMod:send(SenderRef, Msg);
do_send(Msg, Comp, SenderRef, SenderMod, raw, Host) ->
	Body = re:replace(io_lib:format("~p", [Msg]), "\n", "\\\\n", [{return, binary}, multiline, global, unicode]),
	do_send(Body, Comp, SenderRef, SenderMod, raw, Host).
	
