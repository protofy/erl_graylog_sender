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
%% @doc This is the TCP worker module for erl_graylog_sender.
%%
%% == Options ==
%% ```
%% - ip_version - optional, default: ipv4
%%   - values: ipv4 | ipv6 | default (ip4)
%%
%% - addr - mandatory
%%   - values: inet:ip_address() | inet:hostname()
%% 
%% - port - mandatory
%%   - value: inet:port_number()
%%
%% - socket_opts - optional, default:
%%   - values: see gen_tcp:option().
%% '''
%%
%% == Example ==
%% ```
%% > {ok, Socket} = erl_graylog_tcp_sender:open([{addr, {127,0,0,1}}, {port, 1234}]).
%% > erl_graylog_tcp_sender:send(Socket, <<"Test Packet">>).
%% > erl_graylog_tcp_sender:close(Socket).
%% '''
%%

-module(erl_graylog_tcp_sender).

-include_lib("protofy_common/include/protofy_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/1,
		 close/1,
		 send/2]).

-define(DEFAULT_IP_VERSION, inet).

open(Opts) ->
	Version = case ?GV(ip_version, Opts, default) of
				  ipv4 -> inet;
				  ipv6 -> inet6;
				  default -> ?DEFAULT_IP_VERSION
			  end,
	SocketOpts = [Version, binary, {packet, 0} | ?GV(socket_opts, Opts, [])],
	gen_tcp:connect(?GV(addr, Opts), ?GV(port, Opts), SocketOpts).
	
close(Socket) ->
	gen_tcp:close(Socket).

send(Socket, Packet) ->
	gen_tcp:send(Socket, <<Packet/binary,0>>).

%% ====================================================================
%% Internal functions
%% ====================================================================


