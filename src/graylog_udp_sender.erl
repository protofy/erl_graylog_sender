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
%% @doc This is the UDP worker module for graylog_sender.
%%
%% Packets, which are too big for one UDP message will be chunked according to GELF protocol (https://www.graylog.org/resources/gelf/).
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
%%   - values: see gen_udp:option().
%% '''
%%
%% == Example ==
%% ```
%% > {ok, Socket} = graylog_udp_sender:open([{addr, {127,0,0,1}}, {port, 1234}]).
%% > graylog_udp_sender:send(Socket, <<"Test Packet">>).
%% > graylog_udp_sender:close(Socket).
%% '''
%%


-module(graylog_udp_sender).

-include_lib("protofy_common/include/protofy_common.hrl").

-define(MAX_UDP_SIZE, 8192).
-define(MAX_CHUNK_SIZE, 8180). %% 8192 minus Chunked GELF header
-define(MAX_MSG_SIZE, 1047040). %% 8180 * 128 (chunks)
-define(GELF_MSGID_SIZE, 8).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  open/1,
  close/1,
  send/2
]).

-define(DEFAULT_IP_VERSION, inet).

-type addr() :: inet:ip_address()
              | inet:hostname().
-type opt() :: {ip_version, ipv4 | ipv6 | default}
             | {addr, addr()}
             | {port, inet:port_number()}
             | gen_udp:option().
-type socket_info() :: {inet:socket(), addr(), inet:port_number()}.

%% open/1
%% ====================================================================
%% @doc Open socket for sending
-spec open([opt()]) -> {ok, socket_info()} | {error, inet:posix()}.
%% ====================================================================
open(Opts) ->
  Port = ?GV(port, Opts),
  Addr = ?GV(addr, Opts),
  Version = case ?GV(ip_version, Opts, default) of
              ipv4 -> inet;
              ipv6 -> inet6;
              default -> ?DEFAULT_IP_VERSION
            end,
  SocketOpts = [Version, binary | ?GV(socket_opts, Opts, [])],
  {ok, Socket} = gen_udp:open(0, SocketOpts),
  {ok, {Socket, Addr, Port}}.


%% close/1
%% ====================================================================
%% @doc Close the socket
-spec close(socket_info()) -> ok.
%% ====================================================================
close({Socket,_,_}) ->
  gen_udp:close(Socket).


%% send/2
%% ====================================================================
%% @doc Send Packet (e.g. the graylog message) via socket.
%%
%% The packet will get separated into chunks if it is more than
%% 8192 bytes in size.
-spec send(socket_info(), Packet :: iodata()) -> ok | {error, Reason} when
  Reason :: not_owner
          | inet:posix().
%% ====================================================================
send({Socket, Addr, Port}, Packet) ->
  Size = byte_size(Packet),
  if
    Size > ?MAX_MSG_SIZE ->
      throw({message_too_big, [{is, Size},{max, ?MAX_MSG_SIZE}]});
    Size > ?MAX_UDP_SIZE ->
      Num0 = Size div ?MAX_CHUNK_SIZE,
      Num = if
              (Num0 * ?MAX_CHUNK_SIZE) < Size -> Num0 + 1;
              true -> Num0
            end,
      MessageId = crypto:rand_bytes(?GELF_MSGID_SIZE),
      send_chunks(Socket, Addr, Port, Packet, MessageId, binary:encode_unsigned(Num), 0, Size);
    true ->
      gen_udp:send(Socket, Addr, Port, Packet)
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% send_chunks/8
%% ====================================================================
%% @doc Send chunks of a packet
- spec send_chunks(Socket, Addr, Port, Packet, MessageId, Num, Seq, RestSize) -> Result when
  Socket :: gen_udp:socket(),
  Addr :: inet:ip_address(),
  Port :: inet:port_number(),
  Packet :: binary(),
  MessageId :: binary(),
  Num :: binary(),
  Seq :: non_neg_integer(),
  RestSize :: non_neg_integer(),
  Result :: ok | {error, not_owner | inet:posix()}.
%% ====================================================================
send_chunks(Socket, Addr, Port, Packet, MessageId, Num, Seq, RestSize) when RestSize > ?MAX_CHUNK_SIZE ->
  <<Payload:?MAX_CHUNK_SIZE/binary,Rest/binary>> = Packet,
  gen_udp:send(Socket, Addr, Port, make_chunk(Payload, MessageId, Seq, Num)),
  send_chunks(Socket, Addr, Port, Rest, MessageId, Num, Seq+1, byte_size(Rest));
send_chunks(Socket, Addr, Port, Packet, MessageId, Num, Seq, _) ->
  gen_udp:send(Socket, Addr, Port, make_chunk(Packet, MessageId, Seq, Num)).


%% make_chunk/4
%% ====================================================================
%% @doc Make graylog chunk
-spec make_chunk(Payload, MessageId, Seq, Num) -> binary() when
  Payload :: binary(),
  MessageId :: binary(),
  Seq :: non_neg_integer(),
  Num :: binary().
%% ====================================================================
make_chunk(Payload, MessageId, Seq, Num) ->
  BinSeq =  binary:encode_unsigned(Seq),
  <<16#1e,16#0f, MessageId:8/binary, BinSeq:1/binary, Num:1/binary, Payload/binary>>.

