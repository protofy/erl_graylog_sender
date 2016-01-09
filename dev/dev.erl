%% coding: utf-8

-module(dev).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  tcp_opts/0, tcp_opts/1,
  msg/0,
  udp/0, udp/1,
  udp_raw/1,
  tcp/0, tcp/1,
  send/2]).

msg() ->
	[{full_message, <<"Full Message ∑∑∑"/utf8>>},
	 {short_message, <<"Short ååå Message"/utf8>>},
	 {host, <<"test">>},
	 {level, 1}].

tcp_opts() ->
  tcp_opts({192,168,99,100}).

tcp_opts(Addr) ->
  [
    {version, '1.1'},
    {compression, none},
    {connection, [
      {type, tcp},
      {ip_version, ipv4},
      {addr, Addr},
      {port, 12201}
    ]},
    {format, gelf}
  ].

tcp() ->
	tcp(msg()).
	
tcp(Msg) ->
	Opts = tcp_opts(),
	{ok, Pid} = graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.

send(Ref, Msg) ->
	graylog_sender:send(Ref, Msg).

send_sync(Ref, Msg) ->
  graylog_sender:send_sync(Ref, Msg).

udp() ->
	udp(msg()).

udp(Msg) ->
	Opts = [{version, '1.1'},
			{compression, zlib},
			{connection, [{type, udp},
						  {addr, {192,168,99,100}},
						  {port, 12202}]},
			{format, gelf}],
	{ok, Pid} = graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.

udp_raw(Msg) ->
	Opts = [{version, '1.1'},
			{compression, none},
			{connection, [{type, udp},
						  {addr, {192,168,99,100}},
						  {port, 5555}]},
			{format, raw}],
	{ok, Pid} = graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


