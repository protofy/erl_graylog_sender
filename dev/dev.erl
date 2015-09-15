%% coding: utf-8

-module(dev).

%% ====================================================================
%% API functions
%% ====================================================================
-export([msg/0,
		 udp/0, udp/1,
		 udp_raw/1,
		 tcp/0, tcp/1,
		 send/2]).

msg() ->
	[{full_message, <<"Full Message ∑∑∑"/utf8>>},
	 {short_message, <<"Short ååå Message"/utf8>>},
	 {host, <<"test">>},
	 {level, 1}].

tcp() ->
	tcp(msg()).
	
tcp(Msg) ->
	Opts = [{version, '1.1'},
			{compression, none},
			{connection, [{type, tcp},
						  {ip_version, ipv4},
						  {addr, "lsrv.local"},
						  {port, 12201}]},
			{format, gelf}], 
	{ok, Pid} = erl_graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.

send(Ref, Msg) ->
	erl_graylog_sender:send(Ref, Msg).

udp() ->
	udp(msg()).

udp(Msg) ->
	Opts = [{version, '1.1'},
			{compression, zlib},
			{connection, [{type, udp},
						  {addr, {192,168,56,101}},
						  {port, 12202}]},
			{format, gelf}],
	{ok, Pid} = erl_graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.

udp_raw(Msg) ->
	Opts = [{version, '1.1'},
			{compression, none},
			{connection, [{type, udp},
						  {addr, {192,168,56,101}},
						  {port, 5555}]},
			{format, raw}],
	{ok, Pid} = erl_graylog_sender:start_link(Opts),
	R = send(Pid, Msg),
	{Pid, R}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


