[![Build Status](https://travis-ci.org/protofy/erl_graylog_sender.svg)](https://travis-ci.org/protofy/erl_graylog_sender)

# graylog_sender

This library application can be used to send messages to [graylog](https://www.graylog.org/).
Messages can be sent to TCP and UDP inputs either in [GELF (1.1)](https://www.graylog.org/resources/gelf/) or raw format.

## Usage
Although you can use all modules separately you probably want to use the graylog_sender module.

### Example 1: unnamed, UDP, GELF, gzip
If you like to take it for a spin: After cloning, get the updates and compile:

    > make update && make

You should have a running graylog server with properly configured inputs.

Start the dev console:

    > dev/dev_console.sh
    
Start a sender (start_link/1):
```
erl> Connection = [{type, udp},
                   {addr, "graylog.example.com"},
                   {port, 12202}].
erl> Opts = [{connection, Connection},
             {compression, gzip},
             {format, gelf}].
erl> {ok, Pid} = graylog_sender:start_link(Opts).
```

Send a message (send/2):
```
erl> Msg = [{full_message, <<"Test Message">>}, {level, 1}].
erl> graylog_sender:send(Pid, Msg).
```

The message should now appear in graylog.

To stop the server call stop(Pid).


### Example 2: singleton, TCP, ipv6, raw, no compression, explicit host, binary message
You can also start the server in singleton mode. This is useful if you want only one sender and you want to be able to access it node-wide without having to know its name or pid.

```
> Connection = [{type, tcp},
                {ip_version, ipv6},
                {addr, {0,0,0,0,0,0,0,1}},
                {port, 12201}].
> Opts = [{connection, Connection},
          {compression, none},
          {format, raw},
          {server_ref, singleton},
          {host, <<"Testhost">>}].
> {ok, Pid} = graylog_sender:start_link(Opts).

> Msg = <<"Test Message">>.
> graylog_sender:send(Msg).
```

### Notes:
As of right now you cannot send messages compressed via TCP, due to graylog using \0 as message separator.  

In singleton mode you should use send/1, stop/0, get_opt/1 and set_opt/2 to access the sender. Otherwise use send/2, stop/1, get_opt/2, set_opt/3.

You can have a maximum of one singleton sender and several named and/or unnamed senders. 

For further information about the available options see the graylog_sender module.

## Dependencies
- protofy_common: Protofy common modules and macros
- jiffy: JSON NIFs for Erlang


## ToDos
- Add set_opt for host
- Add field encryption / pseudonymization / anonymization 
