-module(e9p_server).

-export([start/1]).

start(Opts) ->
    ranch:start_listener(e9p, ranch_tcp, #{socket_opts => [{port, 9999}]},
                         e9p_proto, Opts).
