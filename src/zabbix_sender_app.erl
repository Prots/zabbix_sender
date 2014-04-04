-module(zabbix_sender_app).
-author("Igor Prots <prots.igor@gmail.com>").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    zabbix_sender_sup:start_link().

stop(_State) ->
    ok.
