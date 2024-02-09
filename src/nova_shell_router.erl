-module(nova_shell_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", { nova_shell_main_controller, index}, #{methods => [get]}},
                 {"/term", nova_shell_terminal_controller, #{protocol => ws}},
                 {"/assets/[...]", "assets"}
                ]
      }].
