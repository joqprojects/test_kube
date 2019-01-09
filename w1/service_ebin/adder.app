%% This is the application resource file (.app file) for the 'base'
%% application.
{application, adder,
[{description, "adder  " },
{vsn, "1.0.0" },
{modules, 
	  [adder_app,adder_sup,adder,adder_lib]},
{registered,[adder]},
{applications, [kernel,stdlib]},
{mod, {adder_app,[]}},
{start_phases, []}
]}.
