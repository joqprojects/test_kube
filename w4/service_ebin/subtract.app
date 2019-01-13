%% This is the application resource file (.app file) for the 'base'
%% application.
{application, subtract,
[{description, "subtract  " },
{vsn, "1.0.0" },
{modules, 
	  [subtract_app,subtract_sup,subtract,subtract_lib]},
{registered,[subtract]},
{applications, [kernel,stdlib]},
{mod, {subtract_app,[]}},
{start_phases, []}
]}.
