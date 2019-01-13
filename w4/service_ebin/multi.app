%% This is the application resource file (.app file) for the 'base'
%% application.
{application, multi,
[{description, "multi  " },
{vsn, "1.0.0" },
{modules, 
	  [multi_app,multi_sup,multi,multi_lib]},
{registered,[multi]},
{applications, [kernel,stdlib]},
{mod, {multi_app,[]}},
{start_phases, []}
]}.
