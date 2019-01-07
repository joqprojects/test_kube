%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_lib).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/kubelet_data.hrl").
-include("kube/include/repository_data.hrl").
%% --------------------------------------------------------------------
-define(NUM_TRIES_START_SERVICE,10).
-define(INTERVAL_START_SERVICE,1000).


%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
zone()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(zone,1,I) of
	  {zone,Z}->
	      Z;
	  false ->
	      []
      end,
    R.

capabilities()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(capabilities,1,I) of
	  {capabilities,C}->
	      C;
	  false ->
	      []
      end,
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

upgrade(ServiceId,Vsn)->
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules}=repo:read_artifact(ServiceId,Vsn),  
    EbinDir=?SERVICE_EBIN,
    Appfile=filename:join(EbinDir,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
    [file:write_file(filename:join(EbinDir,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
    {ok,[{application,_,Info}]}=file:consult(Appfile),
		  {modules,ModuleList}=lists:keyfind(modules,1,Info),

    GenServerModule=list_to_atom(ServiceId),  
    ModulesToPurge=[Module||Module<-ModuleList,false==(GenServerModule==Module)],
    update_modules(ModulesToPurge),
    update_server(GenServerModule),
    ok.

update_server(GenServerModule)->
    ok= sys:suspend(GenServerModule),
    false=code:purge(GenServerModule),
    {module,GenServerModule}=code:load_file(GenServerModule),
    ok= sys:change_code(GenServerModule,GenServerModule,"0",[]),
    sys:resume(GenServerModule).

update_modules([])->
    ok;
update_modules([Module|T]) ->
    code:purge(Module),
    code:load_file(Module),
    update_modules(T).
