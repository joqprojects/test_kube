%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Pool =[{Pid1,Ref1,Module},{Pid2,Ref2,Module}]
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/tcp.hrl").
-include("certificate/cert.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/kubelet_data.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Define
%% --------------------------------------------------------------------
%-define(DEFINE,define).
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(state, {kubelet_info,lSock,max_workers,active_workers,workers,service_list}).
%% --------------------------------------------------------------------


%% External exports -gen_server functions 

-export([start_service/2,
	 stop_service/1,
	 upgrade/2,
	 my_ip/0,
	 dns_register/1,
	 de_dns_register/1
	]).
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start_link(?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

start_service(ServiceId,Vsn)-> 
    gen_server:call(?MODULE, {start_service,ServiceId,Vsn},infinity).
stop_service(ServiceId)-> 
    gen_server:call(?MODULE, {stop_service,ServiceId},infinity).

my_ip()-> 
    gen_server:call(?MODULE, {my_ip},infinity).


dns_register(ServiceInfo)-> 
    gen_server:cast(?MODULE, {dns_register,ServiceInfo}).
de_dns_register(ServiceInfo)-> 
    gen_server:cast(?MODULE, {de_dns_register,ServiceInfo}).

upgrade(ServiceId,Vsn)-> 
    gen_server:cast(?MODULE, {ServiceId,Vsn}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,InitialInfo}=file:consult("kubelet.config"),
    {ip_addr,MyIp}=lists:keyfind(ip_addr,1,InitialInfo),
    {port,Port}=lists:keyfind(port,1,InitialInfo),
    {service_id,ServiceId}=lists:keyfind(service_id,1,InitialInfo),
    {vsn,Vsn}=lists:keyfind(vsn,1,InitialInfo),
    {max_workers,MaxWorkers}=lists:keyfind(max_workers,1,InitialInfo),
    {zone,Zone}=lists:keyfind(zone,1,InitialInfo),
    {capabilities,Capabilities}=lists:keyfind(capabilities,1,InitialInfo),
    KubeletInfo=#kubelet_info{time_stamp="not_initiaded_time_stamp",
			service_id = ServiceId,
			vsn = Vsn,
			ip_addr=MyIp,
			port=Port,
			max_workers=MaxWorkers,
			zone=Zone,
			capabilities=Capabilities
		       },    

    {ok, LSock} = gen_tcp:listen(Port,?SERVER_SETUP),
    Workers=init_workers(LSock,MaxWorkers,[]), % Glurk remove?

    %------ send info to controller
    % if_dns:call("controller",controller,node_register,[KubeletInfo]),
    io:format("Started Service  ~p~n",[{?MODULE}]),
    {ok, #state{kubelet_info=KubeletInfo,
		lSock=LSock,max_workers=MaxWorkers,
		active_workers=0,workers=Workers,service_list=[]}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({my_ip},_From, State) ->
    Reply="localhost", % Test only glurk
    {reply, Reply, State};

handle_call({start_service,ServiceId,Vsn},_From, State) ->
   Reply= case lists:keymember(ServiceId,1,State#state.service_list) of
	      true->
		  {error,[?MODULE,?LINE,'allready started',ServiceId,Vsn]};
	      false->
	    % this function  shall be in lib so it can be used by upgrade
	    % get tar file from SW repositroy
	    % create service_info record
	    % create temp dir 
	    % untar files 
	    % read app file -get all modules
	    % copy modules and app file to service_ebin dir
	    % start the service
	    % remove temp dir
	    % add service to service_list
	    % service shall push info to dns and kubectroller 
	    % reply
		  glurk
	    
	  end,
    {reply, Reply, State};

handle_call({stop_service,ServiceId}, _From, State) ->
    Reply=case lists:keymember(ServiceId,1,State#state.service_list) of
	      false->
		  {error,[?MODULE,?LINE,'service nexists',ServiceId]};
	      true->
	    % de_register the service and remove it from service list
	    % stop application call
	    % read app file -> [modules]
	    % delete[modules] + .app
	    % reply
		  glurk
	  end,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({upgrade,_ServiceId,_Vsn}, State) ->
	    % get tar file from SW repositroy
	    % create service_info record
	    % create temp dir 
	    % untar files 
	    % read app file -get all modules
            % de_register the service and remove it from service list
	    % copy modules and app file to service_ebin dir
	    % start the service
	    % remove temp dir
	    % add service to service_list
	    % service shall push info to dns and kubectroller     % 
    
    
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({_Pid,active}, State) ->
    ActiveWorkers=State#state.active_workers+1,
    NewState = State#state{active_workers=ActiveWorkers},
   io:format("active  ~p~n",[{?MODULE,?LINE,NewState}]), 
   {noreply, NewState};

handle_info({'DOWN',Ref,process,Pid,normal},  #state{lSock = LSock,active_workers=ActiveWorkers,
						     max_workers=Max,workers=Workers} = State) ->
      io:format("DOWN Pid,Ref an workers  ~p~n",[{?MODULE,?LINE,Pid,Ref,Workers}]), 
    W1=lists:delete({Pid,Ref},Workers),
    NewActiveWorkers=ActiveWorkers-1,
    if
	ActiveWorkers<Max-> %Accept new 
	    ParentPid=self(),
	    {NewPid,NewRef}=spawn_monitor(fun()->start_worker(ParentPid,LSock) end),
	    NewWorkerList=[{NewPid,NewRef}|W1];
	ActiveWorkers==Max->
	    NewWorkerList=W1
    end,

    NewState=State#state{active_workers=NewActiveWorkers,workers=NewWorkerList},
    io:format("DOWN  ~p~n",[{?MODULE,?LINE,NewState}]),
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
init_workers(_,0,Workers)->
    Workers;
init_workers(LSock,N,Workers)->
    ParentPid=self(),
    {Pid,Ref}=spawn_monitor(fun()->start_worker(ParentPid,LSock) end),
    NewWorkers=[{Pid,Ref}|Workers],
    init_workers(LSock,N-1,NewWorkers).
    



%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
start_worker(ParentPid,LSock)->
    case gen_tcp:accept(LSock) of 
	{error,closed}->
	    {error,closed};
	{ok, Socket}->	       
	    ParentPid!{self(),active},
	    receive
		{tcp, Socket, RawData}->
		    case binary_to_term(RawData) of
			[{call,{M,F,A}},?KEY_MSG]->
			    Reply=rpc:call(node(),M,F,A),
			    gen_tcp:send(Socket,term_to_binary(Reply));
			[{cast,{M,F,A}},?KEY_MSG]->
			    io:format(" ~p~n",[{?MODULE,?LINE,{cast,{M,F,A}}}]),
			    A=rpc:cast(node(),M,F,A),
			    io:format("Error ~p~n",[{?MODULE,?LINE,A}]);
			Err->
			    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
		    end;
		{tcp_closed,Socket} ->
		    exit
	    end
    end.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

