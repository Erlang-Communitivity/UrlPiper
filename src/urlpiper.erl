%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/EPLICENSE.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Bill Barnhill.
%% Portions created by Bill Barnhill are Copyright 2012, Bill 
%% Barnhill. All Rights Reserved.''
%% 

%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------

-module(urlpiper).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 content_for/1,
	 start_piping/1, 
	 start_piping/2, 
	 start_piping/3, 
	 stop_piping/1,
	 register_started_piper/2,
	 unregister_stopping_piper/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  pid2url_map=dict:new() :: dict(), 
	  url2pid_map=dict:new() :: dict()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


content_for(Url) ->
    gen_server:call(?MODULE, {content, Url}).

start_piping(Url) ->
    gen_server:cast(?MODULE, {start, Url}).
start_piping(Url, Format) ->
    gen_server:cast(?MODULE, {start, Url, Format}).
start_piping(Url, Format, Interval) ->
    gen_server:cast(?MODULE, {start, Url, Format, Interval}).

stop_piping(Url) ->
    gen_server:cast(?MODULE, {stop, Url}).

register_started_piper(Pid, Url) ->
    gen_server:cast(?MODULE, {register, Pid, Url}).

unregister_stopping_piper(Pid) ->
    gen_server:cast(?MODULE, {unregister, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({content, Url},_From, State) ->
    case watcher_for_url(Url, State) of
	{ok, Pid} -> 
	    Reply = gen_server:call(Pid, content),
	    {reply, Reply, State};
	none ->
	    {reply, {ok, none}, State}
    end;
handle_call(Msg, _From, State) ->
    io:format("Unknown message received at urlpiper:handle_call:~n~w~n", [Msg]),
    {reply, ok, State}.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({register, Pid, Url}, State) ->
    State1 = update_map(Pid, Url, State),
    {noreply, State1};
handle_cast({unregister, Pid}, State) ->
    State1 = remove_from_map(Pid, State),
    {noreply, State1};
handle_cast({start, Url}, State) ->
    supervisor:start_child(urlpiper_worker_rootsup,[Url]),
    {noreply, State};
handle_cast({start, Url, Format}, State) ->
    supervisor:start_child(urlpiper_worker_rootsup,[Url, Format]),
    {noreply, State};
handle_cast({start, Url, Format, Interval}, State) ->
    supervisor:start_child(urlpiper_worker_rootsup,[Url, Format, Interval]),
    {noreply, State};
handle_cast({stop, Url}, State) ->
    WorkerSupPid = global:whereis_name({urlpiper_worker_sup, Url}),
    io:format("Attempting to stop piping for ~s supervised by ~w~n", [Url, WorkerSupPid]),
    %%erlang:exit(WorkerSupPid, kill), 
    supervisor:terminate_child(urlpiper_worker_rootsup, WorkerSupPid),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function looks up the pid of the URLPiper piping the content
%% of the given URL.
%%
%% @spec watcher_for_url(url(), State) -> pid()
%% @end
%%--------------------------------------------------------------------
watcher_for_url(Url, #state{url2pid_map=Map}) ->
    case dict:find(Url, Map) of
	{ok, Pid} ->
	    io:format("Found worker at ~w for ~s~n", [Pid, Url]),
	    {ok, Pid};
	error -> none
    end.
		

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function updates the internal lookup tables so we can find
%% the appropriate URLPiper handling a specific URL, and vice versa.
%%
%% @spec update_map(pid(), url(), #state{}) -> #state{}
%% @end
%%--------------------------------------------------------------------
update_map(Pid, Url, #state{pid2url_map=Pids2Urls, url2pid_map=Urls2Pids}) ->
    #state{
	    pid2url_map=dict:store(Pid, Url, Pids2Urls),
	    url2pid_map=dict:store(Url, Pid, Urls2Pids)
	  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function removes the entries for the passed in Pid
%% from the lookup tables.
%%
%% @spec remove_from_map(pid(), #state{}) -> #state{}
%% @end
%%--------------------------------------------------------------------
remove_from_map(Pid, #state{pid2url_map=Pids2Urls, url2pid_map=Urls2Pids}) ->
    {ok, Url } = dict:find(Pid, Pids2Urls),
    #state{
	    pid2url_map=dict:erase(Pid, Pids2Urls),
	    url2pid_map=dict:erase(Url, Urls2Pids)
	  }.
