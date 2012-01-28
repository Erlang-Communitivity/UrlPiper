%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%% Watch a URL, getching it repeatedly after waiting for an arbitrary 
%%% interval.  What's available in cache depends on the format requested.
%%% If 'raw' then the whole response from httpc is cached. If 'body then
%%% just the body is cached. If 'xml' then the body is treated as XML and
%%% parsed by xmerl, with the parse result stored in cache.
%%% There are a number of things to fix, including:
%%% * Status other than 200 and process dies
%%% * Protocol other than "HTTP/1.1" and process dies
%%% * Does not follow redirects
%%% * Does not handle authenticated requests
%%% * xml format only handles XML body that unicode:bin_is_7bit returns true for 
%%%    - found some compatible UTF-8 causes a false, or xmerl is accepting a
%%%      UTF-8 list form of binary and parsing it. For now, a hack is to
%%%    - remove check all together.
%%% * Should check e-tag and last-modified headers so as not to fetch unchanged content
%%% @end
%%% Created : 21 Jan 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------
-module(urlpiper_worker).

-behaviour(gen_server).

%% API
-export([
	 start_link/0, 
	 start_link/1, 
	 start_link/2,
	 start_link/3,
	 stop/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(DEFAULT_INTERVAL, 3600000). %% Default is 1 hr
-define(INITIAL_INTERVAL, 5000). %% Does first fetch after 5 seconds
-define(DEFAULT_FORMAT, xml).

-record(state, {url, interval, cached, format, buffer, timer, request}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server. This requires inets and ssl to be started
%% prior. Manually this can be done with inets:start(),ssl:start().
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).
start_link(Url) ->
    gen_server:start_link(?MODULE, [Url],[]).
start_link(Url, Format) ->
    gen_server:start_link(?MODULE, [Url, Format], []).
start_link(Url, Format, PollingInterval) ->
    gen_server:start_link(?MODULE, [Url, Format, PollingInterval], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


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
init([Url]) ->
    init([Url, ?DEFAULT_FORMAT, ?DEFAULT_INTERVAL]);
init([Url, Format]) ->
    init([Url, Format, ?DEFAULT_INTERVAL]);
init([Url, Format, PollingInterval]) ->
    io:format("urlpiper_worker started for ~s as ~p ~n", [Url, self()]),
    urlpiper:register_started_piper(self(), Url),
    {ok, #state{
       url=Url,                   %% The URL of web page to fetch
       interval=PollingInterval,  %% The time between fetches of that URL
       cached=none,               %% The cached output
       buffer=[],                 %% Holds content as it is being refreshed, when done copied to cached and reset
       format=Format              %% The output format
                                  %%  This is one of:
                                  %%    body - just the body of the HTML returned is sent
                                  %%           and exception is thrown if not HTML
                                  %%    raw - the whole result is sent
                                  %%    xml - the result from parsing body with xmlerl is sent
        
      }, ?INITIAL_INTERVAL}.  %% The 3rd arg triggers a timeout after the value of the arg in ms

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
handle_call(url, _From, State) ->
    Reply = {ok, State#state.url},
    {reply, Reply, State};
handle_call(content, _From, State) ->
    Reply = {ok, State#state.cached},
    {reply, Reply, State}.

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
handle_cast(stop, State) ->
    io:format("Received stop msg in urlpiper_worker at ~w.~n",[self()]),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @todo Handler non 7bit-compatible UTF8, and handle non-UTF8 unicode
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({http, {_From, Result={{"HTTP/1.1",200,"OK"}, _Headers, _Body}}}, State=#state{format=raw}) ->
    log("got result to return raw"),
    {noreply, State#state{cached=Result}};
handle_info({http, {_From, {{"HTTP/1.1",200,"OK"}, _Headers, Body}}}, State=#state{format=body}) ->
    log("got body to return as is"),
    {noreply, State#state{cached=Body}};
handle_info({http, {_From, {{"HTTP/1.1",200,"OK"}, _Headers, Body}}}, State=#state{format=xml}) ->
    log("got body to format as xml"),
    %%case unicode:bin_is_7bit(Body) of
	%%true ->
	    XmlString = binary_to_list(Body),
	    ParseResult = xmerl_scan:string(XmlString),
	    {noreply, State#state{cached=ParseResult}};
	%%_ ->
	  %%  Error = {error, bad_arg, ["The URL ", State#state.url, " returned non non-ASCII compatible content."], State}, 
	  %%  {stop, Error, Error, State}
    %%end;
handle_info(timeout, State) ->
    log("entered", {handle_info, timeout}),
    Url = State#state.url,
    RequestId = httpc:request(get, {Url, []}, [], [{sync, false}]),
    TimerRef = timer:send_after(State#state.interval, timeout),
    {noreply, State#state{buffer=[], timer=TimerRef, request=RequestId}};
handle_info(Info, State) ->
    log("handle_info", Info),
    {noreply, State}.


log(Tag, Msg) ->
    io:format("-- Received [~s] --~n~w~n-- End of received --~n~n", [Tag, Msg]).
log(Tag) ->
    io:format("-- Received [~s] --~n~n", [Tag]).

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
terminate(_Reason, #state{timer=TimerRef,request=RequestId}) ->
    io:format("Terminating urlpiper_worker at ~w~n", [self()]),
    urlpiper:unregister_stopping_piper(self()),
    httpc:cancel_request(RequestId),
    timer:cancel(TimerRef),
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
