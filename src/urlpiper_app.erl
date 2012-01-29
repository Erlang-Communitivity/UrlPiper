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

-module(urlpiper_app).

-behaviour(application).


%% Application callbacks
-export([start/2, start_deps/0, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, #state{} } | {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%% @end
%%--------------------------------------------------------------------
start(_StartType, _Args) ->
    case urlpiper_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	{error, Reason} -> {error, Reason};
	Other -> io:format("Unknown result from urlpiper_sup:start_link: ~w~n", [Other]),
		 {error, {unknown_result, Other}}
    end.
	     
start_deps() ->
    Deps = [sasl, crypto, inets, public_key,ssl],
    Running = [ A || {A, _Desc, _Vsn} <- application:which_applications() ],
    io:format("Running applications are ~w~n", [Running]),
    Start = lists:subtract(Deps, Running),
    io:format("Applications to start are ~w~n", [Start]),
    lists:foreach(fun application:start/1, Start).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


