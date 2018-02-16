%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Georges Younes.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(pingserv).
-author("Georges Younes <georges.r.younes@gmail.com>").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% handler callbacks
-export([ping/1,
         setreply/1,
         fullmembership/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {actor :: node(),
                full_membership :: [node()],
                reply_function :: fun()}).

-type state_t() :: #state{}.

%%%===================================================================
%%% handler callbacks
%%%===================================================================

%% callback for setting fullmemberhsip of the group.
-spec fullmembership(term()) -> ok.
fullmembership(Nodes) ->
    gen_server:call(?MODULE, {fullmembership, Nodes}, infinity).

%% ping
-spec ping(term()) -> non_neg_integer().
ping(Index) ->
    gen_server:call(?MODULE, {ping, Index}, infinity).

%% Set reply Notification Fun.
-spec setreply(term()) -> ok.
setreply(Node) ->
    gen_server:call(?MODULE, {setreply, Node}, infinity).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Same as start_link([]).
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(list()) -> {ok, state_t()}.
init([]) ->
    %% Seed the process at initialization.
    rand_compat:seed(erlang:phash2([node()]),
                     erlang:monotonic_time(),
                     erlang:unique_integer()),

    Actor = node(),

    {ok, #state{actor=Actor,
                full_membership=[]}}.

%% @private
-spec handle_call(term(), {pid(), term()}, state_t()) ->
    {reply, term(), state_t()}.

%% @todo Update other actors when this is changed
handle_call({fullmembership, Nodes}, _From, State) ->
    Nodes1 = case lists:last(Nodes) of
        {_, _} ->
            [Node || {_Name, Node} <- Nodes];
        _ ->
            Nodes
    end,
    Sorted = lists:usort(Nodes1),
    lager:info("fullmembership is ~p", [Sorted]),
    {reply, ok, State#state{full_membership=Sorted}};

handle_call({setreply, Node}, _From, State) ->
    ReplyFun = fun(Index) ->
      Node ! {setreply, Index},
      ok
    end,

    {reply, ok, State#state{reply_function=ReplyFun}};

handle_call({ping, Index}, _From, #state{actor=Actor, full_membership=FullMembership}=State) ->

    ToMembers = pingserv_util:without_me(FullMembership),

    Msg = {pong, Index, Actor},

    pingserv_util:send(Msg, ToMembers),
    
    {reply, ok, State}.

%% @private
-spec handle_cast(term(), state_t()) -> {noreply, state_t()}.
handle_cast({pong, Index, Sender}=Msg, #state{actor=Actor, reply_function=ReplyFun}=State) ->

    case Sender of
        Actor ->
            ReplyFun(Index);
        _ ->
            pingserv_util:send(Msg, Sender)
    end,
    
    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec handle_info(term(), state_t()) -> {noreply, state_t()}.
handle_info(Msg, State) ->
    lager:warning("Unhandled info messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec terminate(term(), state_t()) -> term().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term() | {down, term()}, state_t(), term()) -> {ok, state_t()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

