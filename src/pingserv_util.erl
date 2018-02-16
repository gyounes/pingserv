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

-module(pingserv_util).
-author("Georges Younes <georges.r.younes@gmail.com>").

%% other
-export([send/2,
         without_me/1]).

%%%===================================================================
%%% util common functions
%%%===================================================================

%% @private
send(Msg, Peers) when is_list(Peers) ->
    lists:foreach(
        fun(Peer) ->
            partisan_default_peer_service_manager:forward_message(Peer, Msg)
        end,
    Peers);
send(M, Peer) ->
    send(M, [Peer]).

% %% @private get current time in milliseconds
% -spec get_timestamp() -> integer().
% get_timestamp() ->
%   {Mega, Sec, Micro} = os:timestamp(),
%   (Mega*1000000 + Sec)*1000 + round(Micro/1000).


%% @private
without_me(Members) ->
    Members -- [node()].