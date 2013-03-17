%% @author Ádám Gólya <adam.stork@gmail.com>
%% @copyright 2013 Ádám Gólya
%% Generated on 2013-03-15
%% @doc This site was based on the 'nodb' skeleton.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(zotimetracker).
-author("Ádám Gólya").

-behaviour(gen_server).

-mod_title("zotimetracker zotonic site").
-mod_description("A Timetracker powered by zotonic.").
-mod_prio(10).

-export([get_users/1, add_user/2, add_user_activity/3, has_user/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").

% actually this record with users field repesent a fake database,
% but its good chance to try a gen_server based module :)
-record(state, {
    context,
    users=[
        {"Lionel Messi",[
            [{"from", 100},{"duration", 73}, {"name", "Breakfast Issue#30214"}, {"color","#8BD61B"}],
            [{"from", 362},{"duration", 150}, {"name", "Do some usefull thing Issue#28474"}, {"color","#FFFFFF"}],
            [{"from", 270},{"duration", 100}, {"name", "Football football football Issue#27894"}, {"color","#2B95BC"}],
            [{"from", 184},{"duration", 103}, {"name", "Working with redirection Issue#30214"}, {"color","#FCEC07"}]
        ]},
        {"Andres Iniesta",[
            [{"from", 114},{"duration", 334}, {"name", "Make some trick with the ball Issue#29544"}, {"color","#D7ED87"}]
        ]}
    ]
}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    Name = z_utils:name_for_host(?MODULE, z_context:site(Context)),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

get_users(Context) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), get_users).

has_user(Context, User) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {has_user, User}).

add_user(Context, User) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {add_user, User}).

add_user_activity(Context, User, Activity) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {add_user_activity, User, Activity}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{context=z_context:new(Context)}}.

handle_call({add_user, User}, _From, State) ->
    NewUsers = lists:append(State#state.users, User),
    NewState = State#state{users = NewUsers },
    {reply, NewState#state.users, NewState};

handle_call({has_user, Username}, _From, State) ->
    case proplists:lookup(Username, State#state.users) of
        none ->
            {reply, {false, State#state.users}, State};
        {_, _} ->
            {reply, {true, State#state.users}, State}
    end;

handle_call(get_users, _From, State) ->
    {reply, State#state.users, State};

handle_call({add_user_activity, Username, Activity}, _From, State) ->
    UserActivities = proplists:get_value(Username, State#state.users),
    NewUser = {Username, lists:append(UserActivities, [Activity])},
    NewUsers = [ NewUser | proplists:delete(Username, State#state.users)],
    NewState = State#state{users = NewUsers},
    {reply, NewState#state.users, NewState};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.