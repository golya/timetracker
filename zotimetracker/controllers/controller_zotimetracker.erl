%% @author author <adam.stork@gmail.com>
%% @copyright 2013 Ádám Gólya.
%% @doc TimeTracker Demo application form controller.

-module(controller_zotimetracker).
-author("Ádám Gólya").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([
	provide_content/2,
	event/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").
-define(MINUTES,720).

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[{"text/html", provide_content}], ReqData, Context};
        Mime ->
            {[{Mime, provide_content}], ReqData, Context}
    end.

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Users = zotimetracker:get_users(Context),
    NumberOfUsers = length(Users),
    Vars = [{number_of_users, NumberOfUsers}, {users, Users}],
    home_page(z_context:set(Vars, Context2)).

home_page(Context) ->
    Rendered = z_template:render("home.tpl", z_context:get_all(Context), Context),
    {Output, OutputContext} = z_context:output(Rendered, Context),
    ?WM_REPLY(Output, OutputContext).

%% -----------------------------------------------------------------------------------------------
%% Handle all events
%% -----------------------------------------------------------------------------------------------

event(#postback{message=fill_content}, Context) ->
    z_render:update("content", "Hello World<br/>", Context);

event(#submit{form="add_user"}, Context) ->
    Username = string:strip(z_context:get_q(username, Context)),
    NewUsers = case zotimetracker:has_user(Context,Username) of
        {true, Users} ->
            Users;
        {false, _} ->
            zotimetracker:add_user(Context,[{Username,[]}])
    end,
    refresh_users([{users, NewUsers}], Context);

event(#submit{form="add_activity"}, Context) ->
    Username = string:strip(z_context:get_q(name, Context)),
    [Hour, Minutes] = string:tokens(z_context:get_q(from, Context), ":"),
    From = (list_to_integer(Hour)*60+list_to_integer(Minutes)) div 2,
    Duration = list_to_integer(z_context:get_q(duration, Context)) div 2,
    Activity = [
        {"from", From},
        {"duration", get_duration(From, Duration)},
        {"name", z_context:get_q(activity, Context)},
        {"color",io_lib:format("#~s", [get_random_color()])}
    ],
    NewUsers = case zotimetracker:has_user(Context, Username) of
        {true, _} ->
            zotimetracker:add_user_activity(Context, Username, Activity);
        {false, Users} ->
            Users
    end,
    refresh_users([{users, NewUsers}], Context);

event(Event, Context) ->
    io:format("~p: unknown event ~p", [?MODULE,Event]),
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    z_render:wire({growl, [{text,Error},{stay,1}]}, Context).

%% -----------------------------------------------------------------------------------------------
%% Supporter functions
%% -----------------------------------------------------------------------------------------------

refresh_users(Vars, Context) ->
    NumberOfUsers = length(proplists:get_value(users, Vars)),
    ResultContext = z_render:appear(
        "users_container",
        #render{
            template="ztt_users.tpl",
            vars=[{number_of_users,NumberOfUsers}|Vars]},
        Context
    ),
    z_render:wire({fade_in, [{selector, "#users_container"},{speed, 2000}]}, ResultContext).

get_duration(From, Duration) when (Duration+From) > ?MINUTES ->
    ?MINUTES-From;
get_duration(_, Duration) ->
    Duration.

get_random_color() ->
    random:seed(erlang:now()),
    int_to_hex(random:uniform(999999)).

int_to_hex(I) -> [ i2h(X) || <<X:4>> <= <<I:12/signed-integer>> ].
i2h(X) when X > 9 -> $a + X - 10;
i2h(X) -> $0 + X.