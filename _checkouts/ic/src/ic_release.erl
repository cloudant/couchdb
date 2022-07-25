-module(ic_release).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, boot_script).
-define(NAMESPACE, ic).
%-define(DEPS, [{default, app_discovery}]).
-define(DEPS, [{default, compile}]).
%%-define(DEPS, [{default, lock}]).
%-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 ic boot_script"},
        {short_desc, "Crete boot script"},
        {desc, "Crete boot script to speed up dev node startup"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = rebar_state:get(State, relx, []),
    {release, {Name, Version}, _Apps} = lists:keyfind(release, 1, Config),
    ReleaseSpec = {release,
        {atom_to_list(Name), Version},
        {erts, erlang:system_info(version)},
        apps(State)
    },
    ok = make_script(ReleaseSpec),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

make_script(ReleaseSpec) ->
    ok = rlx_file_utils:write_term("dev/devnode.rel", ReleaseSpec),
    Options = [{path,["_build/default/lib"]}, {outdir,"dev/"}],
    systools:make_script("dev/devnode", [{script_name, "start"} | Options]).

apps(State) ->
    Apps = rebar_state:project_apps(State)
    ++ rebar_state:all_deps(State),
    ProjectAppsVsn = [
        {binary_to_atom(rebar_app_info:name(AppInfo)), rebar_app_info:vsn(AppInfo)}
        || AppInfo <- Apps],
    AppNames = [A || {A, _} <- ProjectAppsVsn],

    Dependencies = lists:foldl(fun(App, Acc) ->
        lists:foldl(fun(A, InAcc) ->
            case maps:is_key(A, InAcc) of
                true -> InAcc;
                false -> maps:put(A, true, InAcc)
            end
        end, Acc, rebar_app_info:applications(App))
    end, #{}, Apps),

    SystemApps = lists:usort(maps:keys(Dependencies) -- AppNames),
    SystemAppsVsn = [system_app(A) || A <- SystemApps],
    ProjectAppsVsn ++ SystemAppsVsn.

system_app(App) ->
    case application:get_key(App, vsn) of
        undefined ->
            application:load(App),
            {ok, Vsn} = application:get_key(App, vsn),
            application:unload(App),
            {App, Vsn};
        {ok, Vsn} ->
            {App, Vsn}
    end.
