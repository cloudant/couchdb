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
    %%adjust_path(State),
    ExtraApps = [
        kernel,
        stdlib,
        asn1,
        compiler,
        crypto,
        inets,
        runtime_tools,
        sasl,
        public_key,
        ssl,
        syntax_tools,
        xmerl,
        tools
    ],
    Extra = [system_app(A) || A <- ExtraApps],
    ReleaseSpec = {release,
        {atom_to_list(Name), Version},
        {erts, erlang:system_info(version)},
        apps(State) ++ Extra
    },
    io:format("===~nReleaseSpec: ~p~n~n===", [ReleaseSpec]),
    ok = make_script(ReleaseSpec),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

% release_spec(RelXConfig, Name, RelVersion) ->
%     {ok, State} = rlx_config:to_state(RelXConfig),
%     Release = rlx_state:get_configured_release(State, Name, RelVersion),
%     {ok, RealizedRelease, _} = rlx_resolve:solve_release(
%         Release,
%         rlx_state:available_apps(State, #{})),
%     rlx_release:metadata(RealizedRelease).

make_script(ReleaseSpec) ->
    ok = rlx_file_utils:write_term("dev/devnode.rel", ReleaseSpec),
    Options = [{path,["_build/default/lib"]}, {outdir,"dev/"}],
    systools:make_script("dev/devnode", [{script_name, "start"} | Options]).

% adjust_path(State) ->
%     Apps = rebar_state:project_apps(State),
%     State2 = rebar_state:update_all_deps(State, Apps),
%     CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps],
%     State3 = rebar_state:update_code_paths(State2, all_deps, CodePaths),
%     %ListOfApps = [rebar_app_info:name(A) || A <- Apps],
%     %io:format("~p~n", [ListOfApps]),
%     %CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps],
%     %code:add_pathsa(CodePaths).
%     ok.

apps(State) ->
    Apps = rebar_state:project_apps(State)
    ++ rebar_state:all_deps(State),
    [{binary_to_atom(rebar_app_info:name(AppInfo)), rebar_app_info:vsn(AppInfo)}
    || AppInfo <- Apps].

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