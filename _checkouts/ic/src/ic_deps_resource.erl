-module(ic_deps_resource).

% This resource does a very simple thing. 
% It copies the source of deps_dir/<app> if it is present into `rebar_app_info:dir(AppInfo)` (`_build/default/lib/<app>`).
% Otherwise it dispatches to rebar's `rebar_resource_v2:download/3`.
% This is in order to ensure that we can build from a source archive where all external dependencies would be already in `deps_dir`

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

-include_lib("kernel/include/file.hrl").

-define(DEBUG_INTO,
    rebar_api:debug("~s:~i ~s/~i~n", [?MODULE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY])).

init(Type, State) ->
    ?DEBUG_INTO,
    DepsDir = case rebar_state:get(State, extra_apps_dir, undefined) of
        undefined -> undefined;
        Dir -> filename:join([rebar_dir:root_dir(State), Dir])
    end,
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{deps_dir => DepsDir}),
    {ok, Resource}.

lock(AppInfo, State) ->
    ?DEBUG_INTO,
    case has_source(AppInfo, State) of
        true ->
            lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo));
        false ->
            Source = unwrap_source(AppInfo),
            AppInfo1 = rebar_app_info:source(AppInfo, Source),
            rebar_git_resource:lock(AppInfo1, #{})
    end.

%% Documented order of arguments here 
download(TmpDir, AppInfo, RebarState, State) ->
    ?DEBUG_INTO,
    AppSourceDir = app_source_dir(AppInfo, State),
    Source = unwrap_source(AppInfo),
    case has_source(AppSourceDir) of
        true ->
            rebar_api:debug("~s:~i `~p` exists, copying it to build", [?MODULE, ?LINE, AppSourceDir]),
            ok = filelib:ensure_dir(TmpDir),
            foreach(fun(X) -> copy(X, Source, TmpDir) end, [".git", "_build", "examples"], AppSourceDir),
            LastModified = last_modified(AppSourceDir),
            {ok, A} = file:read_file_info(TmpDir),
            file:write_file_info(AppSourceDir, A#file_info{mtime = LastModified, atime = LastModified});
        false ->
            rebar_api:debug("~s:~i `~p` doesn't exists, fetching from ~p", [?MODULE, ?LINE, AppSourceDir, Source]),
            AppInfo1 = rebar_app_info:source(AppInfo, Source),
            rebar_git_resource:download(TmpDir, AppInfo1, RebarState, #{})
    end.


app_source_dir(AppInfo, #{deps_dir := DepsDir}) ->
    filename:join([DepsDir, binary_to_list(rebar_app_info:name(AppInfo))]).

unwrap_source(AppInfo) ->
    case rebar_app_info:source(AppInfo) of
        {dep, Spec} ->
            Spec;
        {dep, Spec, _Opts} ->
            Spec()
    end.

has_source(AppInfo, State) ->
    filelib:is_dir(app_source_dir(AppInfo, State)).

has_source(AppSourceDir) ->
    filelib:is_dir(AppSourceDir).


make_vsn(AppInfo, State) ->
    ?DEBUG_INTO,
    case has_source(AppInfo, State) of
        true ->
            make_vsn_(AppInfo, State);
        false ->
            Source = unwrap_source(AppInfo),
            AppInfo1 = rebar_app_info:source(AppInfo, Source),
            rebar_git_resource:make_vsn(AppInfo1, #{})
    end.

needs_update(AppInfo, State) ->
    ?DEBUG_INTO,
    case has_source(AppInfo, State) of
        true ->
            needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo));
        false ->
            Source = unwrap_source(AppInfo),
            AppInfo1 = rebar_app_info:source(AppInfo, Source),
            rebar_git_resource:needs_update(AppInfo1, #{})
        end.

%% Following functions are lifted from https://github.com/benoitc/rebar3_path_deps/blob/master/src/rebar_path_resource.erl
%% Which is licensed under ASFv2
%% Copyright 2018, Benoit Chesneau <bchesneau@gmail.com>.

make_vsn_(_Dir, _State) ->
    {error, "Replacing version of type path not supported."}.

lock_(Dir, {dep, Path, _}) ->
    lock_(Dir, {path, Path});

lock_(_Dir, {dep, Path}) ->
    ?DEBUG_INTO,
    {ok, Cwd} = file:get_cwd(),
    Source = filename:join([Cwd, Path]),
    {path, Path, {mtime, to_iso8601(last_modified(Source))}}.


needs_update_(Dir, {dep, Path}) ->
    {ok, Cwd} = file:get_cwd(),
    Source = filename:join([Cwd, Path]),
    LastModified = last_modified(Source),
    Old = filelib:last_modified(Dir),
    rebar_log:log(debug, "compare dir=~p, path=~p last modified=~p, old=~p~n", [Dir, Path, LastModified, Old]),
    (Old < LastModified).


last_modified(Source) ->
    Files = filter_files(dir_files(Source)),
    last_modified_(Files).

last_modified_([]) -> calendar:local_time();
last_modified_(Files) ->
    lists:foldl(
        fun(Path, OldT) ->
            T = filelib:last_modified(Path),
            if
            T > OldT -> T;
            true -> OldT
            end
        end,
        0,
        Files).

to_iso8601({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).


dir_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
            filelib:wildcard(filename:join(Path, "**"));
        false ->
            [Path]
    end.


filter_files(Files) ->
    lists:filter(fun is_excluded/1, [filename:absname(F) || F <- Files]).


is_excluded(Path) ->
      KnownExcludes = [
                     "^.",
                     "~$"
                      ],

      lists:foldl(fun(_, true) -> true;
                     (RE, false) ->
                      (re:run(Path, RE, [unicode]) =/= nomatch) orelse (filelib:is_regular (Path) /= true)
                  end, false, KnownExcludes).

copy(File, Source, Target) ->
    SourceFile = filename:join([Source | File]),
    TargetFile = filename:join([Target | File]),
    ok = filelib:ensure_dir(TargetFile),
    {ok, _} = file:copy(SourceFile, TargetFile).

%%
%% applies a function to each file for its side-effects
foreach(Fun, Ignore, Path) ->
    foreach(Fun, Path, Ignore, []).

foreach(Fun, Root, Ignore, Path) ->
    File = filename:join([Root | Path]),
    case filelib:is_dir(File) of
        true  ->
            case file:list_dir(File) of
                {ok, List} ->
                    lists:foreach(
                        fun(X) ->
                            foreach(Fun, Root, Ignore, X)
                        end,
                        [Path ++ [X] || X <- List, not lists:member(X, Ignore)]
                    );
                {error, _Reason} ->
                   ok
            end;
        false ->
            Fun(Path)
    end.