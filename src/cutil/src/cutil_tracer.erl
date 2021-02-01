% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.
-module(cutil_tracer).

-export([enabled/3]).
-export([enabled_call/3]).
-export([enabled_procs/3]).
-export([enabled_running_procs/3]).
-export([enabled_send/3]).
-export([trace/5]).

-on_load(on_load/0).
on_load() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).


enabled(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).


enabled_call(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).


enabled_procs(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).


enabled_running_procs(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).


enabled_send(_, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).


trace(_, _, _, _, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

