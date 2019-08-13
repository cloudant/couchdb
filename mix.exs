defmodule CouchDB.Mixfile do
  use Mix.Project

  @src_dir Path.expand("src", __DIR__)
  def project do
    set_env()
    add_rebar_files()
    [
      app: :couchdbapp,
      apps: apps(),
      version: "0.1.0",
      elixir: "~> 1.5",
      lockfile: Path.expand("mix.lock", __DIR__),
      deps_path: Path.expand("src", __DIR__),
      build_path: Path.expand("_build", __DIR__),
      erlc_options: erlc_options(),
      compilers: [:elixir, :app],
      start_permanent: Mix.env() == :prod,
      build_embedded: Mix.env() == :prod,
      deps: deps(),
      consolidate_protocols: Mix.env() not in [:test, :dev, :integration],
      test_paths: get_test_paths(Mix.env()),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [
        :setup,
        :logger,
        :couch_index
      ],
      applications: [:httpotion]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(:integration), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(_), do: ["test/elixir/lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      # In-repo dependencies
      {:couch_epi, path: expand("couch_epi")},
      {:couch_log, path: expand("couch_log")},
      {:chttpd, path: expand("chttpd")},
      {:couch, path: expand("couch")},
      {:couch_event, path: expand("couch_event")},
      {:mem3, path: expand("mem3")},
      {:couch_index, path: expand("couch_index"),
          #extra: [erl_opts: [{:i, @src_dir}]],
          #system_env: [{"ERL_LIBS", @src_dir}],
          #erl_opts: [{:i, @src_dir}],
          #compile: "~/.mix/rebar compile deps_dir=#{@src_dir}"
      },
      {:couch_mrview, path: expand("couch_mrview")},
      {:couch_replicator, path: expand("couch_replicator")},
      {:couch_plugins, path: expand("couch_plugins")},
      {:couch_pse_tests, path: expand("couch_pse_tests")},
      {:couch_stats, path: expand("couch_stats")},
      {:couch_peruser, path: expand("couch_peruser")},
      {:couch_tests, path: expand("couch_tests")},
      {:ddoc_cache, path: expand("ddoc_cache")},
      {:dreyfus, path: expand("dreyfus")},
      {:fabric, path: expand("fabric")},
      {:global_changes, path: expand("global_changes")},
      {:mango, path: expand("mango")},
      {:rexi, path: expand("rexi")},
      {:setup, path: expand("setup")},

      # Independent apps
      {:config, git: "https://github.com/apache/couchdb-config.git", tag: "2.1.6"},
      {:b64url, git: "https://github.com/apache/couchdb-b64url.git", tag: "1.0.1"},
      {:ets_lru, git: "https://github.com/apache/couchdb-ets-lru.git", tag: "1.0.0"},
      {:khash, git: "https://github.com/apache/couchdb-khash.git", tag: "1.0.1"},
      {:snappy, git: "https://github.com/apache/couchdb-snappy.git", tag: "CouchDB-1.0.4"},
      {:ioq, git: "https://github.com/apache/couchdb-ioq.git", tag: "2.1.2"},
      {:hqueue, git: "https://github.com/apache/couchdb-hqueue.git", tag: "1.0.1"},
      {:smoosh, git: "https://github.com/apache/couchdb-smoosh.git", tag: "1.0.1"},
      {:ken, git: "https://github.com/apache/couchdb-ken.git", tag: "1.0.3"},

      # Non-Erlang deps
      {:docs, git: "https://github.com/apache/couchdb-documentation.git", tag: "2.3.0", compile: false},
      {:fauxton, git: "https://github.com/apache/couchdb-fauxton", tag: "v1.1.19", compile: false},

      # Third party deps
      {:folsom, git: "https://github.com/apache/couchdb-folsom.git", tag: "CouchDB-0.8.3"},
      {:hyper, git: "https://github.com/apache/couchdb-hyper.git", tag: "CouchDB-2.2.0-4"},
      {:ibrowse, git: "https://github.com/apache/couchdb-ibrowse.git", tag: "CouchDB-4.0.1-1", override: true, manager: :rebar},
      {:jiffy, git: "https://github.com/apache/couchdb-jiffy.git", tag: "CouchDB-0.14.11-2"},
      {:mochiweb, git: "https://github.com/apache/couchdb-mochiweb.git", tag: "v2.19.0"},
      {:meck, git: "https://github.com/apache/couchdb-meck.git", tag: "0.8.8"},
      {:grpcbox, git: "https://github.com/tsloughter/grpcbox"}, 

      # Development deps
      {:couchdbtest, path: expand("test")}
    ]
  end

  defp apps() do
    [
      :couch,
      :couch_index
    ]
  end

  def get_test_paths(:test) do
    Path.wildcard("src/*/test/exunit") |> Enum.filter(&File.dir?/1)
  end

  def get_test_paths(:integration) do
    integration_tests =
      Path.wildcard("src/*/test/integration") |> Enum.filter(&File.dir?/1)

    ["test/elixir/test" | integration_tests]
  end

  def get_test_paths(_) do
    []
  end

  defp erlc_options() do
    [{:i, Path.expand("src", __DIR__)}]
  end

  defp expand(dep_dir) do
    Path.expand(dep_dir, @src_dir)
  end

  defp set_env() do
    # This the only option to add include dir when we calling into
    # external manager `rebar` | `rebar3`
    System.put_env("ERL_COMPILER_OPTIONS", "[{i, \"#{@src_dir}\"}]")
    System.put_env("COUCHDB_APPS_CONFIG_DIR", Path.expand("rel/apps", __DIR__))
    System.put_env("rebar", Path.expand("~/.mix/rebar"))
  end

  defp add_rebar_files() do
    # Some applications don't have rebar.config and Makefile
    [
      "setup",
      "ddoc_cache",
      "global_changes",
      "couch_stats",
      "dreyfus",
      "couch_plugins",
      "couch_pse_tests",
      "couch_peruser",
      "mem3",
      "couch_replicator",
      "ets_lru"
    ] |> Enum.map(&create_rebar_file/1)
  end

  defp create_rebar_file(app_dir) do
    IO.puts Path.expand("#{app_dir}/rebar.config", @src_dir)
    File.touch(Path.expand("#{app_dir}/rebar.config", @src_dir))
  end

end
