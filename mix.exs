defmodule CouchDBTest.Mixfile do
  use Mix.Project

  def project do
    # ets_lru uses REBAR environment
    System.put_env("REBAR", Path.expand("~/.mix/rebar"))
    [
      app: :couchdbtest,
      version: "0.1.0",
      elixir: "~> 1.5",
      lockfile: Path.expand("mix.lock", __DIR__),
      deps_path: Path.expand("src", __DIR__),
      build_path: Path.expand("_build", __DIR__),
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
      extra_applications: [:logger],
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
      {:httpotion, "~> 3.0", only: [:dev, :test, :integration], runtime: false},
      {:credo, "~> 1.0.0", only: [:dev, :test, :integration], runtime: false},
      # Indepdent Apps
      {:config, git: "https://github.com/apache/couchdb-config", tag: "2.1.6"},
      {:b64url, git: "https://github.com/apache/couchdb-b64url", tag: "1.0.1"},
      {:ets_lru, git: "https://github.com/apache/couchdb-ets-lru", tag: "1.0.0"},
      {:khash, git: "https://github.com/apache/couchdb-khash", tag: "1.0.1"},
      {:snappy, git: "https://github.com/apache/couchdb-snappy", tag: "CouchDB-1.0.2"},
      {:ioq, git: "https://github.com/apache/couchdb-ioq", tag: "2.1.2"},
      {:hqueue, git: "https://github.com/apache/couchdb-hqueue", tag: "1.0.1"},
      {:smoosh, git: "https://github.com/apache/couchdb-ioq", tag: "1.0.1"},
      {:ken, git: "https://github.com/apache/couchdb-ken", tag: "1.0.3"},
      # Non-Erlang deps
      {:docs, git: "https://github.com/apache/couchdb-documentation", tag: "2.3.0", compile: false},
      {:fauxton, git: "https://github.com/apache/couchdb-fauxton", tag: "v1.1.19", compile: false},
      # Third party deps
      {:folsom, git: "https://github.com/apache/couchdb-folsom", tag: "CouchDB-0.8.3"},
      {:hyper, git: "https://github.com/apache/couchdb-hyper", tag: "CouchDB-2.2.0-4"},
      {:ibrowse, git: "https://github.com/apache/couchdb-ibrowse", tag: "CouchDB-4.0.1-1", override: true},
      {:jiffy, git: "https://github.com/apache/couchdb-jiffy", tag: "CouchDB-0.14.11-2"},
      {:mochiweb, git: "https://github.com/apache/couchdb-mochiweb", tag: "v2.19.0"},
      {:meck, git: "https://github.com/apache/couchdb-meck.git", tag: "0.8.8"},
      # rebar3 plugins
      {:grpcbox_plugin, "~> 0.7.0"},
      # grpc
      {:grpcbox, "~> 0.11.0"}
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
end