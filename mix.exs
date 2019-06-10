defmodule CouchDBTest.Mixfile do
  use Mix.Project

  def project do
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
      consolidate_protocols: Mix.env() not in [:test, :dev],
      test_paths: get_test_paths(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["test/elixir/lib", "test/elixir/test/support"]
  defp elixirc_paths(_), do: ["test/elixir/lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:httpotion, "~> 3.0", only: [:dev, :test], runtime: false},
      {:jiffy, "~> 0.15.2", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.0.0", only: [:dev, :test], runtime: false}
    ]
  end

  def get_test_paths do
    Path.wildcard("src/*/test/exunit") |> Enum.filter(&File.dir?/1)
  end
end
