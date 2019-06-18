# If build number detected assume we running on Jenkins
# and skip certain tests that fail on jenkins.
exclude =
  case System.get_env("BUILD_NUMBER") !== nil do
    true -> [pending: true, skip_on_jenkins: true]
    false -> [pending: true]
  end

ExUnit.configure(
  exclude: exclude,
  formatters: [JUnitFormatter, ExUnit.CLIFormatter]
)

{:ok, _started} = Application.ensure_all_started(:httpotion)
ExUnit.start()
Code.require_file("partition_helpers.exs", __DIR__)
Code.require_file("reshard_helpers.exs", __DIR__)
