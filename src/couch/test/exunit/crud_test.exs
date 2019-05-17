defmodule Couch.Test.CRUD do
  use ExUnit.Case
  alias Couch.Test.Adapter
  alias Couch.Test.Utils, as: Utils

  alias Couch.Test.Setup

  require Record

  test_groups = [
    "using Clustered API": {Adapter.Clustered, %{service: :chttpd}},
    "using Backdoor API": {Adapter.Backdoor, %{service: :couch_httpd}},
    "using Fabric API": {Adapter.Fabric, %{service: nil}}
  ]

  #  for {describe, {adapter, args}} <- test_groups do
  #    describe "Database CRUD #{describe}" do
  #      @describetag adapter: adapter, adapter_args: args
  #      test "Create", %{adapter: adapter, adapter_args: args} do
  #        with ctx when Record.is_record(ctx, :test_context) <-
  #               :test_util.start_couch([:chttpd]),
  #             # FIXME
  #             %{} = adapter <- adapter.new("Utils.connection_url(args.service)"),
  #             # FIXME
  #             :ok <- Utils.add_admin("adm", "pass"),
  #             %{} = adapter <- Adapter.login(adapter, "adm", "pass") do
  #          db_name = Utils.random_name("db")
  #          assert {:ok, resp} = Adapter.create_db(adapter, db_name)
  #          assert resp.body["ok"]
  #          # TODO query all dbs to make sure db_name is added
  #        else
  #          resp -> assert :ok = resp
  #        end
  #      end
  #    end
  #  end

  for {describe, {adapter, args}} <- test_groups do
    describe "Database CRUD2 #{describe}" do
      @describetag adapter: adapter, adapter_args: args
      test "Create", %{adapter: adapter, adapter_args: args} do
        db_name = Utils.random_name("db")

        ctx =
          %Setup{}
          |> Setup.Start.new([:chttpd])
          |> Setup.Adapter.new(adapter)
          |> Setup.Admin.new(user: "adm", password: "pass")
          |> Setup.Login.new(user: "adm", password: "pass")
          # |> Create.DB(db_name)
          |> Setup.run()

        assert {:ok, resp} = Adapter.create_db(Setup.get(ctx, :adapter), db_name)
        assert resp.body["ok"]
        # TODO query all dbs to make sure db_name is added
      end
    end
  end
end
