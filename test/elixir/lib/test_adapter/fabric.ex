defmodule Couch.Test.Adapter.Fabric do
  @moduledoc "Fabric API testing adapter type"
  defstruct [:connection_str, :session]

  def new() do
    %Couch.Test.Adapter.Fabric{}
  end
end

defimpl Couch.Test.Adapter, for: Couch.Test.Adapter.Fabric do
  import ExUnit.Assertions

  import Couch.DBTest,
    only: [
      retry_until: 1,
      retry_until: 2,
      retry_until: 3
    ]

  @moduledoc "Implements Fabric API testing adapter"
  def login(adapter, user, pass) do
    adapter
  end

  def create_user(adapter, user) do
    if user == [] do
      user = Couch.Test.random_name("user")
    end

    user_doc = Couch.Test.Adapter.Shared.format_user_doc(user)
    # TODO
    doc = :couch_doc.from_json_obj(user_doc)
    assert {:ok, resp} = :fabric.update_doc("_users", doc, [])
    {:ok, resp}
  end

  def create_db(adapter, db_name, opts \\ []) do
    # TODO opts will be different for every adapter type
    assert :ok = :fabric.create_db(db_name, opts)
    {:ok, %{body: %{"ok" => true}}}
  end

  def delete_db(adapter, db_name) do
    assert :ok = :fabric.delete_db(db_name)
    {:ok, :ok}
  end

  def create_doc(adapter, db_name, body) do
    doc = :couch_doc.from_json_obj(body)
    assert {:ok, resp} = :fabric.update_doc(db_name, doc, [])
    {:ok, resp}
  end

  def open_doc(adapter, db_name, doc_name) do
    assert {:ok, resp} = :fabric.open_doc(db_name, doc_name, [])
    {:ok, resp}
  end

  def bulk_save(adapter, db_name, docs) do
    # TODO
  end

  def query(
        adapter,
        db_name,
        map_fun,
        reduce_fun \\ nil,
        options \\ nil,
        keys \\ nil,
        language \\ "javascript"
      ) do
    {view_options, request_options} =
      if options != nil and Map.has_key?(options, :options) do
        {options.options, Map.delete(options, :options)}
      else
        {nil, options}
      end

    ddoc =
      Couch.Test.Adapter.Shared.format_query_ddoc(
        map_fun,
        reduce_fun,
        language,
        view_options
      )

    request_options =
      if keys != nil and is_list(keys) do
        Map.merge(request_options || %{}, %{:keys => :jiffy.encode(keys)})
      else
        request_options
      end

    resp =
      Couch.put(
        "/#{db_name}/#{ddoc._id}",
        headers: ["Content-Type": "application/json"],
        body: ddoc
      )

    assert resp.status_code == 201

    # TODO transform resp
    resp = :fabric.query_view(db_name, ddoc._id, "view")

    adapter.delete_doc(db_name, ddoc._id)

    {:ok, resp}
  end

  def set_config(adapter, section, key, val) do
    :config.set(section, key, String.to_charlist(val), false)
  end
end
