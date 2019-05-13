defmodule Couch.Test.Adapter.Clustered do
  @moduledoc "Clustered API testing adapter type"
  defstruct [:connection_str, :session]
  def new(connection_str) do
    addr = :config.get("chttpd", "bind_address", "127.0.0.1")
    port = :mochiweb_socket_server.get(:chttpd, :port)  
    %Couch.Test.Adapter.Clustered{
      connection_str: "https://#{addr}:#{port}"
    }
  end
end

defimpl Couch.Test.Adapter, for: Couch.Test.Adapter.Clustered do
  import ExUnit.Assertions
  import Couch.DBTest, only: [
    retry_until: 1,
    retry_until: 2,
    retry_until: 3,
  ]
  use HTTPotion.Base
  
  @moduledoc "Implements Clustered API testing adapter"

  def process_request_headers(headers, options) do
    Couch.process_request_headers(headers, options)
  end

  def process_options(options) do
    Couch.process_options(options)
  end
  def process_request_body(body) do
    Couch.process_request_body(body)
  end
  def process_response_body(headers, body) do
    Couch.process_response_body(headers, body)
  end
  def handle_response(response) do
    Couch.handle_response(response)
  end
  defp process_arguments(method, url, options) do
    Couch.process_arguments(method, url, options)
  end
  

  def process_url(url) do
    # FIXME this woulod work only if we run on the same VM
    addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:chttpd, :port)
    "http://#{addr}:#{port}" <> url
  end
  def login(adapter, user, pass) do
    resp = post("/_session", body: %{:username => user, :password => pass})
    true = resp.body["ok"]
    cookie = resp.headers[:"set-cookie"]
    [token | _] = String.split(cookie, ";")
    session = %Couch.Session{cookie: token}
    %{adapter | session: session}
  end
  def create_user(adapter, user) do
    assert adapter.session, "Requires login"
    if user == [] do
      user = Couch.Test.random_name("user")
    end
    user_doc = Couch.Test.Adapter.Shared.format_user_doc(user)

    resp = get("/_users/#{user_doc["_id"]}")

    user_doc =
      case resp.status_code do
        404 ->
          user_doc

        sc when sc >= 200 and sc < 300 ->
          Map.put(user_doc, "_rev", resp.body["_rev"])
      end

    resp = post("/_users", body: user_doc)
    assert HTTPotion.Response.success?(resp)
    assert resp.body["ok"]
    Map.put(user_doc, "_rev", resp.body["rev"])
  end

  def create_db(adapter, db_name, opts \\ []) do
    assert adapter.session, "Requires login"
    retry_until(fn ->
      resp = adapter.session.put("/#{db_name}", opts)
      assert resp.status_code in [201, 202]
      assert resp.body == %{"ok" => true}
      {:ok, resp}
    end)
  end

  def delete_db(adapter, db_name) do
    assert adapter.session, "Requires login"
    resp = adapter.session.delete("/#{db_name}")
    assert resp.status_code in [200, 202, 404]
    {:ok, resp}
  end

  def create_doc(adapter, db_name, body) do
    resp = adapter.session.post("/#{db_name}", body: body)
    assert resp.status_code in [201, 202]
    assert resp.body["ok"]
    {:ok, resp}
  end

  def open_doc(adapter, db_name, doc_name) do
    resp = adapter.session.get("/#{db_name}/#{doc_name}")
    assert resp.status_code in [200]
    {:ok, resp.body}
  end

  def delete_doc(adapter, db_name, doc_id, rev \\ nil) do
    # Doc#doc{deleted = true}
  end

  def bulk_save(adapter, db_name, docs) do
    resp =
      adapter.session.post(
        "/#{db_name}/_bulk_docs",
        body: %{
          docs: docs
        }
      )

    assert resp.status_code == 201
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
    ddoc = Couch.Test.Adapter.Shared.format_query_ddoc(
      map_fun, reduce_fun, language, view_options)

    request_options =
      if keys != nil and is_list(keys) do
        Map.merge(request_options || %{}, %{:keys => :jiffy.encode(keys)})
      else
        request_options
      end

    resp =
      put(
        "/#{db_name}/#{ddoc._id}",
        headers: ["Content-Type": "application/json"],
        body: ddoc
      )

    assert resp.status_code == 201

    resp = get("/#{db_name}/#{ddoc._id}/_view/view", query: request_options)
    assert resp.status_code == 200

    delete("/#{db_name}/#{ddoc._id}")

    resp.body    
  end

  def set_config(adapter, section, key, val) do
    url = "#{adapter.connection_str}/#{section}/#{key}"
    headers = ["X-Couch-Persist": "false"]
    resp = adapter.session.put(url, headers: headers, body: :jiffy.encode(val))
    resp.body
  end
  
end
