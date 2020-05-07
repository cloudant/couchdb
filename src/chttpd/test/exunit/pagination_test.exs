defmodule Couch.Test.Pagination do
  use ExUnit.Case
  import Couch.DBTest, only: [retry_until: 1]
  alias Couch.DBTest, as: Utils

  defp create_admin(user_name, password) do
    hashed = String.to_charlist(:couch_passwords.hash_admin_password(password))
    :config.set('admins', String.to_charlist(user_name), hashed, false)
  end

  defp base_url() do
    addr = :config.get('chttpd', 'bind_address', '127.0.0.1')
    port = :mochiweb_socket_server.get(:chttpd, :port)
    "http://#{addr}:#{port}"
  end

  setup_all do
    test_ctx = :test_util.start_couch([:chttpd])
    :ok = create_admin("adm", "pass")

    on_exit(fn ->
      :test_util.stop_couch(test_ctx)
    end)

    %{
      base_url: base_url(),
      user: "adm",
      pass: "pass"
    }
  end

  defp with_session(context) do
    session = Couch.login(context.user, context.pass, base_url: context.base_url)
    %{session: session}
  end

  defp random_db(context) do
    db_name = Utils.random_db_name("db")

    on_exit(fn ->
      delete_db(context.session, db_name)
    end)

    create_db(context.session, db_name)
    %{db_name: db_name}
  end

  defp with_docs(context) do
    assert Map.has_key?(context, :n_docs), "Please define '@describetag n_docs: 10'"
    %{docs: create_docs(context.session, context.db_name, 1..context.n_docs)}
  end

  defp all_docs(context) do
    assert Map.has_key?(context, :page_size), "Please define '@describetag page_size: 4'"

    assert Map.has_key?(context, :descending),
           "Please define '@describetag descending: false'"

    resp =
      Couch.Session.get(context.session, "/#{context.db_name}/_all_docs",
        query: %{page_size: context.page_size, descending: context.descending}
      )

    assert resp.status_code == 200

    %{
      response: resp.body
    }
  end

  defp paginate(context) do
    if Map.has_key?(context.response, "next") do
      bookmark = context.response["next"]
      pages = Map.get(context, :pages, [context.response])
      assert length(pages) < div(context.n_docs, context.page_size) + 1

      resp =
        Couch.Session.get(context.session, "/#{context.db_name}/_all_docs",
          query: %{bookmark: bookmark}
        )

      context =
        Map.merge(context, %{
          pages: [resp.body | pages],
          response: resp.body
        })

      paginate(context)
    else
      context =
        Map.update(context, :pages, [], fn acc ->
          Enum.reverse(acc)
        end)

      context
    end
  end

  def create_db(session, db_name, opts \\ []) do
    retry_until(fn ->
      resp = Couch.Session.put(session, "/#{db_name}", opts)
      assert resp.status_code in [201, 202]
      assert resp.body == %{"ok" => true}
      {:ok, resp}
    end)
  end

  defp delete_db(session, db_name) do
    retry_until(fn ->
      resp = Couch.Session.delete(session, "/#{db_name}")
      assert resp.status_code in [200, 202, 404]
      {:ok, resp}
    end)
  end

  defp create_doc(session, db_name, body) do
    {:ok, body} =
      retry_until(fn ->
        resp = Couch.Session.post(session, "/#{db_name}", body: body)
        assert resp.status_code in [201, 202]
        assert resp.body["ok"]
        {:ok, resp.body}
      end)

    Map.delete(body, "ok")
  end

  defp create_docs(session, db_name, range) do
    # docs = Utils.make_docs(range)
    docs = make_docs(range)

    docs
    |> Enum.map(fn doc ->
      create_doc(session, db_name, doc)
    end)
  end

  defp docid(id) do
    id |> Integer.to_string() |> String.pad_leading(3, "0")
  end

  defp make_docs(id_range) do
    for id <- id_range do
      str_id = docid(id)
      %{"_id" => str_id, "integer" => id, "string" => str_id}
    end
  end

  describe "Pagination API (10 docs)" do
    @describetag n_docs: 10
    @describetag page_size: 4
    setup [:with_session, :random_db, :with_docs]

    test ": _all_docs?page_size=4", ctx do
      %{session: session, db_name: db_name} = ctx

      resp =
        Couch.Session.get(session, "/#{db_name}/_all_docs",
          query: %{page_size: ctx.page_size}
        )

      assert resp.status_code == 200
    end
  end

  for descending <- [false, true] do
    describe "Legacy API (10 docs) : _all_docs?descending=#{descending}" do
      @describetag n_docs: 10
      @describetag descending: descending
      setup [:with_session, :random_db, :with_docs]

      test "total_rows matches the length of rows array", ctx do
        resp =
          Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
            query: %{descending: ctx.descending}
          )

        assert resp.status_code == 200
        body = resp.body
        assert body["total_rows"] == length(body["rows"])
      end

      test "the rows are correctly sorted", ctx do
        resp =
          Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
            query: %{descending: ctx.descending}
          )

        assert resp.status_code == 200
        body = resp.body
        ids = Enum.map(body["rows"], fn row -> row["id"] end)

        if ctx.descending do
          assert Enum.reverse(Enum.sort(ids)) == ids
        else
          assert Enum.sort(ids) == ids
        end
      end

      test "start_key is respected", ctx do
        head_pos = 2
        tail_pos = ctx.n_docs - head_pos
        doc_ids = Enum.map(ctx.docs, fn doc -> doc["id"] end)

        {start_pos, doc_ids} =
          if ctx.descending do
            {head_pos, Enum.reverse(Enum.drop(Enum.sort(doc_ids), -tail_pos))}
          else
            {tail_pos, Enum.drop(Enum.sort(doc_ids), tail_pos - 1)}
          end

        start_key = ~s("#{docid(start_pos)}")

        resp =
          Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
            query: %{descending: ctx.descending, start_key: start_key}
          )

        assert resp.status_code == 200
        ids = Enum.map(resp.body["rows"], fn row -> row["id"] end)
        assert doc_ids == ids
      end

      test "end_key is respected", ctx do
        head_pos = 2
        tail_pos = ctx.n_docs - head_pos
        doc_ids = Enum.map(ctx.docs, fn doc -> doc["id"] end)

        {end_pos, doc_ids} =
          if ctx.descending do
            {tail_pos, Enum.reverse(Enum.drop(Enum.sort(doc_ids), tail_pos - 1))}
          else
            {head_pos, Enum.drop(Enum.sort(doc_ids), -tail_pos)}
          end

        end_key = ~s("#{docid(end_pos)}")

        resp =
          Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
            query: %{descending: ctx.descending, end_key: end_key}
          )

        assert resp.status_code == 200
        ids = Enum.map(resp.body["rows"], fn row -> row["id"] end)
        assert doc_ids == ids
      end
    end
  end

  for descending <- [false, true] do
    for n <- [4, 9] do
      describe "Pagination API (10 docs) : _all_docs?page_size=#{n}&descending=#{
                 descending
               }" do
        @describetag n_docs: 10
        @describetag descending: descending
        @describetag page_size: n
        setup [:with_session, :random_db, :with_docs, :all_docs]

        test "should return 'next' bookmark", ctx do
          body = ctx.response
          assert Map.has_key?(body, "next")
        end

        test "total_rows matches the length of rows array", ctx do
          body = ctx.response
          assert body["total_rows"] == length(body["rows"])
        end

        test "total_rows matches the requested page_size", ctx do
          body = ctx.response
          assert body["total_rows"] == ctx.page_size
        end

        test "can use 'next' bookmark to get remaining results", ctx do
          bookmark = ctx.response["next"]

          resp =
            Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
              query: %{bookmark: bookmark}
            )

          assert resp.status_code == 200
          body = resp.body
          assert body["total_rows"] == length(body["rows"])
          assert body["total_rows"] <= ctx.page_size
        end

        test "start_key is respected", ctx do
          head_pos = 2
          tail_pos = ctx.n_docs - head_pos
          doc_ids = Enum.map(ctx.docs, fn doc -> doc["id"] end)

          {start_pos, doc_ids} =
            if ctx.descending do
              {head_pos, Enum.reverse(Enum.drop(Enum.sort(doc_ids), -tail_pos))}
            else
              {tail_pos, Enum.drop(Enum.sort(doc_ids), tail_pos - 1)}
            end

          start_key = ~s("#{docid(start_pos)}")

          resp =
            Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
              query: %{descending: ctx.descending, start_key: start_key}
            )

          assert resp.status_code == 200
          ids = Enum.map(resp.body["rows"], fn row -> row["id"] end)
          assert doc_ids == ids
        end

        test "end_key is respected", ctx do
          head_pos = 2
          tail_pos = ctx.n_docs - head_pos
          doc_ids = Enum.map(ctx.docs, fn doc -> doc["id"] end)

          {end_pos, doc_ids} =
            if ctx.descending do
              {tail_pos, Enum.reverse(Enum.drop(Enum.sort(doc_ids), tail_pos - 1))}
            else
              {head_pos, Enum.drop(Enum.sort(doc_ids), -tail_pos)}
            end

          end_key = ~s("#{docid(end_pos)}")

          resp =
            Couch.Session.get(ctx.session, "/#{ctx.db_name}/_all_docs",
              query: %{descending: ctx.descending, end_key: end_key}
            )

          assert resp.status_code == 200
          ids = Enum.map(resp.body["rows"], fn row -> row["id"] end)
          assert doc_ids == ids
        end
      end
    end
  end

  for descending <- [false, true] do
    for n <- [4, 9] do
      describe "Pagination API (10 docs) : _all_docs?page_size=#{n}&descending=#{
                 descending
               } : pages" do
        @describetag n_docs: 10
        @describetag descending: descending
        @describetag page_size: n
        setup [:with_session, :random_db, :with_docs, :all_docs, :paginate]

        test "final page doesn't include 'next' bookmark", ctx do
          assert not Map.has_key?(ctx.response, "next")
          assert ctx.response["total_rows"] == rem(ctx.n_docs, ctx.page_size)
        end

        test "each but last page has page_size rows", ctx do
          pages = Enum.drop(ctx.pages, -1)

          assert Enum.all?(pages, fn resp ->
                   length(resp["rows"]) == ctx.page_size
                 end)
        end

        test "sum of rows on all pages is equal to number of documents", ctx do
          pages = ctx.pages
          n = Enum.reduce(pages, 0, fn resp, acc -> acc + length(resp["rows"]) end)
          assert n == ctx.n_docs
        end

        test "the rows are correctly sorted", ctx do
          pages = ctx.pages

          ids =
            Enum.reduce(pages, [], fn resp, acc ->
              acc ++ Enum.map(resp["rows"], fn row -> row["id"] end)
            end)

          if ctx.descending do
            assert Enum.reverse(Enum.sort(ids)) == ids
          else
            assert Enum.sort(ids) == ids
          end
        end
      end
    end
  end

  for n <- 10..11 do
    describe "Pagination API (10 docs) : _all_docs?page_size=#{n}" do
      @describetag n_docs: 10
      @describetag descending: false
      @describetag page_size: n
      setup [:with_session, :random_db, :with_docs, :all_docs]

      test "should not return 'next' bookmark", ctx do
        body = ctx.response
        assert not Map.has_key?(body, "next")
      end

      test "total_rows matches the length of rows array", ctx do
        body = ctx.response
        assert body["total_rows"] == length(body["rows"])
      end

      test "total_rows less than the requested page_size", ctx do
        body = ctx.response
        assert body["total_rows"] <= ctx.page_size
      end
    end
  end
end
