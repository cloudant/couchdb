defmodule :evalstub do
  def create_map_context(ctx_id, _language, sig, _lib, _map_funs) do
    {:ok,
     %{
       context_id: ctx_id,
       sig: sig
     }}
  end

  def destroy_context(_ctx) do
    :ok
  end
end

defmodule CouchEvalServerTest do
  use Couch.Test.ExUnit.Case

  alias Couch.Test.Setup

  def with_apps(context, setup) do
    setup =
      setup
      |> Setup.Step.Start.new(:start, extra_apps: [:couch_eval])
      |> Setup.run()

    old_eval = :config.get('couch_eval', 'languages')
    :config.set('couch_eval', 'languages', 'evalstub')

    on_exit(fn ->
      :config.set('couch_eval', 'languages', old_eval)
    end)

    {context, setup}
  end

  @moduletag setup: &__MODULE__.with_apps/2

  test "should create context and return it", _context do
    {:ok, ctx} =
      :couch_eval.get_map_context("db1", "ddoc", "language", "sig1", "lib", [
        %{id: "1", def: "def"}
      ])

    %{id: context_id} = ctx
    assert context_id == "db1sig1"
  end

  test "adds info to all the correct ets tables", _context do
    {:ok, ctx} =
      :couch_eval.get_map_context("db2", "ddoc", "language", "sig2", "lib", [
        %{id: "1", def: "def"}
      ])

    %{
      :id => id
    } = ctx

    [found_ctx] = :ets.lookup(:couch_eval_contexts, id)

    assert elem(found_ctx, 1) == id

    assert length(:ets.lookup(:couch_eval_active, self())) == 1
    assert length(:ets.lookup(:couch_eval_waiters, self())) == 0

    :ok = :couch_eval.return_context(ctx)

    assert length(:ets.lookup(:couch_eval_active, self())) == 0

    [found_ctx1] = :ets.lookup(:couch_eval_contexts, id)

    assert elem(found_ctx1, 2) == :idle
  end

  test "adds 2nd request to waiting queue and then returns later" do
    task1 =
      Task.async(fn ->
        {:ok, ctx} =
          :couch_eval.get_map_context("db3", "ddoc", "language", "sig3", "lib", [
            %{id: "1", def: "def"}
          ])

        receive do
          :return -> :ok
        end

        :couch_eval.return_context(ctx)
      end)

    %{
      pid: pid1
    } = task1

    wait_to_be_added_to_queue(:couch_eval_active, pid1)

    task2 =
      Task.async(fn ->
        :couch_eval.get_map_context("db3", "ddoc", "language", "sig3", "lib", [
          %{id: "1", def: "def"}
        ])
      end)

    %{
      pid: pid2
    } = task2

    wait_to_be_added_to_queue(:couch_eval_waiters, pid2)

    send(pid1, :return)

    Task.await(task1)
    Task.await(task2)
  end

  test "crashed process causes context to be deleted" do
    ctx_id = "db4sig4"
    task1 =
      Task.async(fn ->
        {:ok, _ctx} =
          :couch_eval.get_map_context("db4", "ddoc", "language", "sig4", "lib", [
            %{id: "1", def: "def"}
          ])

        receive do
          :return -> :ok
        end
      end)

    %{
      pid: pid1
    } = task1

    wait_to_be_added_to_queue(:couch_eval_active, pid1)

    Process.unlink(pid1)
    Process.exit(pid1, :testing)
    wait_to_be_removed_from(:couch_eval_contexts, ctx_id)
    assert :ets.lookup(:couch_eval_contexts, ctx_id) == []
  end

  test "crashed process with waiter is recreated" do
    ctx_id = "db5sig5"
    task1 =
      Task.async(fn ->
        {:ok, _ctx} =
          :couch_eval.get_map_context("db5", "ddoc", "language", "sig5", "lib", [
            %{id: "1", def: "def"}
          ])
        IO.inspect "done"
        receive do
          :return -> :ok
        end
        IO.inspect "done 2"
      end)

    %{
      pid: pid1
    } = task1

    wait_to_be_added_to_queue(:couch_eval_active, pid1)

    task2 =
      Task.async(fn ->
        {:ok, ctx} =
          :couch_eval.get_map_context("db5", "ddoc", "language", "sig5", "lib", [
            %{id: "1", def: "def"}
          ])
          receive do
            :return -> :ok
          end
        :couch_eval.return_context(ctx)
        IO.inspect "tak2 done"
      end)

    %{
      pid: pid2
    } = task2

    wait_to_be_added_to_queue(:couch_eval_waiters, pid2)

    Process.unlink(pid1)
    Process.exit(pid1, :killed_for_testing)
    IO.inspect "killed"
    wait_to_be_added_to_queue(:couch_eval_active, pid2)
    assert :ets.lookup(:couch_eval_contexts, ctx_id) != []

    send(pid2, :return)
    Task.await(task2)
  end

  def wait_to_be_added_to_queue(queue, pid) do
    IO.inspect :ets.lookup(queue, pid)
    IO.inspect queue
    case :ets.lookup(queue, pid) do
      [] ->
        :timer.sleep(100)
        wait_to_be_added_to_queue(queue, pid)

      _ ->
        :ok
    end
  end

  def wait_to_be_removed_from(table_id, id) do
    case :ets.lookup(table_id, id) do
      [_Item] ->
        :timer.sleep(100)
        wait_to_be_removed_from(table_id, id)
      [] ->
        :ok
    end
  end
end
