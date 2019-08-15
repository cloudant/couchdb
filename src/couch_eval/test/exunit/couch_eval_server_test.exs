defmodule EvalStub do
  
end


defmodule CouchEvalServerTest do
  use Couch.Test.ExUnit.Case

  alias Couch.Test.Utils

  alias Couch.Test.Setup

  alias Couch.Test.Setup.Step

  def with_apps(context, setup) do
    setup = setup
      |> Setup.Step.Start.new(:start, extra_apps: [:couch_eval])
      |> Setup.run()

    :meck.new(:config)
    :meck.expect(:config, :get, fn('couch_eval', 'languages', _lang) -> 
        'EvalStub'
      end)

    on_exit(fn ->
      :meck.unload()
    end)
    
    {context, setup}
  end

  @tag setup: &__MODULE__.with_apps/2
  test "should create context and return it", context do
    ctx = :couch_eval.get_map_context("db", "ddoc", "language", "sig", "lib", [%{id => "1", def => "def"}])

    %{context_id: context_id} = ctx
    assert context_id == "dbsig" 
  end
end
