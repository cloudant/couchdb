defmodule Couch.Test.Utils do
  @moduledoc "Helper functions for testing"
  def random_name(prefix) do
    time = :erlang.monotonic_time()
    umi = :erlang.unique_integer([:monotonic])
    "#{prefix}-#{time}-#{umi}"
  end
  def connection_url(service) do
    if service do
      addr = :config.get(Atom.to_charlist(service), 'bind_address', '127.0.0.1')
      port = :mochiweb_socket_server.get(service, :port)  
      "http://#{addr}:#{port}"
    end
  end
  def add_admin(admin_name, pass) do
    :config.set("admins", "adm", String.to_charlist("pass"), false)
  end

end