defmodule MwAppInterfaceTest do
  use ExUnit.Case
  doctest Interlayer.MwAppInterface

  import Mock

  test "loads message in the pub/sub mw to app system" do
    pid = self()
    with_mocks([
      {
        Persistence.MwAppMessageDb,
        [],
        [
          get_all_messages: fn -> [] end,
          store_msg: fn msg -> send pid, {:store, msg} end,
          remove: fn -> send pid, :rm end
        ]
      }
    ]) do
      :ok =
        Interlayer.MwAppInterface.sync_to_application "erlang_the_movie"
      receive do
        {:store, msg} ->
          assert msg == "erlang_the_movie"
      end
    end
  end
end
