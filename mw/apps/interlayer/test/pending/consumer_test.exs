defmodule ConsumerTest do
  use ExUnit.Case
  doctest Interlayer.Pending.Consumer

  import Mock

  test "handle single message without changing Consumer's state" do
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
      ])
    do
      {:noreply, [], :ok} =
        Interlayer.Pending.Consumer.handle_events ["valim"], nil, :ok
      receive do
        :rm -> nil
      end
    end
  end

  test "handle a couple of messages without changing Consumer's state" do
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
      ])
    do
      {:noreply, [], :ok} =
        Interlayer.Pending.Consumer.handle_events ["deadmau5", "diplo"], nil, :ok
      receive do
        :rm -> nil
      end
      receive do
        :rm -> nil
      end
    end
  end

  test "handle no messages without changing Consumer's state" do
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
      ])
    do
      {:noreply, [], :ok} =
        Interlayer.Pending.Consumer.handle_events [], nil, :ok
    end
  end
end
