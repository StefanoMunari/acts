defmodule DispatcherTest do
  use ExUnit.Case
  doctest Incoming.Dispatcher

  import Mock

  alias Utils.Message, as: Message

  test "dispatches a message to snapshot module" do
    pid = self()
    with_mock Incoming.Sandbox.Snapshot,
      [
        handle_message:
          fn(msg) ->
            send pid, {:msg, msg}
            :ok
          end
      ]
    do
      payload   = "Consistency is the key to accuracy."
      recipient = "2"
      topic     = "snapshot"
      message = %Message{payload: payload, recipient: recipient, topic: topic}
      Incoming.Dispatcher.handle_events [{message, {}}], nil, :ok

      receive do
        {:msg, msg} ->
          assert msg == message
      end

    end
  end

  test "dispatches a message to interlayer module" do
    pid = self()
    with_mock Incoming.Sandbox.Interlayer,
      [
        handle_message:
          fn(msg) ->
            send pid, {:msg, msg}
            :ok
          end
      ]
    do
      payload   = "Even your wrist watch has 2 cores."
      recipient = "2"
      topic     = "application.pippo.goes.to.the.bar"
      message = %Message{payload: payload, recipient: recipient, topic: topic}
      Incoming.Dispatcher.handle_events [{message, {}}], nil, :ok

      receive do
        {:msg, msg} ->
          assert msg == message
      end

    end
  end

end
