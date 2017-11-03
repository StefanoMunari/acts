defmodule RabbitReceiverTest do
  use ExUnit.Case
  doctest Incoming.Proxy.RabbitReceiver

  import Mock

  alias Utils.Message, as: Message
  alias Utils.Message.MessageCodec, as: MessageCodec
  alias Utils.Routing.Info, as: RoutingInfo

  test "passes message to dispatcher" do
    pid = self()
    with_mock Incoming.Router,
      [
        handle_events:
          fn(messages, _from, state) ->
            for message <- messages do
              send pid, {:msg, message}
            end
            {:noreply, [], state}
          end,
        terminate: fn(_, _) -> nil end
      ]
    do
      payload   = "Elementary, my dear Watson. [Sherlock]"
      recipient = RoutingInfo.whoami()
      topic     = "yellow"
      message   = %Message{payload: payload, recipient: recipient, topic: topic}
      encoded   = MessageCodec.encode message
      sender    = "1"

      {:ok, conn} = AMQP.Connection.open(host: host(), port: port())
      {:ok, chan} = AMQP.Channel.open conn

      queue_name = sender <> "to" <> RoutingInfo.whoami()
      AMQP.Queue.declare(chan, queue_name, durable: true)

      AMQP.Basic.publish(chan, "", queue_name, encoded, persistent: true)

      AMQP.Connection.close conn
    end
  end

  defp host do
    Application.get_env(:incoming, :rec_host)
  end

  defp port do
    Application.get_env(:incoming, :rec_port)
  end
end
