defmodule CollectorTest do
  use ExUnit.Case, async: false
  doctest BrokerToAs.Collector

  alias BrokerToAs.Utils.Message, as: Message

  test "TODO: find a name to this test" do
    payload_1 = "Come here brochacho"
    # TODO: Update when topics will be decided
    topic_1 = "application.enter.traveller=1.node=3"
    meta_1 = "Dillon Francis"
    unfolded_msg_1 = %Message{payload: payload_1, topic: topic_1}
    {:ok, msg_1} = JSX.encode unfolded_msg_1

    payload_2 = "Le domeniche d'agosto quanta neve che cadra'"
    # TODO: Update when topics will be decided
    topic_2 = "middleware.boot.node=7"
    meta_2 = "Immenso Gigi"
    unfolded_msg_2 = %Message{payload: payload_2, topic: topic_2}
    {:ok, msg_2} = JSX.encode unfolded_msg_2

    messages = [ { msg_1, meta_1 }, { msg_2, meta_2} ]

    {:ok, conn} = AMQP.Connection.open(host: host(), port: port())
    {:ok, chan} = AMQP.Channel.open conn

    AMQP.Exchange.declare chan, broker_as_xch(), :topic
    {:ok, %{queue: queue}} =
      AMQP.Queue.declare(chan, "", exclusive: true)
    AMQP.Queue.bind chan, queue, broker_as_xch(), routing_key: "application.#"

    BrokerToAs.Collector.handle_events messages, nil, chan

    AMQP.Basic.consume(chan, queue, nil, no_ack: true)

    receive do
      {:basic_deliver, payload, _meta} ->
        assert msg_1 == payload
    end
  end

  defp host do
    Application.get_env(:broker_to_as, :send_host)
  end

  defp port do
    Application.get_env(:broker_to_as, :send_port)
  end

  defp broker_as_xch do
    Application.get_env(:broker_to_as, :broker_as_xch)
  end
end
