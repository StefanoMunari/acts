defmodule RabbitSenderTest do
  use ExUnit.Case
  doctest Forwarding.Proxy.RabbitSender

  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  # IMPORTANT: Use neighbors "1" and "2" for sender tests

  test "forwards messages to another middleware" do
    payload   = "We're running out of money [Quackmore]"
    recipient = "1"
    topic     = "scrooge"
    msg = %Message{payload: payload, recipient: recipient, topic: topic}

    {:ok, conn} = AMQP.Connection.open(host: host(), port: port())
    {:ok, chan} = AMQP.Channel.open conn

    # test MW to Broker communication
    AMQP.Exchange.declare(chan, mw_broker_xch(), :fanout)
    {:ok, %{queue: broker_queue}} =
      AMQP.Queue.declare(chan, "", exclusive: true)
    AMQP.Queue.bind(chan, broker_queue, mw_broker_xch())


    Forwarding.Proxy.RabbitSender.forward msg

    AMQP.Basic.consume(chan, broker_queue, nil, no_ack: true)

    receive do
      {:basic_deliver, _payload, _meta} ->
        expected_payload = payload <> "--++--" <> topic
        assert _payload = expected_payload
    end



    # test MW to MW communication
    queue_name = RoutingInfo.whoami() <> "to" <> "1"
    AMQP.Queue.declare(chan, queue_name, durable: true)

    AMQP.Basic.consume(chan, queue_name)

    receive do
      {:basic_deliver, _payload, rec_meta} ->
        assert _payload = payload
        AMQP.Basic.ack(chan, rec_meta.delivery_tag)
    end

    AMQP.Connection.close conn
  end

  test "forwards messages to two other middlewares" do
    payload    = "Voodoo People"
    recipient1 = "1"
    recipient2 = "2"
    topic      = "90's"
    msg1 = %Message{payload: payload, recipient: recipient1, topic: topic}
    msg2 = %Message{payload: payload, recipient: recipient2, topic: topic}


    Forwarding.Proxy.RabbitSender.forward msg1
    Forwarding.Proxy.RabbitSender.forward msg2

    {:ok, conn} = AMQP.Connection.open(host: host(), port: port())
    {:ok, chan} = AMQP.Channel.open conn

    # test MW to MW communication
    queue_name = RoutingInfo.whoami() <> "to" <> "1"
    AMQP.Queue.declare(chan, queue_name, durable: true)

    AMQP.Basic.consume(chan, queue_name)

    receive do
      {:basic_deliver, _payload, rec_meta} ->
        assert _payload = payload
        AMQP.Basic.ack(chan, rec_meta.delivery_tag)
    end

    # test MW to MW communication
    queue_name = RoutingInfo.whoami() <> "to" <> "2"
    AMQP.Queue.declare(chan, queue_name, durable: true)

    AMQP.Basic.consume(chan, queue_name)

    receive do
      {:basic_deliver, _payload, rec_meta} ->
        assert _payload = payload
        AMQP.Basic.ack(chan, rec_meta.delivery_tag)
    end

    AMQP.Connection.close conn
  end

  defp host do
    Application.get_env(:forwarding, :send_host)
  end

  defp port do
    Application.get_env(:forwarding, :send_port)
  end

  defp mw_broker_xch do
    Application.get_env(:forwarding, :mw_broker_xch)
  end
end
