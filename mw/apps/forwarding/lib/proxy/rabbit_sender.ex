defmodule Forwarding.Proxy.RabbitSender do
  use GenServer
  @behaviour Forwarding.Proxy.MQProxy

  alias Utils.Message.MessageCodec, as: MessageCodec
  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  @spec start_link(any) :: any
  def start_link(conn) do
    GenServer.start_link __MODULE__, conn, name: __MODULE__
  end

  @spec forward(struct) :: any
  def forward(message) do
    GenServer.call __MODULE__, {:send, message}
  end

  ### SERVER CALLBACKS
  def init(connection) do
    {:ok, channel} = AMQP.Channel.open connection
    for neighbor <- [ RoutingInfo.whoami() | RoutingInfo.neighbors() ] do
      AMQP.Queue.declare channel,
                         RoutingInfo.whoami() <> "to" <> neighbor,
                         durable: true
    end
    AMQP.Exchange.declare channel, mw_broker_xch(), :topic
    { :ok, { connection, channel } }
  end

  def handle_call({:send, message}, _from, {connection, channel}) do
    encoded_msg = MessageCodec.encode message

    # Send message to other MW
    if message.async
    do
      # Send message to AS broker
      AMQP.Basic.publish channel, mw_broker_xch(), message.topic, encoded_msg
    else
      # Send message to `message.recipient`'s Application layer
      routed_to   = RoutingInfo.get_next message.recipient
      queue_name  = RoutingInfo.whoami() <> "to" <> routed_to
      AMQP.Basic.publish channel, "", queue_name, encoded_msg, persistent: true
    end

    {:reply, :ok, {connection, channel}}
  end

  def terminate(_reason, {connection, _}) do
    AMQP.Connection.close connection
  end

  # MW to Broker exchange
  defp mw_broker_xch do
    Application.get_env :forwarding, :mw_broker_xch
  end
end
