defmodule BrokerToAs.AsListener do
  use Wabbit.GenStage

  alias BrokerToAs.Utils.Routing.Info, as: RoutingInfo

  def start_link(connection) do
    Wabbit.GenStage.start_link __MODULE__, connection, :ok, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    { :producer, :ok }
  end

  def handle_channel_opened(channel, state) do
    Wabbit.Exchange.declare channel, broker_as_xch(), :topic
    { :ok, %{ queue: queue } } = Wabbit.Queue.declare channel,
                                                      "",
                                                      exclusive: true
    Wabbit.Queue.bind channel,
                      queue,
                      broker_as_xch(),
                      routing_key: "fe.command." <> RoutingInfo.whoami()

    # Set consume queue and options
    { :ok, queue, state }
  end

  defp broker_as_xch do
    Application.get_env :broker_to_as, :broker_as_xch
  end
end
