defmodule BrokerToAs.BackendControlListener do
  use Wabbit.GenStage

  def start_link(connection) do
    Wabbit.GenStage.start_link __MODULE__, connection, :ok, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    { :producer, :ok }
  end

  def handle_channel_opened(channel, state) do
    Wabbit.Exchange.declare channel, mw_broker_xch(), :topic
    { :ok, %{ queue: queue } } = Wabbit.Queue.declare channel,
                                                      "",
                                                      exclusive: true
    Wabbit.Queue.bind channel,
                      queue,
                      mw_broker_xch(),
                      routing_key: "control.#"

    # Set consume queue and options
    { :ok, queue, state }
  end

  defp mw_broker_xch do
    Application.get_env :broker_to_as, :mw_broker_xch
  end
end
