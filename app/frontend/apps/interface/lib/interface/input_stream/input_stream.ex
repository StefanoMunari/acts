defmodule Interface.InputStream do
  use Wabbit.GenStage

  def start_link(hostname) do
    { :ok, connection } =
      Wabbit.Connection.start_link host: hostname, port: port()
    Wabbit.GenStage.start_link __MODULE__, connection, :ok, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    { :producer, :ok }
  end

  def handle_channel_opened(channel, state) do
    Wabbit.Exchange.declare channel, input_exchange(), :topic
    { :ok, %{queue: queue} } =
      Wabbit.Queue.declare channel, "", exclusive: true
    Wabbit.Queue.bind channel,
                      queue,
                      input_exchange(),
                      routing_key: "#"

    # Set consume queue and options
    { :ok, queue, state }
  end

  defp input_exchange do
    "broker_to_as"
  end

  defp port do
    Application.get_env :interface, :broker_port
  end
end
