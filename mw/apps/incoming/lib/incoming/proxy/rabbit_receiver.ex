defmodule Incoming.Proxy.RabbitReceiver do
  use Wabbit.GenStage

  alias Utils.Routing.Info, as: RoutingInfo

  def start_link(conn, neighbor) do
    name =
      ("RabbitReceiver" <> neighbor)
      |> String.to_atom
    Wabbit.GenStage.start_link __MODULE__, conn, neighbor, name: name
  end

  def init(neighbor) do
    # `neighbor is the state
    {:producer, neighbor}
  end

  def handle_channel_opened(chan, neighbor) do
    # Declare exchange, queue, bindings, etc...
    queue_name = neighbor <> "to" <> RoutingInfo.whoami()
    {:ok, %{queue: queue}} =
      Wabbit.Queue.declare(chan, queue_name, durable: true)
    # Set consume queue and options
    {:ok, queue, neighbor, prefetch_count: 1}
  end
end
