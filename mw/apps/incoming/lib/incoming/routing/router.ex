defmodule Incoming.Router do
  use GenStage

  alias Incoming.Utils.Acker, as: Acker
  alias Utils.Message.MessageCodec, as: MessageCodec
  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  def start_link(producers) do
    GenStage.start_link __MODULE__, producers, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(producers) do
    producer_names =
      for producer <- producers do
        ("RabbitReceiver" <> producer)
        |> String.to_atom
      end
    { :producer_consumer, :ok, subscribe_to: producer_names }
  end

  def handle_events(messages, _from, state) do
    local_messages =
      for {message, meta} <- messages do
        message
        |> MessageCodec.decode
        |> route_message(meta)
      end
      |> List.flatten
    { :noreply, local_messages, state }
  end

  defp route_message(message, meta) do
    if message.recipient == RoutingInfo.whoami() do
      { message, meta }
    else
      # ack message only if it is routed to something else
      Forwarding.Proxy.RabbitSender.forward message
      :ok = Acker.ack meta
      [ ]
    end
  end
end
