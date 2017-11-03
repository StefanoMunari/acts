defmodule BrokerToAs.ContentEnricher do
  use GenStage

  alias BrokerToAs.Utils.MessageCodec, as: MessageCodec

  ## EXTERNAL APIS

  def start_link do
    GenStage.start_link __MODULE__, :ok, name: __MODULE__
  end

  ## SERVER CALLBACKS

  def init(:ok) do
    {
      :producer_consumer,
      :ok,
      subscribe_to:
        [
          BrokerToAs.Daemon,
          BrokerToAs.BackendControlListener
        ]
    }
  end

  def handle_events(messages, _from, state) do
    for { json_message, meta } <- messages do
      json_message
      |> MessageCodec.decode
      |> BrokerToAs.Utils.DeepInspection.inspect
      |> Enum.map(fn enriched -> { enriched, meta } end)
    end
    |> List.flatten
    |> Enum.map(&(encode_msg &1))
    |> (fn msgs -> { :noreply, msgs, state } end).()
  end

  def encode_msg( { message, meta } ) do
    message
    |> MessageCodec.encode
    |> (fn encoded -> { encoded, meta } end).()
  end
end
