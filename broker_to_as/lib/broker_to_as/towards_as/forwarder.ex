defmodule BrokerToAs.Forwarder do
  use GenStage

  alias BrokerToAs.Utils.MessageCodec, as: MessageCodec

  ## EXTERNAL APIS

  def start_link(chan) do
    GenStage.start_link __MODULE__, chan, name: __MODULE__
  end

  ## SERVER CALLBACKS

  def init(chan) do
    AMQP.Exchange.declare chan, broker_as_xch(), :topic
    {
      :consumer,
      chan,
      subscribe_to: [
        BrokerToAs.ContentEnricher
      ]
    }
  end

  def handle_events(messages, _from, chan) do
    # for each message
    for { json_message, _ } <- messages do
      message = MessageCodec.decode json_message
      IO.inspect message
      send_to_as json_message, message.topic, chan
    end
    { :noreply, [], chan }
  end

  defp send_to_as(msg, topic, chan) do
    AMQP.Basic.publish chan, broker_as_xch(), topic, msg
  end

  defp broker_as_xch do
    Application.get_env :broker_to_as, :broker_as_xch
  end
end
