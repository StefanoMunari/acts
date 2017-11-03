defmodule BrokerToAs.AsExecutor do
  use GenStage

  alias BrokerToAs.Utils.Message, as: Message

  ## EXTERNAL APIS

  def start_link(chan) do
    GenStage.start_link __MODULE__, chan, name: __MODULE__
  end

  ## SERVER CALLBACKS

  def init(chan) do
    { :consumer, chan, subscribe_to: [ BrokerToAs.AsListener ] }
  end

  def handle_events(messages, _from, chan) do
    # for each message
    for { str, _ } <- messages do
      [ "stream", district_full_id ] = String.split str, "."
      [ city_id, _ ] = String.split district_full_id, "_"
      district_id = city_id <> "_" <> "1"
      msg = %Message{
        payload:   "start",
        recipient: district_id,
        sender:    district_id,
        topic:     "coordination"
      }
      AMQP.Queue.declare chan,
                         district_id <> "to" <> district_id,
                         durable: true
      encoded_msg = BrokerToAs.Utils.MessageCodec.encode msg
      queue_name  = district_id <> "to" <> district_id
      AMQP.Basic.publish chan, "", queue_name, encoded_msg, persistent: true
    end
    { :noreply, [], chan }
  end
end
