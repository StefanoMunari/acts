defmodule Utils.Message.MessageCodec do

  alias Utils.Message, as: Message

  # in the followings, any stands for JSON.t

  @spec decode(any) :: struct
  def decode(message) do
    {:ok, message_map} = JSX.decode message
    %Message{payload:   message_map["payload"],
             recipient: message_map["recipient"],
             sender:    message_map["sender"],
             topic:     message_map["topic"]}
  end

  @spec encode(struct) :: any
  def encode(message) do
    {:ok, json} = JSX.encode message
    json
  end
end
