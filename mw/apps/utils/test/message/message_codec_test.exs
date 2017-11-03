defmodule MessageCodecTest do
  use ExUnit.Case
  doctest Utils.Message.MessageCodec

  alias Utils.Message, as: Message
  alias Utils.Message.MessageCodec, as: MessageCodec

  test "decode(encode(msg)) for mw keeps payload" do
    payload    = "Functional programming is fun"
    recipient = "Anakin"
    topic      = "kittens"
    msg = %Message{payload: payload, recipient: recipient, topic: topic}
    expected = %Message{payload: payload, recipient: recipient, topic: topic}
    actual = MessageCodec.decode(MessageCodec.encode(msg))
    assert actual == expected
  end

  test "decode(encode(msg)) for proxy keeps payload and topic" do
    payload    = "It's a trap"
    recipient = "Obi"
    topic      = "stormtroopers"
    msg = %Message{payload: payload, recipient: recipient, topic: topic}
    expected = %Message{payload: payload, recipient: recipient, topic: topic}
    actual = MessageCodec.decode(MessageCodec.encode(msg))
    assert actual == expected
  end

  test "encode(decode(msg)) for mw returns the given string" do
    payload    = "All that glitters is not gold."
    recipient = "Will"
    topic      = "tobeornottobe"
    msg = "{\"payload\":\"#{payload}\","
       <> "\"recipient\":[\"#{recipient}\"],"
       <> "\"topic\":\"#{topic}\"}"
    expected = msg
    actual = MessageCodec.encode(MessageCodec.decode(msg))
    assert actual == expected
  end

  test "encode(decode(msg)) for proxy returns the given string" do
    payload    = "Houston, we have a problem."
    recipient = "Apollo"
    topic      = "moon"
    msg = "{\"payload\":\"#{payload}\","
       <> "\"recipient\":[\"#{recipient}\"],"
       <> "\"topic\":\"#{topic}\"}"
    expected = msg
    actual = MessageCodec.encode(MessageCodec.decode(msg))
    assert actual == expected
  end
end
