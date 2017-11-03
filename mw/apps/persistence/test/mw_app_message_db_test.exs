defmodule MwAppMessageDbTest do
  use ExUnit.Case
  doctest Persistence.MwAppMessageDb

  alias Utils.Message, as: Message
  alias Utils.Message.MessageCodec, as: MessageCodec

  test "can store a message with empty set" do
    {:ok, conn} = Redix.start_link "redis://#{host()}:#{port()}"
    Redix.command conn, ["DEL", set_name()]

    payload   = "Kalimera. [Greek granny]"
    recipient = "Minos"
    topic     = "Caldera"
    message   = %Message{payload: payload, recipient: recipient, topic: topic}
    encoded   = MessageCodec.encode message

    :ok = Persistence.MwAppMessageDb.store_msg message

    {:ok, greetings} = Redix.command conn, ["LRANGE", set_name(), 0, -1]
    assert greetings == [ encoded ]

    # clean
    Redix.command conn, ["DEL", set_name()]
  end

  test "can store two messages with empty set" do
    {:ok, conn} = Redix.start_link "redis://#{host()}:#{port()}"
    Redix.command conn, ["DEL", set_name()]

    payload1   = "Buenos dias! [Maurito]"
    recipient1 = "Maxi"
    topic1     = "Futebal"
    message1   =
      %Message{payload: payload1, recipient: recipient1, topic: topic1}
    encoded1   = MessageCodec.encode message1

    payload2   = "Gruetzi [Roger]"
    recipient2 = "CreditSuisse"
    topic2     = "timeliness"
    message2   =
      %Message{payload: payload2, recipient: recipient2, topic: topic2}
    encoded2   = MessageCodec.encode message2

    :ok = Persistence.MwAppMessageDb.store_msg message1
    :ok = Persistence.MwAppMessageDb.store_msg message2

    {:ok, greetings} = Redix.command conn, ["LRANGE", set_name(), 0, -1]
    assert MapSet.new(greetings) == MapSet.new([encoded1, encoded2])

    # clean
    Redix.command conn, ["DEL", set_name()]
  end

  test "can get a message from the db" do
    {:ok, conn} = Redix.start_link "redis://#{host()}:#{port()}"
    Redix.command conn, ["DEL", set_name()]

    payload   = "Sdravo... [Vladimir]"
    recipient = "Donnie"
    topic     = "cold"
    message   = %Message{payload: payload, recipient: recipient, topic: topic}
    encoded   = MessageCodec.encode message

    Redix.command conn, ["RPUSH", set_name(), encoded]

    greetings = Persistence.MwAppMessageDb.get_all_messages

    assert MapSet.new(greetings) == MapSet.new( [ message ] )

    # clean
    Redix.command conn, ["DEL", set_name()]
  end

  test "can get three messages from the db" do
    {:ok, conn} = Redix.start_link "redis://#{host()}:#{port()}"
    Redix.command conn, ["DEL", set_name()]

    payload1   = "Ciao! [Enrico]"
    recipient1 = "Valentina"
    topic1     = "mooseca"
    message1   =
      %Message{payload: payload1, recipient: recipient1, topic: topic1}
    encoded1   = MessageCodec.encode message1

    payload2   = "Hola [Iker]"
    recipient2 = "Sergio"
    topic2     = "defense"
    message2   =
      %Message{payload: payload2, recipient: recipient2, topic: topic2}
    encoded2   = MessageCodec.encode message2

    payload3   = "Bonjour"
    recipient3 = "Madame"
    topic3     = "chicken"
    message3   =
      %Message{payload: payload3, recipient: recipient3, topic: topic3}
    encoded3   = MessageCodec.encode message3

    Redix.command conn, ["RPUSH", set_name(), encoded1]
    Redix.command conn, ["RPUSH", set_name(), encoded2]
    Redix.command conn, ["RPUSH", set_name(), encoded3]

    greetings = Persistence.MwAppMessageDb.get_all_messages

    assert MapSet.new(greetings) == MapSet.new([message1, message2, message3])

    # clean
    Redix.command conn, ["DEL", set_name()]
  end

  ### CONFIG STUFF

  defp host do
    Application.get_env :persistence, :host
  end

  defp port do
    Application.get_env :persistence, :port
  end

  defp set_name do
    Application.get_env(:persistence, :mw_app)
  end
end
