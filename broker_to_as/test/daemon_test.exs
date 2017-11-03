defmodule DaemonTest do
  use ExUnit.Case, async: false
  doctest BrokerToAs.Daemon

  import Mock

  test "passes message to dispatcher" do
    pid = self()
    with_mock BrokerToAs.Collector,
              [
                handle_events:
                  fn(messages, _from, state) ->
                    for message <- messages do
                      send pid, {:msg, message}
                    end
                    {:noreply, [], state}
                  end,
                terminate: fn(_, _) -> nil end
              ]
    do
      message = "We will be victorious"

      {:ok, conn} = AMQP.Connection.open(host: host(), port: port())
      {:ok, chan} = AMQP.Channel.open conn

      AMQP.Exchange.declare(chan, mw_broker_xch(), :fanout)

      {:ok, %{queue: broker_queue}} =
        AMQP.Queue.declare(chan, "", exclusive: true)
      AMQP.Queue.bind(chan, broker_queue, mw_broker_xch())

      AMQP.Basic.publish(chan, mw_broker_xch(), broker_queue, message)

      receive do
        {:msg, _actual_msg} ->
          expected_msg = message
          assert _actual_msg = expected_msg
      end

      AMQP.Connection.close conn
    end
  end

  defp host do
    Application.get_env(:broker_to_as, :listen_host)
  end

  defp port do
    Application.get_env(:broker_to_as, :listen_port)
  end

  defp mw_broker_xch do
    Application.get_env(:broker_to_as, :mw_broker_xch)
  end
end
