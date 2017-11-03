defmodule BrokerToAs do
  use Application
  @moduledoc """
  Documentation for BrokerToAs.
  """


  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # BE -> AS
    { :ok, rabbit_conn }       =
      AMQP.Connection.open host: sender_host(), port: sender_port()
    { :ok, channel }           =
      AMQP.Channel.open rabbit_conn
    { :ok, connection }        =
      Wabbit.Connection.start_link host: receiver_host(), port: receiver_port()
    # AS -> BE
    { :ok, as_in_connection }  =
      Wabbit.Connection.start_link host: sender_host(), port: sender_port()
    { :ok, as_out_conn }       =
      AMQP.Connection.open host: receiver_host(), port: receiver_port()
    { :ok, as_out_chan }       =
      AMQP.Channel.open as_out_conn

    # List all child processes to be supervised
    children = [
      # BE -> AS
      worker(BrokerToAs.Daemon, [ connection ]),
      worker(BrokerToAs.BackendControlListener, [ connection ]),
      worker(BrokerToAs.ContentEnricher, [ ]),
      worker(BrokerToAs.Forwarder, [ channel ]),
      # AS -> BE
      worker(BrokerToAs.AsListener, [ as_in_connection ]),
      worker(BrokerToAs.AsExecutor, [ as_out_chan ]),
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BrokerToAs.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp receiver_host do
    Application.get_env :broker_to_as, :receiver_host
  end
  defp receiver_port do
    Application.get_env :broker_to_as, :receiver_port
  end
  defp sender_host do
    Application.get_env :broker_to_as, :sender_host
  end
  defp sender_port do
    Application.get_env :broker_to_as, :sender_port
  end
end
