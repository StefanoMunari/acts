defmodule Incoming do
  use Application

  alias Utils.Routing.Info, as: RoutingInfo

  @moduledoc """
  Documentation for Incoming.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    {:ok, wabbitConn} = Wabbit.Connection.start_link host: host(), port: port()

    incoming_queues = [ RoutingInfo.whoami() | RoutingInfo.neighbors() ]
    # List all child processes to be supervised
    children =
      for node_id <- incoming_queues do
        worker(Incoming.Proxy.RabbitReceiver, [wabbitConn, node_id],
               [id: String.to_atom("RabbitReceiver" <> node_id)])
      end
        ++
        [
          worker(Incoming.Router, [incoming_queues] ),
          worker(Incoming.MessageHolder, [ ] ),
          worker(Incoming.Dispatcher, [ ] )
        ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Incoming.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp host do
    Application.get_env :incoming, :rec_host
  end

  defp port do
    Application.get_env :incoming, :rec_port
  end
end
