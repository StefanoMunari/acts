defmodule Persistence do
  use Application

  alias Utils.Routing.Info, as: RoutingInfo

  @moduledoc """
  Documentation for Persistence (if any).
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: Persistence.Worker.start_link(arg)
      worker(Persistence.PendingRequestDb, []),
      worker(Persistence.MwAppMessageDb, []),
      worker(Persistence.ForwardMessageDb, [])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Persistence.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def host do
    node_id =
      RoutingInfo.whoami()
      |> String.split("_")
      |> Enum.at(1)
    host_name = Application.get_env :persistence, :host
    host_name <> node_id
  end

  def port do
    Application.get_env :persistence, :port
  end

end
