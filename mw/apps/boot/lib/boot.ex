defmodule Boot do
  use Application

  @moduledoc """
  Documentation for Boot.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: start_link
      worker(Boot.Manager, [])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Boot.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_message(message) do
    payload = message.payload
    IO.inspect message
    case payload do
       "ready"                 ->
          Boot.Manager.node_ready
       "boot.req."  <> node_id ->
          Boot.Manager.boot_request node_id
       "boot.done." <> node_id ->
          Boot.Manager.boot_reply node_id
       _                        ->
          :ok
    end
  end
end
