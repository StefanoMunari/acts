defmodule Termination do
  use Application

  @moduledoc """
  Documentation for Termination.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: start_link
      worker(Termination.Manager, [])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Termination.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_message(message) do
    payload = message.payload
    case payload do
       "stopped" <> _                  ->
          Termination.Manager.node_stopped
       "termination.req." <> node_id   ->
          Termination.Manager.termination_request node_id
       "termination.reply." <> node_id ->
          Termination.Manager.termination_reply node_id
       _                               ->
          :ok
    end
    IO.inspect message
  end
end
