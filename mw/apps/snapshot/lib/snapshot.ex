defmodule Snapshot do
  use Application
  @moduledoc """
  Documentation for Snapshot.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      worker(algorithm(), []),
    ] # order matters!

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Snapshot.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_message(message) do
    IO.inspect message
  end

  def algorithm do
    Application.get_env :snapshot, :algorithm
  end
end
