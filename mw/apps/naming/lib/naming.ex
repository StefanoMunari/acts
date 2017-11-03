defmodule Naming do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # List all child processes to be supervised
    children = [
      # Starts a worker by calling: Naming.Worker.start_link(arg)
      worker(Naming.NameResolver, [])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Naming.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
