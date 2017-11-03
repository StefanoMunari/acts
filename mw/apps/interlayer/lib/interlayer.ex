defmodule Interlayer do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      worker(Task, [Interlayer.AppMwInterface, :start, [] ]),
      worker(Interlayer.MwAppInterface, []),
      worker(Interlayer.Pending.Consumer, [])
    ] # order matters!

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Interlayer.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def handle_message(message) do
    Interlayer.MwAppInterface.sync_to_application message
  end
end
