defmodule Coordination do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
       worker(Coordination.Monitor, [ boot() ],
              [ id: String.to_atom( "Monitor_" <> boot() ) ]
             ),
       worker(Coordination.Monitor, [ termination() ],
              [id: String.to_atom( "Monitor_" <> termination() ) ]
             ),
       worker(Coordination.Coordinator, [ ])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Coordination.Supervisor]

    IO.inspect "The middleware is ready"

    Supervisor.start_link children, opts
  end

  def handle_message(message) do
    message.payload
    |> case do
       "start" ->
          Coordination.Coordinator.start
       _       ->
          :ok
    end
  end

  def termination do
    Application.get_env :coordination, :termination
  end

  def boot do
    Application.get_env :coordination, :boot
  end
end
