defmodule Forwarding do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    { :ok, connection } = AMQP.Connection.open host: host(), port: port()

    # Define workers and child supervisors to be supervised
    children = [
      # Starts a worker by calling: Forwarding.Worker.start_link(arg1, arg2, arg3)
      # worker(Forwarding.Worker, [arg1, arg2, arg3]),
      #worker(Forwarding.MessageQueue, [])
      worker(sender(), [ connection ])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [ strategy: :one_for_one, name: Forwarding.Supervisor ]
    Supervisor.start_link children, opts
  end

  def handle_message(nil) do
    :ok
  end
  def handle_message(message) do
    IO.inspect "The topic is: #{message.topic}, with recipient #{message.recipient}"
    sender().forward message
  end

  defp host do
    Application.get_env :forwarding, :send_host
  end

  defp port do
    Application.get_env :forwarding, :send_port
  end

  defp sender do
    Application.get_env :forwarding, :sender
  end
end
