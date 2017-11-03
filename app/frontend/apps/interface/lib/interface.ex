defmodule Interface do
  use Application

  alias Interface.OutputStream.CityManager, as: CityManager

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children =
    [
      # Start the endpoint when the application starts
      supervisor(Interface.Endpoint, [ ]),
      worker(Interface.InputStream, [ broker() ]),
      worker(Interface.InputStream.Formatter, [ ]),
      worker(Interface.InputStream.InternalDispatcher, [ ]),
      worker(Interface.InputStream.Broadcaster, [ ]),
    ]
    ++
      for city <- Domain.cities() do
        worker(
          CityManager,
          [ city, broker() ],
          [ id: CityManager.manager_name city ]
        )
      end

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [ strategy: :one_for_one, name: Interface.Supervisor ]
    Supervisor.start_link children, opts
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Interface.Endpoint.config_change changed, removed
    :ok
  end

  defp broker do
    Application.get_env :interface, :broker
  end
end
