defmodule Coordination.Monitor do
  use GenServer
  import Supervisor.Spec, warn: false

  alias Utils.AppMessageOperations, as: AppMessageOperations
  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  ########################
  ### PERIODIC TASK
  ########################

  defmodule Checker do
    use Task, restart: :transient

    def start_link(service) do
      Task.start_link __MODULE__, :check, [:ongoing, service]
    end

    def check(:done, _) do
      :ok
    end
    def check(_, service) do
      status = Coordination.Monitor.is_done service
      if status == :done do
        check :done, status
      else
        status = Coordination.Monitor.check service
        :timer.sleep 15 * 1000
        check status, service
      end
    end
  end

  ########################
  ### CLIENT API
  ########################

  def start_link(service) do
    GenServer.start_link __MODULE__, service, name: monitor_name(service)
  end

  def is_done(service) do
    GenServer.call monitor_name(service), :is_done
  end

  def check(service) do
    GenServer.call monitor_name(service), :check
  end

  def request(service) do
    GenServer.call monitor_name(service), :request
  end

  def stop(service) do
    GenServer.call monitor_name(service), :stop
  end

  ########################
  ### SERVER CALLBACKS
  ########################

  def init(service) do
    { :ok, {:idle, service} }
  end

  def handle_call(:is_done, _from, { status, service } ) do
    { :reply, status, { status, service } }
  end

  def handle_call(:check, _from, {:ongoing = status, service} = state) do
    payload   = AppMessageOperations.create_app_message_as_payload service
    recipient = RoutingInfo.whoami()
    sender    = RoutingInfo.whoami()
    topic     = "interlayer"
    %Message{payload: payload,
             recipient: recipient,
             sender: sender,
             topic: topic}
    |> Forwarding.handle_message
    IO.inspect "Checking #{service}..."
    {:reply, status, state}
  end
  def handle_call(:check, _from, { status, _ } = state) do
    {:reply, status, state}
  end

  def handle_call(:request, _from, {:done, _} = state) do
    {:reply, :ok, state}
  end
  def handle_call(:request, _from_, {_, service} ) do
    child = [
      # Define workers and child supervisors to be supervised
       worker(Coordination.Monitor.Checker, [ service ]),
    ]
    IO.inspect "#{service} has been requested"
    Supervisor.start_link child, strategy: :one_for_one
    {:reply, :ok, {:ongoing, service} }
  end

  def handle_call(:stop, _from, { :ongoing, service } ) do
    if Coordination.Coordinator.am_i_the_coordinator?() do
      payload   = AppMessageOperations.create_app_message_as_payload service
      recipient = RoutingInfo.whoami()
      sender    = RoutingInfo.whoami()
      topic     = "control." <> service
      %Message{
        payload:   payload,
        recipient: recipient,
        sender:    sender,
        async:     true,
        topic:     topic
      }
      |> Forwarding.handle_message
    end

    {:reply, :ok, {:done, service}}
  end
  def handle_call(:stop, _from_, state) do
    {:reply, :ok, state}
  end

  defp monitor_name(service) do
    ("Monitor_" <> service) |> String.to_atom
  end
end
