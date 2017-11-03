defmodule Coordination.Coordinator do
  use GenServer
  import Supervisor.Spec, warn: false

  alias Utils.Message,      as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  ########################
  ### KILLER
  ########################

  defmodule Killer do
    use Task, restart: :transient

    def start_link do
      lifetime = _minutes() * 1000 * 60
      Task.start_link __MODULE__, :live_until, [ lifetime ]
    end

    def live_until(lifetime) do
      :timer.sleep lifetime
      GenServer.call Coordination.Coordinator, :kill
      :ok
    end

    defp _minutes do
      Application.get_env :coordination, :lifetime
    end
  end

  ########################
  ### CLIENT API
  ########################

  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  def am_i_the_coordinator? do
    String.ends_with? RoutingInfo.whoami(), "_1"
  end

  def start do
    GenServer.call __MODULE__, :start
  end

  ########################
  ### SERVER CALLBACKS
  ########################

  def init(:ok) do
    { :ok, :stopped }
  end

  def handle_call(:start, _from, :stopped ) do

    if am_i_the_coordinator?() do

      payload   = "boot.req."  <> RoutingInfo.whoami()
      recipient = RoutingInfo.whoami()
      sender    = RoutingInfo.whoami()
      topic     = "boot"
      %Message{
        payload:   payload,
        recipient: recipient,
        sender:    sender,
        topic:     topic
      }
      |> Forwarding.handle_message

      child = [
         worker(Coordination.Coordinator.Killer, [ ]),
      ]
      Supervisor.start_link child, strategy: :one_for_one
    end

    {:reply, :ok, :started }
  end
  def handle_call(:start, _from, state ), do: {:reply, :ok, state }

  def handle_call(:kill, _from, :started ) do

    if am_i_the_coordinator?() do
      IO.inspect "Time to stop everything"

      payload   = "termination.req."  <> RoutingInfo.whoami()
      recipient = RoutingInfo.whoami()
      sender    = RoutingInfo.whoami()
      topic     = "termination"
      %Message{
        payload:   payload,
        recipient: recipient,
        sender:    sender,
        topic:     topic
      }
      |> Forwarding.handle_message

    end

    {:reply, :ok, :finished }
  end
  def handle_call(:kill, _from, state ), do: {:reply, :ok, state }
end
