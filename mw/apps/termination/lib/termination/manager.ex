defmodule Termination.Manager do
  use GenServer

  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  def node_stopped do
    GenServer.call __MODULE__, :stopped
  end

  def termination_request(node_id) do
    GenServer.call __MODULE__, {:termination_request, node_id}
  end

  def termination_reply(node_id) do
    GenServer.call __MODULE__, {:termination_reply, node_id}
  end

  # CAUTION: Resets Termination.Manager state!!!!
  def reset do
    GenServer.call __MODULE__, :reset
  end

  ### SERVER CALLBACKS
  def init(:ok) do
    {:ok, %Termination.State{pending: RoutingInfo.neighbors()}}
  end

  def handle_call(:stopped, _from, state) do
    Coordination.Monitor.stop "shutdown"
    cond do
      # if I'm the only node in the system
      state.phase != :terminated and RoutingInfo.neighbors() === [] ->
        become_terminated state
      # All markers have been received for stop
      state.phase != :terminated and state.pending === [] ->
        become_terminated state
      true ->
        {:reply, :ok, state}
    end
  end

  def handle_call({:termination_request, node_id}, _from, state) do
    cond do
      :termination in state.markers ->
        # Send ack immediately
        payload   = "termination.reply.#{RoutingInfo.whoami()}"
        recipient = node_id
        send_message payload, recipient
        {:reply, :ok, state}
      true ->
        # There is no termination marker:
        # Propagate marker and remember termination marker reception
        payload   = "termination.req.#{RoutingInfo.whoami()}"
        recipients = RoutingInfo.neighbors()
        send_messages payload, recipients
        {:reply, :ok, %{state | markers: [:termination | state.markers]}}
    end
  end

  def handle_call({:termination_reply, node_id}, _from, state) do
    new_pending = List.delete state.pending, node_id
    IO.inspect "New pending: #{inspect new_pending}"
    cond do
      # ignore messages when not in running state
      state.phase != :running ->
        {:reply, :ok, state}
      # node is terminating and all neighbors have been terminated:
      state.phase == :running and new_pending === [] ->
        become_terminating state
      # node is terminating but there is someone left
      true ->
        {:reply, :ok, %{state | pending: new_pending}}
    end
  end

  def handle_call(:reset, _from, _) do
    {:reply, :ok, %Termination.State{pending: RoutingInfo.neighbors()}}
  end

  defp become_terminating(state) do
    IO.inspect "BECOMING TERMINATING..."
    Coordination.Monitor.request "shutdown"
    {
      :reply,
      :ok,
      %{state | phase: :terminating, pending: []}
    }
  end

  defp become_terminated(state) do
    payload = "termination.reply.#{RoutingInfo.whoami()}"
    recipients = RoutingInfo.neighbors()
    send_messages payload, recipients
    {:reply, :ok, %{state | phase: :terminated, pending: []}}
  end

  defp send_messages(payload, recipients) do
    for recipient <- recipients do
      send_message payload, recipient
    end
  end

  defp send_message(payload, recipient) do
    topic = "termination"
    fmtd_recipient = String.trim recipient, "\""
    %Message{
      payload:   payload,
      recipient: fmtd_recipient,
      sender:    RoutingInfo.whoami(),
      topic:     topic
    }
    |> Forwarding.handle_message
  end
end
