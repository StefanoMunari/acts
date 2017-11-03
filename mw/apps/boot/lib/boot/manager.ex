defmodule Boot.Manager do
  use GenServer

  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  def node_ready do
    GenServer.call __MODULE__, :ready
  end

  def boot_request(node_id) do
    GenServer.call __MODULE__, {:boot_request, node_id}
  end

  def boot_reply(node_id) do
    GenServer.call __MODULE__, {:boot_reply, node_id}
  end

  # CAUTION: Resets Boot.Manager state!!!!
  def reset do
    GenServer.call __MODULE__, :reset
  end

  ### SERVER CALLBACKS
  def init(:ok) do
    { :ok, %Boot.State{ pending: RoutingInfo.neighbors() } }
  end

  def handle_call(:ready, _from, state) do
    Coordination.Monitor.stop "boot"
    cond do
      # if I'm the only node in the system
      state.phase != :booted and RoutingInfo.neighbors() === [] ->
        become_booted state
      # All markers have been received for initialization
      state.phase != :booted and state.pending === [] ->
        become_booted state
      true ->
        { :reply, :ok, state }
    end
  end

  def handle_call({:boot_request, node_id}, _from, state) do
    cond do
      :boot in state.markers ->
        # Send ack immediately
        payload   = "boot.done.#{RoutingInfo.whoami()}"
        recipient = node_id
        send_message payload, recipient
        { :reply, :ok, state }
      true ->
        # There is no boot marker:
        # Propagate marker and remember boot marker reception
        payload   = "boot.req.#{RoutingInfo.whoami()}"
        recipients = RoutingInfo.neighbors()
        send_messages payload, recipients
        { :reply, :ok, %{ state | markers: [:boot | state.markers] } }
    end
  end

  def handle_call({:boot_reply, node_id}, _from, state) do
    new_pending = List.delete state.pending, node_id
    cond do
      # ignore messages when not in booting state
      state.phase != :idle ->
        {:reply, :ok, state}
      # node is ready to boot now!
      state.phase == :idle and new_pending === [] ->
        become_booting state
      # node is booting but there is someone left
      true ->
        { :reply, :ok, %{ state | pending: new_pending } }
    end
  end

  def handle_call(:reset, _from, _) do
    { :reply, :ok, %Boot.State{ pending: RoutingInfo.neighbors() } }
  end

  defp become_booting(state) do
    Coordination.Monitor.request "boot"
    { :reply, :ok, %{ state | phase: :booting, pending: [] } }
  end

  defp become_booted(state) do
    payload = "boot.done.#{RoutingInfo.whoami()}"
    recipients = RoutingInfo.neighbors()
    send_messages payload, recipients
    { :reply, :ok, %{ state | phase: :booted, pending: [] } }
  end

  defp send_messages(payload, recipients) do
    for recipient <- recipients do
      send_message payload, recipient
    end
  end

  defp send_message(payload, recipient) do
    topic = "boot"
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
