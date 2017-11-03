defmodule Snapshot.ChandyLamportExecutor do
  use GenServer

  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  def take(snapshot_message) do
    GenServer.call __MODULE__, {:take, snapshot_message}
  end

  def submit(snapshot_message) do
    GenServer.call __MODULE__, {:submit, snapshot_message}
  end

  ### SERVER CALLBACKS
  def init(:ok) do
    {:ok, []}
  end

  def handle_call({:take, snapshot_message}, _from, snapshots) do
    {node_id, version, req} = snapshot_message
    requested_snapshot =
      "snap_id#{node_id}_v#{version}"
      |> String.to_atom
    cond do
      requested_snapshot not in snapshots ->
        # Start new ChandyLamportSnapshot
        Snapshot.ChandyLamport.spawn_child requested_snapshot, node_id
        take_own_snapshot snapshot_message
        payload = snapshot_payload snapshot_message, "take"
        send_control_messages payload, RoutingInfo.neighbors()
        { :reply, :ok, [ requested_snapshot | snapshots ] }
      requested_snapshot in snapshots ->
      # If node_and_version is already in snapshots, send resume back
        payload = "resume.#{node_id}.#{version}.#{RoutingInfo.whoami()}"
        send_single_control_message payload, req
        { :reply, :ok, snapshots }
    end
  end

  def handle_call({:submit, _}, _from, []) do
    {:reply, :ok, []}
  end

  def handle_call({:submit, snapshot_message}, _from, snapshots) do
    {node_id, version, submitter} = snapshot_message
    snapshot_instance =
      "snap_id#{node_id}_v#{version}"
      |> String.to_atom
    cond do
      snapshot_instance not in snapshots ->
        {:reply, snapshots, snapshots}
      GenServer.call(snapshot_instance, {:ends_with, submitter}) ->
        remaining_snapshots = List.delete snapshots, snapshot_instance
        payload = snapshot_payload snapshot_message, "resume"
        recipients = [RoutingInfo.whoami() | RoutingInfo.neighbors()]
        send_control_messages payload, recipients
        Snapshot.ChandyLamport.stop snapshot_instance
        {:reply, remaining_snapshots, remaining_snapshots}
      true ->
        {:reply, snapshots, snapshots}
    end
  end

  defp snapshot_payload(snapshot_message, action) do
    {node_id, version, _} = snapshot_message
    "#{action}.#{node_id}.#{version}.#{RoutingInfo.whoami()}"
  end

  defp take_own_snapshot(snapshot_message) do
    {node_id, version, _} = snapshot_message
    topic = "application"
    payload = "SNAPSHOT.#{node_id}.#{version}"
    %Message{payload: payload,
             recipient: RoutingInfo.whoami(),
             topic: topic}
    |> Forwarding.handle_message
  end

  defp send_control_messages(payload, recipients) do
    topic = "snapshot"
    for neighbor <- recipients do
      recipient = neighbor
      %Message{payload: payload,
               recipient: recipient,
               topic: topic}
    end
    |> Enum.map(&(Forwarding.handle_message &1))
  end

  defp send_single_control_message(payload, recipient) do
    topic = "snapshot"
    %Message{payload: payload,
             recipient: recipient,
             topic: topic}
    |> Forwarding.handle_message
  end
end
