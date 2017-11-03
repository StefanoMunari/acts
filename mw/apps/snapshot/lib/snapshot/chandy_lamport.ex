defmodule Snapshot.ChandyLamport do
  @behaviour Snapshot.SnapshotAlgorithm
  use Supervisor

  alias Utils.Routing.Info, as: RoutingInfo

  ### CLIENT API

  def start_link do
    Supervisor.start_link __MODULE__, :ok, name: __MODULE__
  end

  def take(snapshot_message) do
    Snapshot.ChandyLamportExecutor.take snapshot_message
  end

  def submit(snapshot_message) do
    Snapshot.ChandyLamportExecutor.submit snapshot_message
  end

  def spawn_child(child_name, node_id) do
    pending =
      [RoutingInfo.whoami() | RoutingInfo.neighbors()]
      |> List.delete(node_id)
    child =
    [
      worker(Snapshot.ChandyLamportSnapshot, [child_name, pending],
             restart: :transient, id: child_name)
    ]
    Supervisor.start_link child, strategy: :one_for_one
  end

  def stop(child_name) do
    Supervisor.terminate_child __MODULE__, child_name
  end

  ### SERVER CALLBACKS
  def init(:ok) do
    children = [
      worker(Snapshot.ChandyLamportExecutor, []),
    ]

    Supervisor.init children, strategy: :one_for_one
  end
end
