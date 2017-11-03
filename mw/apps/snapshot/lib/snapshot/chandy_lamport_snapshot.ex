defmodule Snapshot.ChandyLamportSnapshot do
  use GenServer

  ### CLIENT API

  def start_link(snapshot_name, pending) do
    GenServer.start_link __MODULE__, pending, name: snapshot_name
  end

  ### SERVER CALLBACKS
  def init(pending) do
    {:ok, pending}
  end

  def handle_call({:ends_with, node_id}, _from, [last])
  when last === node_id do
    {
      :reply,
      true,
      [ ]
    }
  end

  def handle_call({:ends_with, node_id}, _from, pending) do
    new_pending =
      case Enum.member? pending, node_id do
         true  -> List.delete pending, node_id
         false -> pending
      end
    {:reply, false, new_pending}
  end
end
