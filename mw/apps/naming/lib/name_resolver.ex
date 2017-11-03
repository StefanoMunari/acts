defmodule Naming.NameResolver do
  use GenServer

  alias Utils.Routing.Info, as: RoutingInfo

  @moduledoc """
  This module finds the name of a node, given an identification code for one of
  the entities inside it.
  """

  def start_link do
    GenServer.start_link __MODULE__, nil, name: __MODULE__
  end

  # EXTERNAL APIS
  @doc """
  Solves a pair {id, type} relative to an entity, yielding the identifier of
  the node in which the entity is placed.
  """
  def solve(nil, _)
  do
    {:not_found, nil}
  end
  def solve(_, nil)
  do
    {:not_found, nil}
  end
  def solve(entity_id, entity_type)
    when is_binary entity_id
  do
    GenServer.call __MODULE__, {:solve, entity_id, entity_type}
  end
  def solve(entity_id, entity_type)
  do
    solve (Kernel.inspect entity_id), entity_type
  end

  # SERVER CALLBACKS

  def handle_call({:solve, entity_id, entity_type}, _, _) do
    entity_type_atom =
      entity_type
      |> String.downcase
    answer =
      Naming.ConfReader.read_config
      |> _find_entity_in_list(entity_id, entity_type_atom)
    {:reply, answer, nil}
  end

  defp _find_entity_in_list([], _, _) do
    {:not_found, nil}
  end

  defp _find_entity_in_list([first_node | list], entity_id, entity_type) do
    entities_list =
      (first_node[entity_type] || [])
      |> Enum.map(&(Kernel.inspect &1))
    node_id =
      first_node["id"]
      |> (fn id -> (RoutingInfo.group_id() <> "_" <> id) end).()
    if Enum.member?(entities_list, entity_id) do
      {:ok, node_id}
    else
      _find_entity_in_list list, entity_id, entity_type
    end
  end
end
