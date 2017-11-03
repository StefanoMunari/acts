defmodule Utils.Routing.Info do

  def whoami do
    if Mix.env === :test do
      Application.get_env :utils, :whoami
    else
      node_id = city_node_id()
      Mix.Tasks.RoutingTables.get_routing_table_for(node_id)
      |> (fn routing_table -> routing_table.id end).()
    end
  end

  def neighbors do
    if Mix.env === :test do
      Application.get_env :utils, :neighbors
    else
      node_id = city_node_id()
      Mix.Tasks.RoutingTables.get_routing_table_for(node_id)
      |> (fn routing_table -> routing_table.neighbors end).()
      |> Enum.map(&(String.trim(&1, "\"")))
    end
  end

  def get_next(dst) do
    routing_table()[dst]
  end

  defp routing_table do
    if Mix.env === :test do
      Application.get_env :utils, :routing_table
    else
      node_id = city_node_id()
      Mix.Tasks.RoutingTables.get_routing_table_for(node_id)
      |> (fn routing_table -> routing_table.table end).()
    end
  end

  defp city_node_id do
    group_id() <> "_" <> own_id()
  end

  def group_id do
    System.get_env("CITY_NODE_ID")
  end
  def own_id do
    System.get_env("CITY_DISTRICT_ID")
  end
end
