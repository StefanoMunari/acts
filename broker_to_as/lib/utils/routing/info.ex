defmodule BrokerToAs.Utils.Routing.Info do

  def whoami do
    city_node_id()
  end

  defp city_node_id do
    System.get_env "CITY_NODE_ID"
  end
end
