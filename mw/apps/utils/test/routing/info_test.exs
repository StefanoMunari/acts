defmodule InfoTest do
  use ExUnit.Case
  doctest Utils.Routing.Info

  test "whoami returns whoami" do
    assert Utils.Routing.Info.whoami == whoami()
  end

  test "neighbors returns neighbors" do
    assert Utils.Routing.Info.neighbors == neighbors()
  end

  test "computes exactly own next hop" do
    assert Utils.Routing.Info.get_next(whoami()) == whoami()
  end

  test "computes exactly neighbors' next hop" do
    for neighbor <- neighbors() do
      assert Utils.Routing.Info.get_next(neighbor) == neighbor
    end
  end

  test "computes exactly next hop for non-directly reachable node" do
    assert Utils.Routing.Info.get_next("6") == "2"
  end

  defp whoami do
    Application.get_env(:utils, :whoami)
  end

  defp neighbors do
    Application.get_env(:utils, :neighbors)
  end
end
