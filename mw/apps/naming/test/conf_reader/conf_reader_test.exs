defmodule ConfReaderTest do
  use ExUnit.Case
  doctest Naming.ConfReader

  test "reads correctly a JSON config file for the Naming module" do
    test_config = Naming.ConfReader.read_config

    first_node =
      test_config
      |> Enum.at(0)

    second_node =
      test_config
      |> Enum.at(1)

    assert first_node["id"] != nil
    assert first_node["id"] == "6"
    assert second_node["id"] != nil
    assert second_node["id"] == "2"

    assert first_node["host"] != nil
    assert Enum.member?(first_node["host"], 17)
    assert second_node["host"] != nil
    assert Enum.member?(second_node["host"], 500)

    assert first_node["treadable"] != nil
    assert Enum.member?(first_node["treadable"], 42)
    assert second_node["treadable"] != nil
    assert Enum.member?(second_node["treadable"], 4709)
  end
end
