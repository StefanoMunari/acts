defmodule NameResolverTest do
  use ExUnit.Case
  doctest Naming.NameResolver

  test "can solve an existing entity with correct HOST type" do
    {:ok, node_id} = Naming.NameResolver.solve 1, "HOST"
    assert node_id == "6"
  end

  test "can solve an existing entity with correct TREADABLE type" do
    {:ok, node_id} = Naming.NameResolver.solve 24, "TREADABLE"
    assert node_id == "2"
  end

  test "cannot solve an unexisting HOST entity" do
    {:not_found, _} = Naming.NameResolver.solve 666, "HOST"
  end

  test "cannot solve an unexisting TREADABLE entity" do
    {:not_found, _} = Naming.NameResolver.solve 666, "TREADABLE"
  end

  test "cannot solve an existing HOST id given TREADABLE type" do
    {:not_found, _} = Naming.NameResolver.solve 17, "TREADABLE"
  end

  test "cannot solve an existing TREADABLE id given HOST type" do
    {:not_found, _} = Naming.NameResolver.solve 42, "HOST"
  end

  test "cannot solve an existing HOST id with unexisting type" do
    {:not_found, _} = Naming.NameResolver.solve 17, "CARTRIDGE"
  end

  test "cannot solve an existing TREADABLE id with unexisting type" do
    {:not_found, _} = Naming.NameResolver.solve 36, "MARVIN"
  end
end
