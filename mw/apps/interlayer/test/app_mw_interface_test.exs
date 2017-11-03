defmodule AppMwInterfaceTest do
  use ExUnit.Case
  doctest Interlayer.AppMwInterface

  alias Utils.Message, as: Message

  import Mock

  test "deprecated" do
    assert 1 = 1
  end
end
