defmodule AdaFormatterTest do
  use ExUnit.Case
  doctest Interlayer.Formatter.AdaFormatter

  test "formats empty string" do
    formatted = Interlayer.Formatter.AdaFormatter.format ""
    assert formatted == <<1,0,0,0, 0,0,0,0>>
  end

  test "formats simple text" do
    formatted = Interlayer.Formatter.AdaFormatter.format "joearmstrong"
    assert formatted == <<1,0,0,0, 12,0,0,0>> <> "joearmstrong"
  end

  assert_raise ArgumentError, fn -> Interlayer.Formatter.AdaFormatter.format 4
  end

  test "parse empty string" do
    parsed = Interlayer.Formatter.AdaFormatter.parse <<1,0,0,0, 0,0,0,0>>
    assert parsed == ""
  end

  test "parse simple text" do
    ada_text = <<1,0,0,0, 0,0,0,0>> <> "joearmstrong"
    parsed = Interlayer.Formatter.AdaFormatter.parse ada_text
    assert parsed == "joearmstrong"
  end
end
