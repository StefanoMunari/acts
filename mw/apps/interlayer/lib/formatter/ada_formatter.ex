defmodule Interlayer.Formatter.AdaFormatter do
  @behaviour Interlayer.Formatter

  def format(data) do
    <<1,0,0,0,byte_size(data),0,0,0>> <> data
  end

  def parse(<< _a, 0, 0, 0, _e, _f, _g, _h >> <> data) do
    data
  end

  def parse(nil) do
    ""
  end

  def parse(something) do
    something
  end
end
