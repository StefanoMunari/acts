defmodule Interlayer.Formatter do
  @callback format(String.t) :: String.t
  @callback parse(String.t) :: String.t
end
