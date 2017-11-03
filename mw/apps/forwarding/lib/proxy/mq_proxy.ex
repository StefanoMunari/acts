defmodule Forwarding.Proxy.MQProxy do
  @callback forward(struct) :: atom
end
