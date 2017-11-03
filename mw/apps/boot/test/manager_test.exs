defmodule ManagerTest do
  use ExUnit.Case
  doctest Boot.Manager

  import Mock

  alias Utils.Routing.Info, as: RoutingInfo

  #########
  ### 1
  #########

  test "can boot the system; requested by itself"
  do
    pid = self()
    with_mock Forwarding,
      [
        handle_message:
          fn(msg) ->
            send pid, {:msg, msg}
          end
      ]
    do
      le_me = RoutingInfo.whoami()
      neighbors = RoutingInfo.neighbors()

      Boot.Manager.reset

      Boot.Manager.boot_request le_me

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "boot.req.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "boot"}
        end
      end

      for neighbor <- neighbors do
        Boot.Manager.boot_reply neighbor
      end

      Boot.Manager.node_ready

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "boot.done.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "boot"}
        end
      end
    end
  end

  #########
  ### 2
  #########

  test "can boot the system; requested by someone else"
  do
    pid = self()
    with_mock Forwarding,
      [
        handle_message:
          fn(msg) ->
            send pid, {:msg, msg}
          end
      ]
    do
      le_me = RoutingInfo.whoami()
      neighbors = RoutingInfo.neighbors()

      Boot.Manager.reset

      Boot.Manager.boot_request (List.first neighbors)

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "boot.req.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "boot"}
        end
      end

      for neighbor <- neighbors do
        Boot.Manager.boot_reply neighbor
      end

      Boot.Manager.node_ready

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "boot.done.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "boot"}
        end
      end
    end
  end
end
