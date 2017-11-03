defmodule TManagerTest do
  use ExUnit.Case
  doctest Termination.Manager

  import Mock

  alias Utils.Routing.Info, as: RoutingInfo

  #########
  ### 1
  #########

  test "can terminate the system; requested by itself"
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

      Termination.Manager.reset

      Termination.Manager.termination_request le_me

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "termination.req.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "termination"}
        end
      end

      for neighbor <- neighbors do
        Termination.Manager.termination_reply neighbor
      end

      Termination.Manager.node_stopped

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "termination.done.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "termination"}
        end
      end
    end
  end

  #########
  ### 2
  #########

  test "can terminate the system; requested by someone else"
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

      Termination.Manager.reset

      Termination.Manager.termination_request (List.first neighbors)

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "termination.req.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "termination"}
        end
      end

      for neighbor <- neighbors do
        Termination.Manager.termination_reply neighbor
      end

      Termination.Manager.node_stopped

      for neighbor <- neighbors do
        receive do
          {:msg, msg} ->
            payload = "termination.done.#{le_me}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: neighbor,
                                         topic:     "termination"}
        end
      end
    end
  end
end
