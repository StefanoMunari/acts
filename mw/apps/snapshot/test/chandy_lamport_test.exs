defmodule ChandyLamportTest do
  use ExUnit.Case, async: false
  doctest Snapshot.ChandyLamport

  import Mock

  alias Utils.Routing.Info, as: RoutingInfo

  test "can take a snapshot" do
    pid = self()
    with_mock Forwarding,
      [
        handle_message:
          fn(msg) ->
            if msg.topic === "snapshot" do
              send pid, {:snap, msg}
            else
              send pid, {:app, msg}
            end
          end
      ]
    do
      starter = List.first RoutingInfo.neighbors()
      version = 1
      snap_name = "snap_id#{starter}_v#{version}" |> String.to_atom
      le_me = RoutingInfo.whoami()

      :ok = Snapshot.ChandyLamport.take {starter, version, RoutingInfo.whoami()}
      for node_id <- [ RoutingInfo.whoami() | RoutingInfo.neighbors() ] do
        receive do
          {:snap, msg} ->
            payload = "take.#{starter}.#{version}.#{RoutingInfo.whoami()}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: node_id,
                                         topic:     "snapshot"}
          {:app, msg} ->
            payload = "SNAPSHOT.#{starter}.#{version}"
            assert msg == %Utils.Message{payload:   payload,
                                         recipient: RoutingInfo.whoami(),
                                         topic:     "application"}
        end
      end

      for node_id <- RoutingInfo.neighbors() do
        snapshots = Snapshot.ChandyLamport.submit {starter, version, node_id}
        assert snapshots == [snap_name]
      end
      [] = Snapshot.ChandyLamport.submit {starter, version, le_me}
      resumes =
        for _ <- [ le_me | RoutingInfo.neighbors() ] do
          receive do
            {:snap, msg} ->
              msg
          end
        end
      for node_id <- [ le_me | RoutingInfo.neighbors() ] do
        payload = "resume.#{starter}.#{version}.#{le_me}"
        assert Enum.member?(
          resumes,
          %Utils.Message{payload:   payload,
                         recipient: node_id,
                         topic:     "snapshot"})
      end
    end
  end

  test "submitting a partial snapshot causes nothing" do
    reply = Snapshot.ChandyLamport.submit {"1", 0, "1"}
    assert reply == :ok
  end
end
