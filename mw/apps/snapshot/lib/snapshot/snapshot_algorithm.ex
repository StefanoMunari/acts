defmodule Snapshot.SnapshotAlgorithm do
  # first arg is the logical clock instant in which the snapshot was requested
  @callback take(any) :: atom
  # first arg is the id of the node who submitted its snapshot
  @callback submit(any) :: atom
end
