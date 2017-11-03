# Snapshot

The `snapshot` application is responsible of providing a way to take
consistent, distributed and possibly concurrent snapshots.

When the application starts, the algorithm in the configuration file is started.
In this case, we implemented the ChandyLamport algorithm which takes consistent
snapshots by leveraging the notion of _logical clocks_, defining the consistency
of a snapshot in a way that the relation of "happened-before" is preserved.

We designed the ChandyLamport implementation as a supervision tree: a main
supervisor monitors both the snapshot executors and the single snapshots which
are currently pending for the middleware node on which this application resides.
Hence, when a new snapshot request arrives, a new snapshot child is started; and
when a snapshot is ended, it gets gracefully terminated by the supervisor.

## Configuration

In `config/config.exs` (`dev`, `prod` or `test`) you may change several
parameters:

* algorithm: algorithm used to take snapshots

## Installation in other apps under the umbrella

Add `snapshot` to your list of dependencies in `mix.exs` (of the
non-`snapshot` app):

    ```elixir
    def deps do
      [{:snapshot, , in_umbrella: true}]
    end
    ```
