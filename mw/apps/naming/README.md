# Naming

The `naming` application, provided with an entity id and its type, finds the
node on which a given entity resides.

## Configuration

In `config/*.exs` (`dev`, `prod` or `test`) you may change several parameters:

* config_file: file containing a map to find in which node a given entity is
  present

## Installation in other apps under the umbrella

Add `naming` to your list of dependencies in `mix.exs` (of the
non-`naming` app):

    ```elixir
    def deps do
      [{:naming, , in_umbrella: true}]
    end
    ```
