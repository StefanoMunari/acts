# Persistence

The `persistence` application is responsible of providing access to databases
for specifical middleware applications.

Since the middleware needs just a couple of queues for both the `forwarding` and
`interlayer` applications, we decided to load this information in Redis queues
by using the [Redix](https://github.com/whatyouhide/redix) package.

## Configuration

In `config/config.exs` (`dev`, `prod` or `test`) you may change the name of the
queues used by Redis to store information, along with the host and port used by
Redis.

## Installation in other apps under the umbrella

Add `persistence` to your list of dependencies in `mix.exs` (of the
non-`persistence` app):

    ```elixir
    def deps do
      [{:persistence, , in_umbrella: true}]
    end
    ```
