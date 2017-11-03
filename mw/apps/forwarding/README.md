# Forwarding

The `forwarding` application is responsible of delivering messages to other
nodes.
It does so by handing over the messages to a RabbitMQ sender, which takes care
of forwarding a message by means of a RabbitMQ broker.

Therefore, in order to make this application work, you must have RabbitMQ
running on localhost at the port 5672. If it is _not_ the case, edit the
configuration files `config/dev.exs`, `config/prod.exs` or `config/test.exs` to
specify proper host and port for the RabbitMQ broker.

## Build your own alternative sender

If you want to use another custom sender, just build a module which implements
the `Forwarding.Proxy.MQProxy` behaviour (you can find it in the
`lib/forwarding/proxy` folder).

Then, change the configuration files `config/dev.exs`, `config/prod.exs`,
`config/test.exs` to include this new sender in a given execution environment
(respectively development, production and test).

## Installation in other apps under the umbrella

Add `forwarding` to your list of dependencies in `mix.exs` (of the
non-`forwarding` app):

    ```elixir
    def deps do
      [{:forwarding, , in_umbrella: true}]
    end
    ```
