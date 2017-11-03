# Incoming

The `incoming` application is responsible of receiving messages from other
middleware nodes and to dispatch these messages to the appropriate middleware
service.
However, before dispatching this component process the message by applying some
checks: in fact, a message may be directed to another node or it might have to
be withheld if a snapshot is occurring.

Our `incoming` application leverages
[GenStage](https://hexdocs.pm/gen_stage/GenStage.html), since each message is
passed through a producer-consumer pipeline before being actually dispatched.
This pipeline is composed by RabbitReceivers, Router, MessageHolder and finally
Dispatcher.

## Configuration

The RabbitReceiver receives messages from several RabbitMQ queues, hence you
must have RabbitMQ running on localhost at the port 5672. If it is _not_ the
case, edit the configuration files `config/dev.exs`, `config/prod.exs` or
`config/test.exs` to specify proper host and port for the RabbitMQ broker.

## Installation in other apps under the umbrella

Add `incoming` to your list of dependencies in `mix.exs` (of the
non-`incoming` app):

    ```elixir
    def deps do
      [{:incoming, , in_umbrella: true}]
    end
    ```
