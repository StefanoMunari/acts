# Interlayer

The `interlayer` application is responsible of the communication that occurs
between the middleware and the application layer.

Since some runtimes may handle strings in a different format with respect to
what Elixir does, we defined a `Formatter` behaviour for cope with these issues.
We implemented a couple of formatters, one for Elixir (it does nothing but
return the message as it is) and one for Ada.

## Configuration

In `config/config.exs` (`dev`, `prod` or `test`) you may change several
parameters:

* listen_port: port through which messages are sent to the middleware
* send_host: host to which messages are sent
* send_port: port through which messages are sent by the middleware
* formatter: formatter used for the messages exchanged between layers

## Installation in other apps under the umbrella

Add `interlayer` to your list of dependencies in `mix.exs` (of the
non-`interlayer` app):

    ```elixir
    def deps do
      [{:interlayer, , in_umbrella: true}]
    end
    ```
