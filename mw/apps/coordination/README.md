# Coordination

The `coordination` application is responsible of supervising the life-cycle of
an application run on our middleware.

`Coordination.Coordinator` is responsible of initiating the boot process and
the termination process respectively when the boot signal arrives and when the
time limit elapses.

`Coordination.Monitor` supervises the status of some services. In particular,
we instantiated two monitor processes, one for the boot process and another one
for the termination process.
If a process X does not end within a pre-configured time limit (we arbitrarily
set 15 seconds for this parameter) after being started, the monitor issues
again a request to perform X.

## Configuration

There is very little configuration for the `coordination` application (`dev`,
`prod` or `test` environment):

* boot: string used by the application layer to start
* termination: string used by the application layer to shutdown
* lifetime: duration value for the application (in minutes)

## Installation in other apps under the umbrella

Add `coordination` to your list of dependencies in `mix.exs` (of the
non-`coordination` app):

    ```elixir
    def deps do
      [{:coordination, , in_umbrella: true}]
    end
    ```
