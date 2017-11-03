# Boot

## Algorithm

The `boot` application is responsible of starting the system in a graceful
fashion.
It does so by dividing the boot phase into two subphases, namely the
**marker diffusion** and the **(own) boot** phase.

In this context, by _marker_ we will mean _boot marker_.

### Marker diffusion

When receiving a marker, this application will forward the marker to all of its
adjacent middleware nodes.

Then, when receiving further markers, `boot` will just reply immediately with
an end marker.

### (Own) boot

After having received all the replies for the markers, `boot` will
request to the application layer to gracefully start.

When the application layer will inform the middleware it started, the `boot`
application will send an end marker to all of its adjacent middleware nodes.

## Installation in other apps under the umbrella

Add `boot` to your list of dependencies in `mix.exs` (of the non-`boot` app):

    ```elixir
    def deps do
      [{:boot, , in_umbrella: true}]
    end
    ```
