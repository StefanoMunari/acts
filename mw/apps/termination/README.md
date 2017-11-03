# Termination

## Algorithm

The `termination` application is responsible of shutting down the system in a
graceful fashion.
It does so by dividing the shutdown phase into two subphases, namely the
**marker diffusion** and the **(own) termination** phase.

In this context, by _marker_ we will mean _termination marker_.

### Marker diffusion

When receiving a marker, this application will forward the marker to all of its
adjacent middleware nodes.

Then, when receiving further markers, `termination` will just reply immediately
with an end marker.

### (Own) termination

After having received all the replies for the markers, `termination` will
request to the application layer to gracefully shutdown.

When the application layer will inform the middleware it has stopped, the
`termination` application will send an end marker to all of its adjacent
middleware nodes.

## Installation in other apps under the umbrella

Add `termination` to your list of dependencies in `mix.exs` (of the
non-`termination` app):

    ```elixir
    def deps do
      [{:termination, , in_umbrella: true}]
    end
    ```
