# Mw

This Umbrella application is the middleware for our City Simulator.

In the `apps` folder you can find the different apps which compose the
middleware, along with their role in it.

## Installation under the umbrella

You will find detailed instructions on how to install an application in this
umbrella in each one of the applications.

Please be careful when installing an application `fancy_app`: the code block we
provide contains annotations for Markdown syntax highlighting, if supported
(the first and the last line in the following snippet, if you are able to see
'elixir' in the first line).

    ```elixir
    def deps do
      [{:fancy_app, , in_umbrella: true}]
    end
    ```

