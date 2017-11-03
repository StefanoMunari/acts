use Mix.Config

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :interface, Interface.Endpoint,
  http: [port: 4000],
  debug_errors: false,
  code_reloader: false,
  check_origin: false,
  watchers: [
    npm: ["run", "clean", cd: Path.expand("../", __DIR__)],
    npm: ["run", "copy-watch", cd: Path.expand("../", __DIR__)],
    npm: ["run", "webpack-watch", cd: Path.expand("../", __DIR__)]
  ]

# Watch static and templates for browser reloading.
config :interface, Interface.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*$},
      ~r{priv/gettext/.*(po)$},
      ~r{web/views/.*(ex)$},
      ~r{web/templates/.*(eex)$}
    ]
  ]

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development.
# Do not configure such in production as keeping
# and calculating stacktraces is usually expensive.
# TODO: Disable this
config :phoenix, :stacktrace_depth, 20

config :interface, broker: "brk_fe_rabbitmq"
config :interface, broker_port: 5672
