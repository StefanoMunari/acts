use Mix.Config

config :interlayer, listen_port: 8081
config :interlayer, send_host: "localhost"
config :interlayer, send_port: 8081
config :interlayer, formatter: Interlayer.Formatter.ElixirFormatter
config :interlayer, resolver: Interlayer.Sandbox.Solver
config :interlayer, forwarder: Interlayer.Sandbox.Forwarder
