use Mix.Config

config :incoming, rec_host: "rabbitmq"
config :incoming, rec_port: 5672
config :incoming, snapshot: Incoming.Sandbox.Snapshot
config :incoming, application: Incoming.Sandbox.Interlayer
config :incoming, coordination: Incoming.Sandbox.Interlayer
