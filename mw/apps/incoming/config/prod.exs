use Mix.Config

config :incoming, rec_host: "mw_brk_rabbitmq"
config :incoming, rec_port: 5672
config :incoming, boot: Boot
config :incoming, interlayer: Interlayer
config :incoming, application: Interlayer
config :incoming, snapshot: Snapshot
config :incoming, termination: Termination
config :incoming, coordination: Coordination
