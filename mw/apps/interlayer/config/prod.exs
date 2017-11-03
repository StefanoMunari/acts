use Mix.Config

config :interlayer, listen_port: 8081
config :interlayer, send_host: "backend"
config :interlayer, send_port: 8082
config :interlayer, formatter: Interlayer.Formatter.AdaFormatter
config :interlayer, resolver: Naming.NameResolver
config :interlayer, forwarder: Forwarding
