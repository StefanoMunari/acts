use Mix.Config

config :persistence, forward: "forward"
config :persistence, mw_app: "messages"
config :persistence, pending: "pending"
config :persistence, host: "redis"
config :persistence, port: 6379
