use Mix.Config

config :persistence, forward: "greetsF"
config :persistence, mw_app: "greetings"
config :persistence, pending: "testPen"
config :persistence, host: "redis"
config :persistence, port: 6379
