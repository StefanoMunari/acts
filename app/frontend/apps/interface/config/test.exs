use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :interface, Interface.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

config :interface, broker: "brk_fe_rabbitmq"
config :interface, broker_port: 5672