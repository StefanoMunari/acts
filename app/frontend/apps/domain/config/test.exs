use Mix.Config

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :domain, Domain.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "cds",
  database: "fe_test",
  hostname: "fe_postgres",
  pool: Ecto.Adapters.SQL.Sandbox,
  brokers: [],
  brokers_port: 5672