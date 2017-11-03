use Mix.Config

# Configure your database
config :domain, Domain.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "cds",
  database: "fe_dev",
  hostname: "fe_postgres",
  port: "5432",
  ssl: false,
  pool: Ecto.Adapters.SQL.Sandbox,
  brokers: [],
  brokers_port: 5672
