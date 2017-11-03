defmodule Interface.Mixfile do
  use Mix.Project

  def project do
    [app: :interface,
     version: "0.0.1",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.0",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     aliases: aliases(),
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [mod: {Interface, []},
     applications: [:phoenix, :phoenix_html, :cowboy, :logger, :gettext,
     :comeonin, :plug_graphql, :phoenix_pubsub, :domain]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(:teste2e), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.2"},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_html, "~> 2.4"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:gettext, "~> 0.12"},
      {:cowboy, "~> 1.0"},
      {:joken, "~> 1.3"},
      {:guardian, "~> 0.13.0"},
      {:comeonin, "~> 2.6"},
      {:plug_graphql, "~> 0.2"},
      {:domain, in_umbrella: true},
      {:amqp, "~> 0.2.1"},
      {:gen_stage, "~> 0.11"},
      {:wabbit, git: "https://github.com/pma/wabbit"},
      {:exjsx, git: "https://github.com/talentdeficit/exjsx.git", tag: "4.0.0"},
    ]
  end

  # Aliases are shortcut or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    ["s": ["phoenix.server"]]
  end
end
