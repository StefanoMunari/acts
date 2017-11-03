defmodule Interlayer.Mixfile do
  use Mix.Project

  def project do
    [app: :interlayer,
     version: "0.1.0",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.5",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger],
     mod: {Interlayer, []}]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # To depend on another app inside the umbrella:
  #
  #   {:myapp, in_umbrella: true}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:socket, git: "https://github.com/meh/elixir-socket"},
      {:poison, "~> 3.0"},
      {:mock, "~> 0.2.0", only: :test},
      {:gen_stage, "~> 0.11"},
      {:qex, "~> 0.3"},
      {:coordination, in_umbrella: true},
      {:forwarding, in_umbrella: true},
      {:naming, in_umbrella: true},
      {:persistence, in_umbrella: true},
      {:utils, in_umbrella: true}
    ]
  end
end
