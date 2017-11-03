defmodule Naming.Mixfile do
  use Mix.Project

  def project do
    [
      app: :naming,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Naming, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:exjsx, git: "https://github.com/talentdeficit/exjsx.git", tag: "4.0.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true},
    ]
  end
end
