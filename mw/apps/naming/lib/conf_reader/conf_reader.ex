defmodule Naming.ConfReader do

  @moduledoc """
  This module reads the configuration file for Naming.
  """

  def read_config do
    {:ok, config_map } =
      _config_file()
      |> File.read!
      |> JSX.decode
    config_map
  end

  defp _config_file do
    if Mix.env === :test do
      Application.get_env :utils, :whoami
    else
      group_id = System.get_env "CITY_NODE_ID"
      config_path = Application.get_env :naming, :config_file
      config_path <> group_id <> "/" <> "entities.conf"
    end
  end
end
