defmodule Mix.Tasks.RoutingTables do
  use Mix.Task

  @shortdoc "Generates routing table configuration for different envs"

  def run(args) do
    args |> parse_args |> process
  end

  defp parse_args([]) do
    [dev: true, prod: true, test: true]
  end

  defp parse_args(args) do
    {options, _, _} = OptionParser.parse(args,
      switches: [
        [dev: :boolean, prod: :boolean, test: :boolean]
        ])
    options
  end

  defp process([]) do
    raise "Invalid arguments given"
  end

  defp process(envs) do
    Keyword.keys(envs)
    |> Enum.map(&Atom.to_string/1)
    |> Enum.map(&config_routing_table_for/1)
  end

  def get_routing_table_for(node_id)
    when is_integer node_id
  do
    get_routing_table_for Integer.to_string(node_id)
  end

  def get_routing_table_for(node_id) do
    { :ok, routing_table } =
      node_id
      |> routing_table_path
      |> (fn path -> "apps/utils/" <> path end).()
      |> File.read!
      |> JSX.decode
    neighbors =
      routing_table["neighbors"]
      |> Enum.map(&(Kernel.inspect &1))
    table = table_on_the_fly routing_table["routing_table"], %{}
    %{
      id:        routing_table["id"],
      neighbors: neighbors,
      table:     table
    }
  end

  # This is filthy code, please don't look at it
  defp table_on_the_fly([], map) do
    map
  end
  defp table_on_the_fly([ first | residual ], map) do
    key      = first["key"]
    decision = first["decision"]
    table_on_the_fly residual, (Map.put map, key, decision)
  end

  defp config_routing_table_for(env) do

    { :ok, routing_table } =
      env
      |> routing_table_path
      |> File.read!
      |> JSX.decode

    config_file =
      env
      |> config_path
      |> File.read!
      |> String.split("\n")
      |> (Enum.map &(String.trim &1))

    new_file =
      for line <- config_file do
        edit_config line, routing_table
      end
      |> Enum.join("\n")

    File.open!(config_path(env), [:write])
    |> IO.write(new_file)

  end

  defp edit_config("config :forwarding, whoami" <> _, routing_table) do
    "config :forwarding, whoami: \"" <> routing_table["id"] <> "\""
  end

  defp edit_config("config :forwarding, neighbors" <> _, routing_table) do
    neighbors_list = print_list(routing_table["neighbors"])
    "config :forwarding, neighbors: [" <> neighbors_list <> "]"
  end

  defp edit_config("config :forwarding, routing_table" <> _, routing_table) do
    table = print_table(routing_table["routing_table"])
    "config :forwarding, routing_table: %{" <> table <> "}"
  end

  defp edit_config(line, _) do
    line
  end

  defp print_list([]) do
    ""
  end

  defp print_list([ element ]) do
    "#{element}"
  end

  defp print_list(list) do
    list
    |> Enum.map(fn x -> "#{x}" end)
    |> Enum.join(", ")
  end

  def print_table([]) do
    ""
  end

  def print_table([ element ]) do
    dst = element["key"]
    next = element["decision"]
    "#{dst} => #{next}"
  end

  def print_table(list) do
    list
    |> Enum.map(
        fn x -> x["key"] <> " => " <> x["decision"] end)
    |> Enum.join(", ")
  end

  defp routing_table_path(env) do
    "etc/" <> env <> ".conf"
  end

  defp config_path(env) do
    "config/" <> env <> ".exs"
  end
end
