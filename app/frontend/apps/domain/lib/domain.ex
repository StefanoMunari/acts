defmodule Domain do
  use Application

  alias Domain.Acts.Loader, as: Loader
  alias Domain.Acts.DistrictInfo, as: DistrictInfo

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children =
      for district <- district_list() do
        worker(DistrictInfo,
               [ district ],
               [ id: DistrictInfo.district_name district ]
        )
      end
    ++
    [
      worker(Loader, [ districts() ])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Domain.Supervisor]
    Supervisor.start_link children, opts
  end

  def cities do
    "etc/*"
    |> Path.wildcard
    |> Enum.reject( &(String.contains?(&1, ".")) )
    |> Enum.map(&(String.replace_prefix &1, "etc/", ""))
  end

  def district_list do
    districts()
    |> Enum.map(
        fn {k, v} ->
          Enum.map(v, &(k <> "_" <> &1))
        end
       )
    |> Enum.concat
  end

  def districts do
    "etc/*"
    |> Path.wildcard
    |> Enum.reject( &(String.contains?(&1, ".")) )
    |> Enum.map(&(&1 <> "/*"))
    |> Enum.map(&Path.wildcard/1)
    |> map_city_to_districts(%{})
  end

  defp map_city_to_districts([], map) do
    map
  end
  defp map_city_to_districts([ [ ] | tail ], map) do
    map_city_to_districts tail, map
  end
  defp map_city_to_districts([ head | tail ], map) do
    city_name =
      head
      |> List.first
      |> String.split("/")
      |> List.pop_at(1)
      |> elem(0)
    districts =
      head
      |> Enum.map(&(String.replace_prefix &1, "etc/", ""))
      |> Enum.map(&(String.replace_suffix &1, ".conf", ""))
      |> Enum.map(&(drop_until(&1, "/")))
      |> Enum.map(&(String.replace        &1, "/district", ""))
      |> Enum.map(&(String.replace        &1, "/active", ""))
      |> Enum.reject(&(String.contains?(&1, "neigh")))
      |> Enum.uniq
      |> Enum.reject(&(&1 == ""))
    map_city_to_districts tail, Map.put(map, city_name, districts)
  end

  defp drop_until("", symbol), do: ""
  defp drop_until(str, symbol) do
    if String.first(str) == symbol do
      str
    else
      drop_until String.slice(str, 1..-1), symbol
    end
  end
end
