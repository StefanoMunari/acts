defmodule Interface.DistrictsController do
  use Interface.Web,:controller

  alias Domain.Acts.DistrictInfo, as: DistrictInfo

  def index(conn, _params) do
    Domain.districts()
    |> Map.keys
    |> _get_cities_info(Domain.districts())
    |> (fn cities -> Map.put(%{}, :cities, cities) end).()
    |> (fn cities -> json conn, cities end).()
  end

  defp _get_cities_info([ ], _), do: [ ]
  defp _get_cities_info([ key | other_keys ], districts) do
    city_name = Domain.Acts.FancyNames.name_for_city key
    [
      %{
        id:        key,
        name:      city_name,
        districts:
          Enum.map(districts[key],
          fn district ->
            district
            |> (fn district_id -> key <> "_" <> district_id end).()
            |> DistrictInfo.info
          end)
      }
    |
      _get_cities_info(other_keys, districts)
    ]
  end
end
