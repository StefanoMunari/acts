defmodule Interface.InfrastructureController do
  use Interface.Web,:controller

  alias Domain.Acts.FancyNames, as: FancyNames

  def index(conn, params) do
    city_id = params["cityId"]
    district_id = params["districtId"] |> String.split("_") |> List.last
    neighbors = "etc/#{city_id}/neighbors#{district_id}.conf"
    |> File.read!()
    |> Poison.decode!()
    |> Map.get("neighbors")
    "etc/#{city_id}/district#{district_id}.conf"
    |> File.read!()
    |> Poison.decode!()
    |> Map.put("name", FancyNames.name_for_district(params["districtId"]))
    |> Map.put("neighbors", neighbors)
    |> (fn district_json -> json conn, district_json end).()
  end
end
