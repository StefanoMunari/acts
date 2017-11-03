defmodule Domain.Acts.Loader do
  use Task

  alias Domain.Acts.DistrictInfo, as: DistrictInfo

  def start_link(district_map) do
    GenServer.start_link __MODULE__, district_map, name: __MODULE__
  end

  def load_info([ ], _) do
    [ ]
  end
  def load_info([ key | other_keys ], district_map) do
    for district <- district_map[key] do
      if String.contains?(district, "neigh") do
        []
      else
        full_dis = key <> "_" <> district
        DistrictInfo.set_travellers full_dis, _get_no_of_travellers(full_dis)
        DistrictInfo.set_streets full_dis, _get_no_of_streets(full_dis)
      end
    end
    |> List.flatten
    load_info other_keys, district_map
  end

  defp _get_no_of_travellers(district) do
    [ city, district_id ] = String.split district, "_"
    json =
      File.read!("etc/#{city}/active#{district_id}.conf")
      |> Poison.decode!()
    length(json["travellers"])
  end

  defp _get_no_of_streets(district) do
    [ city, district_id ] = String.split district, "_"
    json =
      File.read!("etc/#{city}/district#{district_id}.conf")
      |> Poison.decode!()
    length(json["streets"])
  end

  ### SERVER CALLBACKS

  # CREATE
  def init(district_map) do
    district_map
    |> Map.keys
    |> load_info(district_map)
    { :ok, :ok }
  end
end
