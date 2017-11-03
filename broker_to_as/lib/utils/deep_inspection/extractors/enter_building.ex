defmodule BrokerToAs.Utils.DeepInspection.EnterBuilding do

  alias BrokerToAs.Utils.DeepInspection.Vehicle, as: DeepVehicle

  def generate_payload(mw_message) do
    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    traveller_id =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Request_Id")
      |> to_int

    host =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Recipient")
      |> to_int

    traveller_type =
      mw_payload
        |> Map.get("Header")
        |> Map.get("Type")

    passengers =
      mw_payload
      |> DeepVehicle.extract_passengers
      |> Enum.map(fn passenger -> to_int(passenger) end)

    { :ok, new_payload } =
      JSX.encode (
        %{
          travellerId: traveller_id,
          facilityId:  host,
          districtId:  mw_message.recipient,
          carrier:     traveller_type != "PEDESTRIAN",
          passengers:  passengers,
          action:      "enter_building"
        }
      )

    %{mw_message | payload: new_payload}
  end

  def generate_topics(mw_message) do
    persistence =
      "persistence.#{mw_message.sender}"

    districts =
      [ "district.#{mw_message.sender}", "district.#{mw_message.recipient}" ]
      |> Enum.uniq

    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    travellers =
      [
        mw_payload
        |> Map.get("Header")
        |> Map.get("Request_Id")
      |
        DeepVehicle.extract_passengers mw_payload
      ]
      |> Enum.reject(fn traveller -> traveller == "" end)
      |> Enum.map(fn traveller -> "traveller.#{traveller}" end)

    [ persistence | districts ++ travellers ]
    |> List.flatten
    |> Enum.map(fn topic -> %{mw_message | topic: topic} end)
  end

  defp to_int(n)
  when is_integer n do
    n
  end
  defp to_int(n)
  when is_binary n do
    String.to_integer n
  end
  defp to_int(_), do: ""
end
