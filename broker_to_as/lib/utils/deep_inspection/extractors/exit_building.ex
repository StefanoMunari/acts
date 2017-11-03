defmodule BrokerToAs.Utils.DeepInspection.ExitBuilding do

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

    {:ok, new_payload} =
      JSX.encode (
        %{
          travellerId: traveller_id,
          facilityId:  host,
          districtId:  mw_message.recipient,
          carrier:     traveller_type != "PEDESTRIAN",
          passengers:  passengers,
          action:      "exit_building"
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

# "{\"payload\":\"{\\\"Header\\\":{\\\"Call\\\":\\\"ASYNC\\\",\\\"Request\\\":\\\"EXIT_BUILDING\\\",\\\"Request_Id\\\":\\\"50\\\",\\\"Type\\\":\\\"PEDESTRIAN\\\"},\\\"Payload\\\":{\\\"Current_Position\\\":\\\"227\\\",\\\"Current_Speed\\\":\\\"0\\\",\\\"Destination_BIKE\\\":\\\"212,217\\\",\\\"Destination_FOOT\\\":\\\"7,227\\\",\\\"Destination_ROAD\\\":\\\"202,207\\\",\\\"Id\\\":\\\"50\\\",\\\"Maximum_Speed\\\":\\\"5\\\",\\\"Source_BIKE\\\":\\\"43,48\\\",\\\"Source_FOOT\\\":\\\"33,38\\\",\\\"Source_ROAD\\\":\\\"53,58\\\",\\\"Route\\\":[\\\"33,\\\",\\\"34,\\\",\\\"4,\\\",\\\"131,\\\",\\\"132,\\\",\\\"5,\\\",\\\"134,\\\",\\\"135,\\\",\\\"271,\\\",\\\"272,\\\",\\\"273,\\\",\\\"274,\\\",\\\"267,\\\",\\\"264,\\\",\\\"257,\\\",\\\"254,\\\",\\\"247,\\\",\\\"244,\\\",\\\"237,\\\",\\\"234,\\\",\\\"235,\\\",\\\"436,\\\",\\\"437,\\\",\\\"438,\\\",\\\"439,\\\",\\\"440,\\\",\\\"226,\\\",\\\"227,\\\"]}}\",\"recipient\":\"0\",\"sender\":\"0\",\"topic\":\"application.exit_building.0.50\"}"
