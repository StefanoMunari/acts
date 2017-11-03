defmodule BrokerToAs.Utils.DeepInspection.Traffic_Light do

  def generate_payload(mw_message) do
    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    traffic_light_id =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Request_Id")
      |> to_int

    color =
      mw_payload
        |> Map.get("Payload")
        |> Map.get("Message")

    { :ok, new_payload } =
      JSX.encode (
        %{
          trafficLight: traffic_light_id,
          districtId:   mw_message.recipient,
          color:        color,
          action:       "change_color_tl"
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

    traffic_light_id =
      [
        mw_payload
        |> Map.get("Header")
        |> Map.get("Request_Id")
      ]
      |> Enum.map(fn id -> "traffic_light.#{id}" end)

    [ persistence | districts ++ traffic_light_id ]
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
