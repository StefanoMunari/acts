defmodule BrokerToAs.Utils.DeepInspection.EnterBus do

  def generate_payload(mw_message) do
    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    bus_id =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Request_Id")
      |> to_int

    boarder_id =
      mw_payload
      |> Map.get("Payload")
      |> to_int

    { :ok, new_payload } =
      JSX.encode (
        %{
          busId:        bus_id,
          pedestrianId: boarder_id,
          districtId:   mw_message.recipient,
          action:       "enter_bus"
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
        mw_payload
        |> Map.get("Payload")
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
