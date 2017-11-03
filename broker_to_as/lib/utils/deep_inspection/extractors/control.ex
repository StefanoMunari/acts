defmodule BrokerToAs.Utils.DeepInspection.Control do

  def generate_payload(mw_message) do
    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    service =
      mw_payload
        |> Map.get("Header")
        |> Map.get("Request")
        |> String.downcase

    district_id = mw_message.recipient

    %{ mw_message | payload: service <> "." <> district_id <> ".done" }
  end

  def generate_topics(mw_message) do

    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    service =
      mw_payload
        |> Map.get("Header")
        |> Map.get("Request")
        |> String.downcase

    control =
      "control.#{service}"

    districts =
      [ "district.#{mw_message.sender}", "district.#{mw_message.recipient}" ]
      |> Enum.uniq

    [ control | districts ]
    |> List.flatten
    |> Enum.map(fn topic -> %{mw_message | topic: topic} end)
  end
end
