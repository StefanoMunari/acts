defmodule BrokerToAs.Utils.DeepInspection.Query do

  def generate_payload(mw_message) do
    mw_payload =
      mw_message.payload
      |> JSX.decode
      |> elem(1)

    traveller_id =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Request_Id")

    host =
      mw_payload
      |> Map.get("Header")
      |> Map.get("Recipient")

    query =
      mw_payload
        |> Map.get("Payload")
        |> Map.get("Message")
        |> JSX.decode
        |> elem(1)

    query_name = Map.get(query, "proc")
    vehicle_id = List.first _get_traveller_from_queries({query_name, query})

    { :ok, new_payload } =
      JSX.encode (
        %{
          traveller_id: traveller_id,
          host:         host,
          query:        query_name,
          vehicle_id:   vehicle_id}
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
        |> Map.get("Message")
        |> JSX.decode
        |> elem(1)
        |> (fn query -> { Map.get(query, "proc"), query } end).()
        |> _get_traveller_from_queries
      ]
      |> Enum.map(fn traveller -> "traveller.#{traveller}" end)

    [ persistence | districts ++ travellers ]
    |> Enum.map(fn topic -> %{mw_message | topic: topic} end)
  end

  defp _get_traveller_from_queries({ "Book_Parking", query }) do
    [ query["coId"] ]
  end
  defp _get_traveller_from_queries({query, _arg}) do
    []
  end
end

# "{\"payload\":\"{\\\"Header\\\":{\\\"Call\\\":\\\"SYNC\\\",\\\"Recipient\\\":\\\"9\\\",\\\"Recipient_Type\\\":\\\"HOST\\\",\\\"Request\\\":\\\"QUERY\\\",\\\"Request_Id\\\":\\\"95\\\",\\\"Type\\\":\\\"ACK\\\"},\\\"Payload\\\":{\\\"Message\\\":\\\"{\\\\\\\"coId\\\\\\\":\\\\\\\"115\\\\\\\",\\\\\\\"proc\\\\\\\":\\\\\\\"Book_Parking\\\\\\\",\\\\\\\"args\\\\\\\":[153,\\\\\\\"115\\\\\\\"]}\\\"}}\",\"recipient\":\"1\",\"sender\":\"0\",\"topic\":\"application.query.0.95\"}"
