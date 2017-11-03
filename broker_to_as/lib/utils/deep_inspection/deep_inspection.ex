defmodule BrokerToAs.Utils.DeepInspection do

  def inspect(mw_message) do
    extractor =
      mw_message.payload
      |> JSX.decode
      |> elem(1)
      |> _extract_request
      |> _fix_request(mw_message)
      |> _get_extractor

    extractor
      |> _generate_topics(mw_message)
      |> List.flatten
      |> (&(Enum.map(&1, fn msg -> _generate_payload(extractor, msg) end))).()
  end

  defp _generate_topics(extractor, mw_message) do
    extractor.generate_topics mw_message
  end
  defp _generate_payload(extractor, mw_message) do
    extractor.generate_payload mw_message
  end

  defp _extract_request(app_message) do
    app_message
    |> Map.get("Header")
    |> Map.get("Request")
  end

  defp _fix_request("TREAD", mw_message) do
    if mw_message.sender == mw_message.recipient do
      "TREAD"
    else
      "EXIT_DISTRICT"
    end
  end
  defp _fix_request(request, _), do: request

  def _get_extractor(request) do
    case request do
      "QUERY" ->
          BrokerToAs.Utils.DeepInspection.Query
      "EXIT_DISTRICT" ->
          BrokerToAs.Utils.DeepInspection.ExitDistrict
      "EXIT_BUILDING" ->
          BrokerToAs.Utils.DeepInspection.ExitBuilding
      "ENTER_BUILDING" ->
          BrokerToAs.Utils.DeepInspection.EnterBuilding
      "TREAD" ->
          BrokerToAs.Utils.DeepInspection.Tread
      "TRAFFIC_LIGHT" ->
          BrokerToAs.Utils.DeepInspection.Traffic_Light
      "ENTER_BUS" ->
          BrokerToAs.Utils.DeepInspection.EnterBus
      "EXIT_BUS" ->
          BrokerToAs.Utils.DeepInspection.ExitBus
      "SHUTDOWN" ->
          BrokerToAs.Utils.DeepInspection.Control
      _ ->
          BrokerToAs.Utils.DeepInspection.Void_Extractor
    end
  end
end
