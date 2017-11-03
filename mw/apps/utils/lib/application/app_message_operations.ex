defmodule Utils.AppMessageOperations do

  alias Utils.AppMessage,   as: AppMessage
  alias Utils.Message,      as: MwMessage
  alias Utils.Routing.Info, as: RoutingInfo

  def decode(message) do
    {outcome, decoded_message} = JSX.decode message
    if outcome == :ok
       and (is_map decoded_message)
       and (Map.has_key? decoded_message, headers_key())
    do
      %AppMessage{headers: decoded_message[headers_key()],
                  payload: message}
    else
      nil
    end
  end

  def has_recipient?(map) do
    (Map.has_key? map, "Recipient")
    and
    (Map.has_key? map, "Recipient_Type")
  end

  def get_recipient_from_message(message) do
    if has_recipient?(message.headers) do
      headers = message.headers
      entity_id   = headers["Recipient"]
      entity_type = headers["Recipient_Type"]
      %{message | recipient: { entity_id, entity_type } }
    else
      message
    end
  end

  def create_mw_message_from_app_message(nil) do
    nil
  end
  def create_mw_message_from_app_message(message) do
    headers = message.headers
    if Map.has_key? headers, request_key() do
      headers[request_key()]
      |> String.downcase
      |> _create_mw_msg(message)
    else
      nil
    end
  end

  defp _create_mw_msg("boot", message) do
      payload   = "ready"
      recipient = message.recipient
      topic     = "boot"
      %MwMessage{
        payload:   payload,
        recipient: recipient,
        sender:    sender(),
        topic:     topic
      }
  end
  defp _create_mw_msg("shutdown", message) do
      payload   = "stopped"
      recipient = message.recipient
      topic     = "termination"
      %MwMessage{
        payload:   payload,
        recipient: recipient,
        sender:    sender(),
        topic:     topic
      }
  end
  defp _create_mw_msg("snapshot", message) do
      # TODO:     Figure out how to retrieve node_id and version.
      # PROPOSAL: From the app layer message payload?
      node_id   = "???"
      version   = "???"
      payload   = "resume.#{node_id}.#{version}.#{RoutingInfo.whoami()}"
      recipient = message.recipient
      topic     = "snapshot"
      %MwMessage{
        payload:   payload,
        recipient: recipient,
        sender:    sender(),
        async:     true,
        topic:     topic
      }
  end
  defp _create_mw_msg("ack", message) do
      { present, recipient, req } =
        message.payload
        |> JSX.decode
        |> elem(1)
        |> Map.get("Header")
        |> Map.get("Request_Id")
        |> Persistence.PendingRequestDb.get_and_delete
      if present == :present do
        _create_mw_msg req, %{ message| recipient: recipient }
      else
        nil
      end
  end
  defp _create_mw_msg(request, message) do
      payload   = message.payload
      recipient = message.recipient
      node_id   = RoutingInfo.whoami()
      req_id    = message.headers["Request_Id"] || ""
      async     = message.headers["Call"] || ""
      topic     =
        "application" <>
        "." <> request <>
        "." <> node_id <>
        "." <> req_id
      %MwMessage{
        payload:   payload,
        recipient: recipient,
        sender:    sender(),
        async:     async == "ASYNC",
        topic:     topic
      }
  end


  def create_app_message_as_payload(str) do
    Map.new
    |> Map.put(
        headers_key(),
        Map.new
        |> Map.put(request_key(), String.upcase str)
        |> Map.put(type_key(), "ACK"))
    |> Map.put(
        payload_key(),
        Map.new
        |> Map.put("Ack", true)
      )
    |> JSX.encode
    |> elem(1)
  end


  defp sender do
    RoutingInfo.whoami()
  end


  defp headers_key do
    "Header"
  end
  defp payload_key do
    "Payload"
  end
  defp request_key do
    "Request"
  end
  defp type_key do
    "Type"
  end
end
