defmodule Utils.Message do

  defstruct  [
    payload:   "",
    recipient: "",
    sender:    "",
    async:     false,
    topic:     ""
  ]

  def get_request_id(msg) do
    get_header_from_payload(msg)
      |> Map.get("Request_Id")
    || msg.topic
  end

  def get_type(msg) do
    get_header_from_payload(msg)
      |> Map.get("Type")
  end

  def get_request(msg) do
    get_header_from_payload(msg)
      |> Map.get("Request")
  end

  def get_header_from_payload(msg) do
    msg.payload
      |> JSX.decode
      |> elem(1)
      |> Map.get("Header")
  end

end
