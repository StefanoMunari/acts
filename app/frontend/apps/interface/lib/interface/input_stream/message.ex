defmodule Interface.InputStream.Message do

  alias Interface.InputStream.Message, as: Message

  defstruct [
    payload: "",
    event:   "",
    topic:   ""
  ]

  def create(json) do
    case JSX.decode json do
      { :ok, msg } ->
        case msg["topic"] do
          "control" <> _     -> # control message
            %Message{
              payload: msg["payload"],
              event:   msg["payload"],
              topic:   msg["topic"]
            }
          _                  ->
            case JSX.decode(msg["payload"]) do
              { :ok, payload} -> # event message
                action = payload["action"]
                %Message{
                  payload: payload,
                  event:   action,
                  topic:   msg["topic"]
                }
              _ ->
                %Message{}
            end
        end
      _ ->
        %Message{}
    end
  end
end
