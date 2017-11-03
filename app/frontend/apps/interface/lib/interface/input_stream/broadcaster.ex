defmodule Interface.InputStream.Broadcaster do
  use GenStage

  ## EXTERNAL APIS

  def start_link do
    GenStage.start_link __MODULE__, :ok, name: __MODULE__
  end

  ## SERVER CALLBACKS

  def init(_state) do
    {
      :consumer,
      :ok,
      subscribe_to: [ Interface.InputStream.InternalDispatcher ]
    }
  end

  def handle_events(messages, _from, state) do
    # for each message
    for message <- messages do
      IO.inspect "Broadcasting: #{inspect message}"
      broadcast_message message
    end
    { :noreply, [], state }
  end

  defp broadcast_message(nil), do: nil
  defp broadcast_message(message)
  when not is_map message do
    nil
  end
  defp broadcast_message(message) do
    case message.event do
      nil ->
        nil
      event ->
        IO.inspect "Topic: #{message.topic}"
        Interface.Endpoint.broadcast message.topic,
                                     event,
                                     %{"body" => message.payload}
    end
  end
end
