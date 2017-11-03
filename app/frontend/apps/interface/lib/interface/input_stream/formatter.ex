defmodule Interface.InputStream.Formatter do
  use GenStage

  def start_link do
    GenStage.start_link __MODULE__, :ok, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    { :producer_consumer, :ok, subscribe_to: [ Interface.InputStream ] }
  end

    def handle_events(messages, _from, state) do
    # for each message
    for { message, _meta } <- messages do
      message
      |> Interface.InputStream.Message.create
      |> Map.put_new(:topic, "")
      |> (fn msg ->
            Map.put(msg, :topic, String.replace(msg.topic, ".", ":"))
          end).()
    end
    |> Enum.reject( &(&1.topic == "") )
    |> (fn msgs -> { :noreply, msgs, state } end).()
  end
end

# %Message{event: "exit_building", payload: %{"action" => "exit_building", "carrier" => false, "facilityId" => "16", "passengers" => [], "travellerId" => "82"}, topic: "district.0"}
