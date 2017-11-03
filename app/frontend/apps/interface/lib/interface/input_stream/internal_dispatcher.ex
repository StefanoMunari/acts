defmodule Interface.InputStream.InternalDispatcher do
  use GenStage

  def start_link do
    GenStage.start_link __MODULE__, :ok, name: __MODULE__
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    {
      :producer_consumer,
      :ok,
      subscribe_to: [ Interface.InputStream.Formatter ]
    }
  end

  def handle_events(messages, _from, state) do
    for message <- messages do
      message
      |> Map.get(:topic)
      |> dispatch(message)
      |> (fn arg -> IO.inspect arg; arg end).()
    end
    |> List.flatten
    |> Enum.reject( &(&1 == nil) )
    |> (fn msgs -> { :noreply, msgs, state } end).()
  end

  def dispatch("control:" <> command, message) do
    case command do
      "boot"     ->
        message

      "shutdown" ->
        [ "shutdown", district_id, "done" ] = String.split message.payload, "."
        city_id =
          district_id
          |> String.split("_")
          |> List.first
        Domain.Acts.Loader.load_info [ city_id ], Domain.districts()
        Interface.OutputStream.CityManager.stop city_id
        Domain.district_list()
        |> Enum.filter(fn id -> String.starts_with?(id, city_id) end)
        |> Enum.map(fn id ->
                      %{
                        message |
                          payload: "shutdown"  <> id <> "done",
                          event:   "shutdown",
                          topic:   "district:" <> id }
                    end)

      _          ->
        nil
    end
  end
  def dispatch("district:" <> topic_id, message) do
    if message.event == "exit_district" do
      from_district = topic_id
      to_district   = message.payload["districtId"]
      if from_district != to_district do
        # A traveller went from district `from_district` to district
        # `to_district`
        Domain.Acts.DistrictInfo.increment_travellers to_district
        Domain.Acts.DistrictInfo.decrement_travellers from_district
      end
    end
    message
  end
  def dispatch(_topic, message) do
    message
  end
end
