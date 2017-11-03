defmodule Interlayer.Pending.Consumer do
  use GenStage

  alias Interlayer.Pending.Consumer, as: Consumer
  alias Utils.Message, as: Message
  alias Utils.Routing.Info, as: RoutingInfo

  defstruct [
    terminated: false
  ]

    ### CLIENT API

  @doc """

  """
  #@spec start_link :: any
  def start_link() do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### STAGE CALLBACKS
  def init(:ok) do
    {:consumer, %Consumer{}, subscribe_to: [Interlayer.MwAppInterface]}
  end

  def handle_events(_messages, _from, %{ terminated: true } = state) do
    { :noreply, [], state }
  end
  def handle_events(messages, _from, state) do
    new_state = send_messages messages, state
    { :noreply, [], new_state }
  end

  def send_messages(_, %{ terminated: true } = state) do
    state
  end
  def send_messages([], state) do
    state
  end
  def send_messages([ message | rest ], state) do
    send_to_client message.payload
    if (Message.get_request(message) == "QUERY" or
        Message.get_request(message) == "ENTER")
       and Message.get_type(message) != "ACK"
    do
      Persistence.PendingRequestDb.store(
        Message.get_request_id(message),
        message.sender,
        Message.get_request(message)
      )
      Persistence.MwAppMessageDb.remove
    end
    terminated = Message.get_request(message) == "SHUTDOWN"
    if terminated do
      send_to_client message.payload
    end
    IO.inspect "Terminated: #{terminated}"
    send_messages rest, %{ state | terminated: terminated }
  end

  #@spec send_to_client(<>) :: {:ok, <>}
  def send_to_client(content)
  when is_binary content
  do
    IO.inspect "Sending upwards... #{content}"
    socket  = Socket.TCP.connect! send_host(), send_port()
    send_chunks(socket, content)
    socket |> Socket.Stream.close
    {:ok, "Message delivered"}
  end
  def send_to_client(_) do
    { :ok, "" }
  end

  defp send_chunks(_, "") do
    :ok
  end
  defp send_chunks(socket, str) do
    { now, later } = String.split_at(str, 100)
    data = formatter().format now
    socket |> (Socket.Stream.send! data)
    send_chunks socket, later
  end

  #CONFIG STUFF

  defp formatter do
    Application.get_env :interlayer, :formatter
  end

  defp send_host do
    host_prefix = Application.get_env :interlayer, :send_host
    node_id =
      RoutingInfo.whoami()
      |> String.split("_")
      |> Enum.at(1)
    host_prefix <> node_id
  end

  defp send_port do
    Application.get_env :interlayer, :send_port
  end
end
