defmodule Interface.OutputStream.CityManager do
  use GenServer

  alias Interface.OutputStream.CityManager, as: CityManager

  defstruct [
    id:    "",
    state: :stopped,
    chan:  nil,
  ]

  ### EXTERNAL APIS

  def start_link(id, hostname) do
    { :ok, connection } =
      AMQP.Connection.open host: hostname, port: port()
    { :ok, chan } =
      AMQP.Channel.open connection
    GenServer.start_link __MODULE__, {id, chan}, name: manager_name(id)
  end

  def manager_name(city_id) do
    "OutputManager_#{city_id}"
    |> String.to_atom
  end

  def stream(district_id) do
    district_id
    |> String.split("_")
    |> List.first
    |> manager_name
    |> GenServer.call({:start, district_id})
  end

  def stop(city_id) do
    city_id
    |> manager_name
    |> GenServer.call(:stop)
  end

  ### SERVER CALLBACKS

  def init({ id, chan }) do
    AMQP.Exchange.declare chan, output_exchange(), :topic
    { :ok, %CityManager{id: id, chan: chan} }
  end

  def handle_call({ :start, district_id }, _from, state)
  do
    AMQP.Basic.publish state.chan,
                       output_exchange(),
                       "fe.command." <> state.id,
                       "stream." <> district_id
    { :reply, :ok, %{ state | state: :streaming } }
  end

  def handle_call(:stop, _from, %{ state: :streaming } = state)
  do
    { :reply, :ok, %{ state | state: :stopped } }
  end
  def handle_call(:stop, _from, state), do: { :reply, :ok, state }

  defp output_exchange do
    "broker_to_as"
  end

  defp port do
    Application.get_env :interface, :broker_port
  end
end
