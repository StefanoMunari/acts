defmodule Domain.Acts.DistrictInfo do
  use GenServer

  alias Domain.Acts.DistrictInfo, as: DistrictInfo

  defstruct [
    id:         "",
    name:       "",
    travellers: 0,
    streets:    0
  ]

  ### EXTERNAL APIS

  def start_link(district_id) do
    name = district_name(district_id)
    GenServer.start_link __MODULE__, district_id, name: name
  end

  def district_name(district_id) do
    "DistrictInfo_#{district_id}"
    |> String.to_atom
  end

  def info(district_id) do
    gen_name = district_name district_id
    GenServer.call gen_name, :info
  end

  def travellers(district_id) do
    gen_name = district_name district_id
    GenServer.call gen_name, :travellers
  end
  def set_travellers(district_id, travellers) do
    gen_name = district_name district_id
    GenServer.call gen_name, {:set_travellers, travellers}
  end

  def streets(district_id) do
    gen_name = district_name district_id
    GenServer.call gen_name, :travellers
  end
  def set_streets(district_id, streets) do
    gen_name = district_name district_id
    GenServer.call gen_name, {:set_streets, streets}
  end

  def increment_travellers(district_id) do
    gen_name = district_name district_id
    GenServer.call gen_name, :inc_travellers
  end
  def decrement_travellers(district_id) do
    gen_name = district_name district_id
    GenServer.call gen_name, :dec_travellers
  end

  ### SERVER CALLBACKS

  # CREATE
  def init(district_id) do
    name = Domain.Acts.FancyNames.name_for_district district_id
    { :ok, %DistrictInfo{ id: district_id, name: name } }
  end


  # READ
  def handle_call(:info, _from, state) do
    { :reply, state, state }
  end

  def handle_call(:name, _from, state) do
    { :reply, state.name, state }
  end

  def handle_call(:travellers, _from, state) do
    { :reply, state.travellers, state }
  end

  def handle_call(:streets, _from, state) do
    { :reply, state.streets, state }
  end


  # UPDATE
  def handle_call({:set_streets, streets}, _from, state) do
    { :reply, :ok, %{ state | streets: streets } }
  end
  def handle_call({:set_travellers, travellers}, _from, state) do
    { :reply, :ok, %{ state | travellers: travellers } }
  end

  def handle_call(:dec_travellers, _from, %{ travellers: 0 } = state) do
    { :reply, :ok, state }
  end
  def handle_call(:dec_travellers, _from, state) do
    { :reply, :ok, %{ state | travellers: state.travellers - 1 } }
  end

  def handle_call(:inc_travellers, _from, state) do
    { :reply, :ok, %{ state | travellers: state.travellers + 1 } }
  end
end
