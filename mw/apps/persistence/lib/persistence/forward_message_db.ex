defmodule Persistence.ForwardMessageDb do
  use GenServer

  alias Utils.Message.MessageCodec, as: MessageCodec

  @moduledoc """
  This module acts as an adapter for the forwarded messages DB.
  """

  ### EXTERNAL APIS
  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  @doc """
  Stores a message in the DB.
  """
  def store_msg(msg) do
    GenServer.call __MODULE__, {:store, msg}
  end

  @doc """
  Get all the message in the DB.
  """
  def get_all_messages do
    GenServer.call __MODULE__, :get_all
  end

  @doc """
  Flush all the message in the DB for the forward set.
  """
  def flush do
    GenServer.call __MODULE__, :flush
  end

  ### SERVER CALLBACKS
  @doc """
  Initializes the interface to send requests to localhost:`port`.
  """
  def init(:ok) do
    host = Persistence.host()
    port = Persistence.port()
    Redix.start_link "redis://#{host}:#{port}"
  end

  def handle_call({:store, msg}, _from, conn) do
    encoded_msg = MessageCodec.encode msg
    Redix.command conn, ["RPUSH", queue_name(), encoded_msg]
    {:reply, :ok, conn}
  end

  def handle_call(:get_all, _from, conn) do
    members =
      conn
      |> Redix.command(["LRANGE", queue_name(), 0, -1])
      |> elem(1)
      |> Enum.map(fn member -> MessageCodec.decode(member) end)
    {:reply, members, conn}
  end

  def handle_call(:flush, _from, conn) do
    Redix.command conn, ["DEL", queue_name()]
    {:reply, :ok, conn}
  end

  ### CONFIG STUFF

  defp queue_name do
    Application.get_env :persistence, :forward
  end
end
