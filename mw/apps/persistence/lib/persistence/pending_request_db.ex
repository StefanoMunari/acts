defmodule Persistence.PendingRequestDb do
  use GenServer

  @moduledoc """
  This module acts as an adapter for the mw-app pending requests DB.
  """

  ### EXTERNAL APIS
  def start_link do
    GenServer.start_link __MODULE__, :ok, name: __MODULE__
  end

  @doc """
  Stores a message in the DB.
  """
  def store(key, value1, value2) do
    GenServer.call __MODULE__, {:store, key, value1, value2}
  end

  @doc """
  Get all the message in the DB.
  """
  def get_and_delete(key) do
    GenServer.call __MODULE__, {:get_and_delete, key}
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

  def handle_call({:store, key, value1, value2}, _from, conn) do
    key1 = key <> "1"
    key2 = key <> "2"
    if (Redix.command(conn, ["HEXISTS", queue_name(), key1])) == {:ok, 0} do
      Redix.command conn, [ "HSET", queue_name(), key1, value1 ]
      Redix.command conn, [ "HSET", queue_name(), key2, value2 ]
    end
    {:reply, :ok, conn}
  end

  def handle_call({:get_and_delete, key}, _from, conn) do
    key1 = key <> "1"
    key2 = key <> "2"
    if (Redix.command(conn, ["HEXISTS", queue_name(), key1])) == {:ok, 1} do
      {:ok, value1} = Redix.command conn, [ "HGET", queue_name(), key1 ]
      {:ok, value2} = Redix.command conn, [ "HGET", queue_name(), key2 ]
      Redix.command conn, [ "HDEL", queue_name(), key1 ]
      Redix.command conn, [ "HDEL", queue_name(), key2 ]
      {:reply, {:present, value1, value2}, conn}
    else
      {:reply, {:none, :none, :none}, conn}
    end
  end

  ### CONFIG STUFF

  defp queue_name do
    Application.get_env :persistence, :pending
  end
end
