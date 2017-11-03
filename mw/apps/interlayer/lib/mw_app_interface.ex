defmodule Interlayer.MwAppInterface do
  use GenStage

  @moduledoc """
  This module provides an interface to make requests *to* the application layer.
  """


  ### CLIENT API

  @doc """
  Starts the client to the application by selecting the port `port` through
  which communication towards the application are performed.
  """
  @spec start_link() :: any
  def start_link() do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def sync_to_application(message, timeout \\ 5000) do
    GenServer.call(__MODULE__, {:send, message}, timeout)
  end

  ### SERVER CALLBACKS
  @doc """
  Initializes the interface to send requests to localhost:`port`.
  Restores messages which have not been sent.
  """
  @spec init(:ok) :: {:ok, map}
  def init(:ok) do
    restored_messages = []
      #Persistence.MwAppMessageDb.get_all_messages
    # do not re-send control messages
    #|> Enum.reject(fn msg -> msg.topic == "interlayer" end)
    {
      :producer,
      { Qex.new, 0, restored_messages },
      dispatcher: GenStage.BroadcastDispatcher
    }
  end

  @doc """
  Sends a message `what` to the application layer.
  """
  @spec handle_call({:send, <<>>}, any, any) :: any
  def handle_call({:send, what}, from, {queue, demand, messages}) do
    Persistence.MwAppMessageDb.store_msg what
    dispatch_messages(Qex.push(queue, {from, what}), demand, messages)
  end

  def handle_demand(incoming_demand, {queue, demand, messages}) do
    dispatch_messages(queue, incoming_demand + demand, messages)
  end

  defp dispatch_messages(queue, demand, messages) do
    with d when d > 0 <- demand,
         {item, queue} = Qex.pop(queue),
         {:value, {from, message}} <- item
    do
      GenStage.reply(from, :ok)
      dispatch_messages(queue, demand - 1, [message | messages])
    else
      _ -> {:noreply, Enum.reverse(messages), {queue, demand, []}}
    end
  end
end
