defmodule Incoming.MessageHolder do
  use GenStage

  alias Incoming.Utils.Acker, as: Acker

  @moduledoc """
  This module holds messages, preventing them from being dispatched to one or
  more middleware modules.
  """

  ### CLIENT API

  @doc """
  Starts the GenStage process resuming undelivered messages (if any).
  """
  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### SERVER CALLBACKS

  def init(:ok) do
    {:producer_consumer, :open, subscribe_to: [Incoming.Router] }
  end

  def handle_events(meta_messages, _from, holding) do
    forward = hold meta_messages, holding, []
    {:noreply, forward, holding}
  end

  defp hold([], _, queue) do
    queue
  end

  defp hold([meta_message | list], :open, forward) do
    {message, meta} = meta_message
    if pause_message?(message) do
      # start storing messages
      Persistence.ForwardMessageDb.store_msg message
      # ack message
      :ok = Acker.ack meta
      hold list, :closed, [meta_message | forward]
    else
      # message will be acked later
      hold list, :open,   [meta_message | forward]
    end
  end

  defp hold([meta_message | list], :closed, forward) do
    {message, meta} = meta_message
    Persistence.ForwardMessageDb.store_msg message
    :ok = Acker.ack meta
    if resume?(message) do
      held_messages = Persistence.ForwardMessageDb.get_all_messages
      # We should dump messages before flushing 'em...
      Persistence.ForwardMessageDb.flush
      hold list, :open,   forward ++ held_messages
    else
      hold list, :closed, forward
    end
  end

  defp pause_message?(message) do
    payload = String.split message.payload, "."
    command = List.first payload
    topic   = String.split message.topic
    if topic != "snapshot" or command != "take" do
      false
    else
      starter   = Enum.at payload, 1
      version   = Enum.at payload, 2
      requestor = Enum.at payload, 3
      Snapshot.algorithm().take {starter, version, requestor}
      true
    end
  end

  defp resume?(message) do
    payload = String.split message.payload, "."
    command = List.first payload
    topic   = String.split message.topic
    if topic != "snapshot" or command != "resume" do
      false
    else
      starter   = Enum.at payload, 1
      version   = Enum.at payload, 2
      submitter = Enum.at payload, 3
      [ ] == Snapshot.algorithm().submit {starter, version, submitter}
    end
  end
end
