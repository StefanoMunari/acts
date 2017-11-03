defmodule Incoming.Dispatcher do
  use GenStage

  alias Incoming.Utils.Acker, as: Acker

  def start_link do
    GenStage.start_link __MODULE__, :ok, name: __MODULE__
  end

  def init(:ok) do
    { :consumer, :ok, subscribe_to: [ Incoming.MessageHolder ] }
  end

  def handle_events(messages, _from, state) do
    # for each message
    for {message, meta} <- messages do
      topic =
        message.topic
        |> String.split(".")
        |> List.first
        |> String.downcase
        |> String.to_atom
      module = get_module topic
      # Dispatch message
      dispatch(message, module)
      # ack message
      :ok = Acker.ack meta
    end
    {:noreply, [], state}
  end

  defp get_module("application" <> _) do
    Elixir.Interlayer
  end
  defp get_module(module) do
    Application.get_env :incoming, module
  end

  defp dispatch(_, nil) do
    :ok
  end
  defp dispatch(message, module) do
    IO.inspect "Beep beep #{module}"
    module.handle_message message
  end
end
