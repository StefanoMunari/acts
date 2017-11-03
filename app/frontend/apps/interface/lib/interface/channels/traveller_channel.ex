defmodule Interface.TravellerChannel do
  use Phoenix.Channel

  def join("traveller:" <> traveller_id, _message, socket) do
    Interface.Endpoint.subscribe "traveller:#{traveller_id}"
    IO.inspect "Subscribing #{inspect socket} to traveller:#{traveller_id}..."
    socket
    |> assign(:traveller, traveller_id)
    |> (fn socket -> {:ok, socket} end).()
  end

  def handle_in(_, _, socket) do
    {:noreply, socket}
  end

  def handle_out(_, _, socket) do
    IO.puts "not forwarding"
    # Callback not defined for this type of message
    {:noreply, socket}
  end

  def handle_info(_body, socket) do
    { :noreply, socket }
  end
end
