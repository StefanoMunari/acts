defmodule Interface.DistrictChannel do
  use Phoenix.Channel

  alias Interface.OutputStream.CityManager, as: OutputToDistrict

  def join("district:" <> district_id, _message, socket) do
    Interface.Endpoint.subscribe "district:#{district_id}"
    IO.inspect "Subscribing #{inspect socket} to district:#{district_id}..."
    OutputToDistrict.stream district_id
    socket
    |> assign(:district, district_id)
    |> (fn socket -> { :ok, socket } end).()
  end

  def handle_in("watch", %{ "traveller" => traveller_id }, socket) do
    case socket.assigns.chasing do
      ^traveller_id ->
        { :noreply, socket }
      nil ->
        Interface.Endpoint.unsubscribe "traveller:#{traveller_id}"
        socket
        |> assign(:chasing, traveller_id)
        |> (fn socket -> { :noreply, socket} end).( )
      old_id ->
        Interface.Endpoint.unsubscribe "traveller:#{old_id}"
        Interface.Endpoint.subscribe "traveller:#{traveller_id}"
        socket
        |> assign(:chasing, traveller_id)
        |> (fn socket -> { :noreply, socket} end).( )
    end
  end

  def handle_in("unwatch", %{ "traveller" => traveller_id }, socket) do
    case socket.assigns.chasing do
      ^traveller_id ->
        Interface.Endpoint.unsubscribe "traveller:#{traveller_id}"
        socket
        |> assign(:chasing, nil)
        |> (fn socket -> { :noreply, socket } end).( )
      _ ->
        { :noreply, socket }
    end
  end

  def handle_in(_, _, socket) do
    { :noreply, socket }
  end

  def handle_out("tread", %{"body" => body}, socket) do
    IO.inspect "Tread: #{inspect body}"
    push socket, "traveller_motion", body
    { :noreply, socket }
  end

  def handle_out("exit_district", %{"body" => body}, socket) do
    IO.inspect "Exit district: #{inspect body}"
    push socket, "exit_district", body
    { :noreply, socket }
  end

  def handle_out("enter_building", %{"body" => body}, socket) do
    IO.inspect "Enter building: #{inspect body}"
    push socket, "enter_facility", body
    { :noreply, socket }
  end

  def handle_out("exit_building", %{"body" => body}, socket) do
    IO.inspect "Exit building: #{inspect body}"
    push socket, "exit_facility", body
    { :noreply, socket }
  end

  def handle_out("change_color_tl", %{"body" => body}, socket) do
    IO.inspect "Changing traffic light color: #{inspect body}"
    push socket, "change_color_tl", body
    { :noreply, socket }
  end

  def handle_out("enter_bus", %{"body" => body}, socket) do
    IO.inspect "Enter bus: #{inspect body}"
    push socket, "enter_bus", body
    { :noreply, socket }
  end

  def handle_out("exit_bus", %{"body" => body}, socket) do
    IO.inspect "Exit bus: #{inspect body}"
    push socket, "exit_bus", body
    { :noreply, socket }
  end

  def handle_out("shutdown", %{"body" => body}, socket) do
    IO.inspect "Shutdown: #{inspect body}"
    push socket, "shutdown", body
    { :noreply, socket }
  end

  def handle_out(_, _, socket) do
    IO.puts "not forwarding"
    # Callback not defined for this type of message
    { :noreply, socket }
  end


  def handle_info(_body, socket) do
    { :noreply, socket }
  end
end
