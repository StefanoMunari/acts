defmodule Interface.RoomChannel do
  use Phoenix.Channel
  alias Domain.User
  alias Domain.Repo
  alias Interface.Data

  def join("rooms:lobby", _message, socket) do
    IO.puts "User##{socket.assigns.user_id} joined rooms:lobby"
    { :ok, socket }
  end

  def join("rooms:" <> _private_room_id, _params, _socket) do
    { :error, %{ reason: "unauthorized" }}
  end

  def handle_in("graphql", %{"body" => %{"query" => query }}, socket) do
    user = Repo.get(User, socket.assigns.user_id)
    { :ok, result } = Data.execute(query, user)
    { :reply, { :ok, %{ body: result }}, socket }
  end

  def handle_out(name, payload, socket) do
    push socket, name, payload
    {:noreply, socket}
  end
end
