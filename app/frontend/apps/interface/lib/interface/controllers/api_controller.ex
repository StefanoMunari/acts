defmodule Interface.ApiController do
  use Interface.Web, :controller

  import Ecto.Query
  alias Domain.User
  alias Domain.Repo
  alias Comeonin.Bcrypt

  def create_token(conn, params) do
    email = params["email"]
    password = params["password"]
    select = from u in User, where: u.email == ^email
    user = Repo.one(select)
    if user == nil do
      send_unauthorized conn
    else
      hash = user.password_hash
      if Bcrypt.checkpw(password, hash) do
        { :ok, jwt, _full_claims } = Guardian.encode_and_sign(user, :token)
        json conn, %{ token: jwt }
      else
        send_unauthorized conn
      end
    end
  end

  def send_broadcast(conn, params) do
    message = params["message"]
    Interface.Endpoint.broadcast "rooms:lobby", "new_message", %{user: "CLI", body: message}
    json conn, %{ result: "ok" }
  end

  defp send_unauthorized(conn) do
    conn
    |> put_status(401)
    |> json(%{ error: "Unauthorized" })
  end

end
