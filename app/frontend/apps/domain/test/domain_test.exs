defmodule DomainTest do
  use ExUnit.Case
   alias Domain.Repo
   alias Domain.User

  doctest Domain

  test "insert a user in the db" do
    user = %User{
      name: "Testy",
      email: "testy@phoenix.com",
      password_hash: "$2b$12$hnuEjNt8FpFOK92oBlOwJ.gzG2JxaXX3Ae.MZeCj6GDpURmoFbmf6"
    }
    Repo.insert! user
  end

end
