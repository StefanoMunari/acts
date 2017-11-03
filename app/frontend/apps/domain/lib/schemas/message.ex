defmodule Domain.Acts.Message do
  use Ecto.Schema

  # import Ecto
  import Ecto.Changeset
  # import Ecto.Query

  alias Domain.Acts.Message, as: Message


  schema "messages" do
    field :district, :string
    field :payload,  :string
    field :event, :string
    field :topic, :string
    timestamps()
  end

  @required_fields ~w(district event topic)
  @optional_fields ~w(payload)

  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def add(message, district) do
    %Message{
      district: district,
      payload:  message.payload,
      event:    message.event,
      topic:    message.topic
    }
    |> Domain.Repo.insert!
  end
end
