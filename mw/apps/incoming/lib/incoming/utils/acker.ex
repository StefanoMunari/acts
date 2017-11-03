defmodule Incoming.Utils.Acker do

  @doc """
  Sends an ack using Wabbit.
  """
  def ack(meta) do
    try do
      Wabbit.Basic.ack(meta.channel, meta.delivery_tag)
    catch
      _, _ ->
        :ok
    end
  end
end
