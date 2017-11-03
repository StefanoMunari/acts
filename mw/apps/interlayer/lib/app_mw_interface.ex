defmodule Interlayer.AppMwInterface do
  use GenServer

  alias Utils.Message, as: Message
  alias Utils.AppMessageOperations, as: AppMessageOps
  alias Utils.Routing.Info, as: RoutingInfo

  @moduledoc """
  This module waits for requests from application.

  Incoming requests are then passed to the dispatcher.
  """

  @doc """
  Starts listening on the port `listen_port`.
  """
  @spec start :: any
  def start do
    srv = Socket.TCP.listen! listen_port()
    _loop_acceptor srv
  end

  @spec _loop_acceptor(any) :: any
  defp _loop_acceptor(srv) do
    { _, client } = srv |> Socket.TCP.accept
    Task.start_link(fn -> _serve client end)
    _loop_acceptor srv
  end

  @spec _serve(any) :: any
  defp _serve(client) do
    message =
      try do
        raw_msg = read_message client, "", :first
        raw_msg
      rescue
        e in Socket.Error ->
          IO.inspect "Network error in reception: #{inspect e}"
          client |> Socket.Stream.close
          nil
      end
    message
      |> AppMessageOps.decode
      |> find_recipient
      |> AppMessageOps.create_mw_message_from_app_message
      |> (fn args -> IO.inspect args; args end).()
      |> forwarder().handle_message
  end

  def read_message(client, raw_data, :first) do
    raw_data_received = client |> Socket.Stream.recv!
    sanitized_raw_data = raw_data_received || ""
    new_raw_data      = raw_data <> sanitized_raw_data
    formatted_data    = formatter().parse new_raw_data
    { outcome, _ }    = JSX.decode new_raw_data
    if formatted_data == "" do
      read_message client, formatted_data, :first
    else
      read_message client, formatted_data, outcome
    end
  end
  def read_message(_, complete_message, :ok) do
    complete_message
  end
  def read_message(client, raw_data, _) do
    { e, raw_data_received } = client |> Socket.Stream.recv
    if e == :error do
      IO.inspect "Error while reading: #{inspect raw_data}"
      raise Socket.Error, reason: raw_data_received
    end
    sanitized_raw_data = raw_data_received || ""
    new_raw_data = raw_data <> sanitized_raw_data
    formatted_data    = formatter().parse new_raw_data
    { outcome, _ } = JSX.decode formatted_data
    read_message client, formatted_data, outcome
  end


  # Returns an enriched AppMessage, which embodies the recipient (if any)
  defp find_recipient(nil) do
    nil
  end
  defp find_recipient(msg) do
    if Message.get_type(msg) == "ACK" do
      find_recipient_reply msg
    else
      find_recipient_gen msg
    end
  end
  defp find_recipient_reply(msg) do
    { present, recipient, _request } =
      msg
      |> Message.get_request_id
      |> Persistence.PendingRequestDb.get_and_delete
      |> (fn args -> IO.inspect args; args end).()
    if present == :present do
      %{ msg | recipient: recipient }
    else
      %{ msg | recipient: RoutingInfo.whoami }
    end
  end
  defp find_recipient_gen(msg) do
    new_msg = AppMessageOps.get_recipient_from_message msg
    { entity_id, entity_type } = new_msg.recipient
    { outcome, recipient } = resolver().solve entity_id, entity_type
    if outcome == :ok do
      %{ msg | recipient: recipient }
    else
      %{ msg | recipient: RoutingInfo.whoami }
    end
  end



  defp formatter do
    Application.get_env(:interlayer, :formatter)
  end

  defp resolver do
    Application.get_env(:interlayer, :resolver)
  end

  defp forwarder do
    Application.get_env(:interlayer, :forwarder)
  end

  defp listen_port do
    Application.get_env(:interlayer, :listen_port)
  end
end
