defmodule BrokerToAs.Utils.DeepInspection.Vehicle do

  def extract_passengers(app_message)
  when is_map app_message
  do
    app_message
    |> Map.get("Header")
    |> Map.get("Type")
    |> _get_vehicle_passengers(app_message)
  end
  def extract_passengers(app_message), do: [ ]

  defp _get_vehicle_passengers("PEDESTRIAN", _app_message), do: [ ]
  defp _get_vehicle_passengers(_, app_message) do
    app_message
    |> Map.get("Payload")
    |> Map.get("Passenger")
    |> _inspect_passengers
  end

  defp _inspect_passengers(nil) do
    [ ]
  end
  defp _inspect_passengers([ ]) do
    [ ]
  end
  defp _inspect_passengers( [ first_passenger | passengers ] )
  do
    [
      first_passenger
      |> JSX.decode
      |> elem(1)
      |> Map.get("Id")
      |> String.trim("\"")
    |
      _inspect_passengers passengers
    ]
  end
end

# %Utils.Message{async: true, payload: "{\"Header\":{\"Call\":\"ASYNC\",\"Recipient\":\"16\",\"Recipient_Type\":\"HOST\",\"Request\":\"EXIT_BUILDING\",\"Request_Id\":\"22\",\"Type\":\"PRIVATE_MOTOR_VEHICLE\"},\"Payload\":{\"Current_Position\":\"16\",\"Current_Speed\":\"70\",\"Destination_BIKE\":\"846,853\",\"Destination_FOOT\":\"30,867\",\"Destination_ROAD\":\"832,839\",\"Id\":\"142\",\"Max_Passengers\":\"1\",\"Maximum_Speed\":\"70\",\"Passengers_Number\":\"1\",\"Source_BIKE\":\"190,197\",\"Source_FOOT\":\"176,183\",\"Source_ROAD\":\"204,211\",\"Vehicle_Type\":\"MOTORCYCLE\",\"Passenger\":[\"{\\\"Current_Position\\\":\\\"98\\\",\\\"Current_Speed\\\":\\\"0\\\",\\\"Destination_BIKE\\\":\\\"77,84\\\",\\\"Destination_FOOT\\\":\\\"91,98\\\",\\\"Destination_ROAD\\\":\\\"63,70\\\",\\\"Id\\\":\\\"22\\\",\\\"Maximum_Speed\\\":\\\"5\\\",\\\"Source_BIKE\\\":\\\"190,197\\\",\\\"Source_FOOT\\\":\\\"176,183\\\",\\\"Source_ROAD\\\":\\\"204,211\\\"}\"]}}", recipient: "0_1", sender: "0_1", topic: "application.exit_building.0_1.22"}

