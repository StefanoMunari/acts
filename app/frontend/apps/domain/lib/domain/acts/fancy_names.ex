defmodule Domain.Acts.FancyNames do

  def name_for_city(id) do
    case id do
      "0" ->
        "Magrathea"
      "1" ->
        "Dagobah"
      "2" ->
        "Naboo"
      "3" ->
        "Utapau"
      "4" ->
        "Tatooine"
      _ ->
        "City #{id}"
    end
  end

  def name_for_district(id) do
    [ city_id, district_id ] = String.split id, "_"
    "#{name_for_city(city_id)} > District #{district_id}"
  end
end
