defmodule Interface.TravellerChannelOld do
  use Phoenix.Channel

  def join("travellers:motion", _message, socket) do
    IO.puts "join travellers:motion"
    :timer.send_interval(1_000, :pedestrian1)
    :timer.send_interval(200, :vehicle1)
    :timer.send_interval(200, :vehicle2)
    {:ok, socket}
  end

  def join("travellers:" <> _private_room_id, _params, _socket) do
    IO.puts "unauthorized travellers join trial"
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("traveller_motion", %{"body" => body}, socket) do
    IO.puts "handle_in traveller_motion"
    broadcast! socket, "traveller_motion", %{body: body}
    {:noreply, socket}
  end

  def handle_in("exit_district", %{"body" => body}, socket) do
    IO.puts "handle_in exit_district"
    broadcast! socket, "exit_district", %{body: body}
    {:noreply, socket}
  end

  def handle_in("enter_facility", %{"body" => body}, socket) do
    IO.puts "handle_in enter_facility"
    broadcast! socket, "enter_facility", %{body: body}
    {:noreply, socket}
  end

  def handle_in("exit_district", %{"body" => body}, socket) do
    IO.puts "handle_in exit_facility"
    broadcast! socket, "exit_facility", %{body: body}
    {:noreply, socket}
  end

  def handle_out(name, payload, socket) do
    IO.puts "handle_out"
    IO.inspect payload
    push socket, name, payload
    {:noreply, socket}
  end

  def isEmpty([]) do true end
  def isEmpty(_) do false end
  def isNil(nil) do true end
  def isNil(_) do false end
  def isNilOrEmpty(nil) do true end
  def isNilOrEmpty([]) do true end
  def isNilOrEmpty(_) do false end

  def handle_info(:vehicle1, socket) do
    status = socket.assigns[:statusv1]
    IO.puts "STATUS: #{status}"
    if isNilOrEmpty(socket.assigns[:positionsv1]) do
      if status == "BEFORE EXITING FACILITY" do
        positions = [
          %{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
            "infrastructureId": 518
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
            "infrastructureId": 519
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
            "infrastructureId": 520
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
            "infrastructureId": 521
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
            "infrastructureId": 522
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
            "passengers": [2, 10, 11],
             "infrastructureId": 523
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 524
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 525
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 526
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 336
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 337
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 338
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 339
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 340
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 341
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 342
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 343
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 344
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 345
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 346
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 347
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 348
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 349
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 10, 11],
             "infrastructureId": 350
          }
        ];
      else
        positions = [
          %{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
            "infrastructureId": 512
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
            "infrastructureId": 513
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
            "infrastructureId": 514
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
            "infrastructureId": 515
          },%{
            "travellerId": 1,
            "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
            "infrastructureId": 516
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
            "passengers": [2, 3, 4],
             "infrastructureId": 517
          },%{
             "travellerId": 1,
             "travellerType": "VEHICLE",
             "passengers": [2, 3, 4],
             "infrastructureId": 518
          }
        ];
      end
      if isEmpty(socket.assigns[:positionsv1]) do
        if status == "BEFORE ENTERING FACILITY" do
          name = "enter_facility"
          payload = %{"travellerId": 1,
                      "facilityId": 52,
                      "carrier": "true",
                      "passengers": [2, 3, 4]}
          socket = assign(socket, :statusv1, "BEFORE EXITING FACILITY")
          socket = assign(socket, :positionsv1, [])
        else
          if status == "BEFORE EXITING FACILITY" do
            name = "exit_facility"
            payload = %{"travellerId": 1,
                        "facilityId": 52,
                        "carrier": "true",
                        "passengers": [2, 10, 11]}
            socket = assign(socket, :statusv1, "AFTER EXITING FACILITY")
            socket = assign(socket, :positionsv1, positions)
          else
            IO.puts "EXIT DISTRICT"
            name = "exit_district"
            payload = %{"travellerId": 1,
                        "districtId": 9,
                        "passengers": [2, 10, 11]}
            socket = assign(socket, :statusv1, "BEFORE ENTERING FACILITY")
            socket = assign(socket, :positionsv1, positions)
          end
        end
      else
        if isNil(status) do
          status = "BEFORE ENTERING FACILITY"
        end
        name = "traveller_motion"
        payload = hd(positions)
        socket = assign(socket, :positionsv1, tl(positions))
        socket = assign(socket, :statusv1, status)
      end
    else
      positions = socket.assigns[:positionsv1]
      name = "traveller_motion"
      payload = hd(positions)
      socket = assign(socket, :positionsv1, tl(positions))
      socket = assign(socket, :statusv1, status)
    end
    push socket, name, payload
    {:noreply, socket}
  end

  def handle_info(:vehicle2, socket) do
    if isNilOrEmpty(socket.assigns[:positionsv2]) do
      positions = [
        %{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 295
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 296
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 297
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 298
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 299
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 300
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 65
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 66
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 67
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 68
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 69
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 70
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 26
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 27
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 28
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 29
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 30
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 31
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 32
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 33
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 34
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 35
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 36
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 260
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 261
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 262
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 263
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 264
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 265
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 282
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 283
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 284
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 285
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 529
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 530
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 531
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 622
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 623
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 624
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 625
        },%{
          "travellerId": 2,
          "travellerType": "VEHICLE",
          "infrastructureId": 626
        }               
      ];
      if isEmpty(socket.assigns[:positionsv2]) do
        name = "exit_district"
        payload = %{"travellerId": 2}
        socket = assign(socket, :positionsv2, positions)
      else
        name = "traveller_motion"
        payload = hd(positions)
        socket = assign(socket, :positionsv2, tl(positions))
      end
    else
      positions = socket.assigns[:positionsv2]
      name = "traveller_motion"
      payload = hd(positions)
      socket = assign(socket, :positionsv2, tl(positions))
    end
    push socket, name, payload
    {:noreply, socket}
  end

  def handle_info(:pedestrian1, socket) do
    if isNilOrEmpty(socket.assigns[:positionsp1]) do
      positions = [
        %{
          "travellerId": 3,
          "infrastructureId": 377,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 378,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 379,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 380,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 638,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 639,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 640,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 625,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 629,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 634,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 635,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 636,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 647,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 500,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 501,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 502,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 503,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 504,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 505,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 506,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 507,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 508,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 509,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 510,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 511,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 512,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 513,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 514,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 515,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 516,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 519,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 648,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 566,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 567,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 549,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 556,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 563,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 591,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 592,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 593,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 575,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 581,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 587,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 588,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 589,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 232,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 233,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 542,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 543,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 544,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 616,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 617,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 618,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 619,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 309,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 310,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 311,
          "travellerType": "PEDESTRIAN"
        },%{
          "travellerId": 3,
          "infrastructureId": 312,
          "travellerType": "PEDESTRIAN"
        }
      ];
      if isEmpty(socket.assigns[:positionsp1]) do
        name = "exit_district"
        payload = %{"travellerId": 3}
        socket = assign(socket, :positionsp1, positions)
      else
        name = "traveller_motion"
        payload = hd(positions)
        socket = assign(socket, :positionsp1, tl(positions))
      end
    else
      positions = socket.assigns[:positionsp1]
      name = "traveller_motion"
      payload = hd(positions)
      socket = assign(socket, :positionsp1, tl(positions))
    end
    broadcast socket, name, payload
    IO.inspect payload
    {:noreply, socket}
  end
end