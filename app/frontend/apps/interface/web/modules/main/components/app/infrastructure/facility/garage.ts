import {TravellerType} from "../../traveller/travellerType";
import {Vehicle} from "../../traveller/model/vehicle";

export class Garage {
    private parkedVehicles: Map<string, TravellerType> = new Map<string, TravellerType>();
    private totalCapacity: number;

    private constructor() {}

    public static createWithCapacity(capacity: number): Garage {
        let garage: Garage = new Garage();
        garage.totalCapacity = capacity;
        return garage;
    }

    public calculateFreeCapacity(): number {
        let parkedVehicles = this.countParkedVehicles();
        return this.totalCapacity - parkedVehicles;
    }

    public countParkedVehicles(): number {
        return this.parkedVehicles.size;
    }

    public countParkedBicycles(): number {
        return Array.from(this.parkedVehicles.values())
            .reduce((counter, vehicleType) => {
                if (vehicleType === TravellerType.BICYCLE) {
                    return counter + 1;
                } else {
                    return counter;
                }
            }, 0)
    }

    public enterVehicle(vehicle: Vehicle) {
        this.parkedVehicles.set(vehicle.getId(), vehicle.getType());
    }

    public exitVehicle(vehicleId: string) {
        this.parkedVehicles.delete(vehicleId);
    }

    public getTotalCapacity(): number {
        return this.totalCapacity;
    }
}