import {Garage} from "./garage";
import {FacilityStretch} from "./stretch/facilityStretch";
import {Vehicle} from "../../traveller/model/vehicle";

export class Facility {
    private id: number;
    private stretch: FacilityStretch;
    private people: Set<string> = new Set<string>();
    private garage: Garage;

    private constructor() {}

    public static createByStretch(id : number, stretch : FacilityStretch): Facility {
        let facility : Facility = new Facility();
        facility.id = id;
        facility.stretch = stretch;
        return facility;
    }

    public getId() : number {
        return this.id;
    }

    public countPeople(): number {
        return this.people.size;
    }

    public countFreeParks(): number {

        if (!this.garage) {
            return 0;
        }

        return this.garage.calculateFreeCapacity();
    }

    public countParkedBicycles(): number {

        if (!this.garage) {
            return 0;
        }

        return this.garage.countParkedBicycles();
    }

    public countParkedVehicles(): number {

        if (!this.garage) {
            return 0;
        }

        return this.garage.countParkedVehicles();
    }

    public getGarageCapacity(): number {

        if (!this.garage) {
            return 0;
        }

        return this.garage.getTotalCapacity();
    }

    public getStretch() : FacilityStretch {
        return this.stretch;
    }

    public enterPerson(personId: string) {
        this.people.add(personId);
    }

    public exitPerson(personId: string) {
        this.people.delete(personId);
    }

    public parkVehicle(vehicle: Vehicle, passengers: Array<string>) {
        for (let passenger of passengers) {
            this.people.add(passenger);
        }

        if (!this.garage) {
            throw("A garage for facility with id = " + this.id + " does not exist");
        }

        this.garage.enterVehicle(vehicle);
    }

    public takeVehicle(vehicleId: string, passengers: Array<string>) {
        for (let passenger of passengers) {
            this.people.delete(passenger);
        }

        if (!this.garage) {
            throw("A garage for facility with id = " + this.id + " does not exist");
        }

        this.garage.exitVehicle(vehicleId);
    }

    public setGarageCapacity(capacity: number) {
        this.garage = Garage.createWithCapacity(capacity);
    }
}