import {Injectable} from '@angular/core';
import {Traveller} from "./model/traveller";
import {Observable} from "rxjs/Observable";
import {Subject} from "rxjs/Subject";
import {TravellerType} from "./travellerType";
import {Statistic} from "../stats/statistic";
import {Color} from "../shared/color";

@Injectable()
export class TravellerRegistry {
    public static readonly PEDESTRIAN_COLOR: Color = Color.BLUE;
    public static readonly BICYCLE_COLOR: Color = Color.ORANGE;
    public static readonly BUS_COLOR: Color = Color.YELLOW;
    public static readonly CAR_COLOR: Color = Color.BLUE_VIOLET;

    private travellers: Map<string, Traveller> = new Map<string, Traveller>();
    private people: Set<string> = new Set<string>();
    private vehicles: Set<string> = new Set<string>();
    private moveIn : Subject<Array<Traveller>> = new Subject();
    private moveOut : Subject<Array<string>> = new Subject();
    private pursuedTravellerId: string = null;

    public getPursuedTravellerId(): string {
        return this.pursuedTravellerId;
    }

    public setPursuedTravellerId(travellerId: string) {
        this.pursuedTravellerId = travellerId;
    }

    public addTraveller(id: string, traveller: Traveller) {
        if (!this.travellers.has(id)) {
            this.travellers.set(id, traveller);
            this.moveIn.next([traveller]);
        }
    }

    public addPerson(personId: string) {
        this.people.add(personId);
    }

    public addPeople(people: Array<string>) {
        for (let personId of people) {
            this.people.add(personId);
        }
    }

    public addVehicle(vehicleId: string) {
        this.vehicles.add(vehicleId);
    }

    public removeTraveller(id: string) {
        if (this.travellers.has(id)) {
            this.travellers.delete(id);
            this.moveOut.next([id]);
        }
    }

    public removePerson(personId: string) {
        this.people.delete(personId);
    }

    public removeVehicle(vehicleId: string) {
        this.vehicles.delete(vehicleId);
    }

    public findTraveller(id: string): Traveller {
        return this.travellers.get(id);
    }

    public findAllTravellers(): Array<Traveller> {
        return Array.from(this.travellers.values());
    }

    public getStats(): Array<Statistic> {
        let stats: Array<Statistic> = new Array<Statistic>();
        stats.push(new Statistic("people", this.countPeople()));
        stats.push(new Statistic("travellers", this.countTravellers()));
        stats.push(new Statistic("vehicles", this.countVehicles()));
        stats.push(new Statistic("bicycles", this.countBicycles()));
        stats.push(new Statistic("buses", this.countBuses()));
        stats.push(new Statistic("private motor vehicles", this.countPrivateMotorVehicles()));
        return stats;
    }

    public countTravellers(): number {
        if(!!this.travellers) {
            return this.travellers.size;
        }
        return 0;
    }

    public countPeople(): number {
        if(!!this.people) {
            return this.people.size;
        }
        return 0;
    }

    public countVehicles(): number {
        if(!!this.vehicles) {
            return this.vehicles.size;
        }
        return 0;
    }

    public countBicycles(): number {
        return this.countSpecificTraveller(TravellerType.BICYCLE);
    }

    public countBuses(): number {
        return this.countSpecificTraveller(TravellerType.BUS);
    }

    public countPrivateMotorVehicles(): number {
        return this.countSpecificTraveller(TravellerType.PRIVATE_MOTOR_VEHICLE);
    }

    private countSpecificTraveller(travellerType: TravellerType): number {
        if(!!this.travellers) {
            Array.from(this.travellers.values())
                .reduce((counter, traveller, _) => {
                    if (traveller.getType() === travellerType) {
                        return counter + 1;
                    } else {
                        return counter;
                    }
                }, 0);
        }
        return 0;
    }

    public containsTraveller(id: string): boolean {
        return this.travellers.has(id);
    }

    public onMoveIn() : Observable<Array<Traveller>> {
        return this.moveIn;
    }

    public onMoveOut() : Observable<Array<string>> {
        return this.moveOut;
    }

    public clear() {
        this.travellers.clear();
        this.people.clear();
        this.vehicles.clear();
    }

    public getTravellerColors(): Array<[string, Color]> {
        let travellerColors: Array<[string, Color]> = new Array();
        travellerColors.push(["pedestrian", TravellerRegistry.PEDESTRIAN_COLOR]);
        travellerColors.push(["bicycle", TravellerRegistry.BICYCLE_COLOR]);
        travellerColors.push(["car", TravellerRegistry.CAR_COLOR]);
        travellerColors.push(["bus", TravellerRegistry.BUS_COLOR]);
        return travellerColors;
    }
}