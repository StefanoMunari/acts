import {Street} from "./street/street";
import {Intersection} from "./intersection/intersection";
import {Treadable} from "./treadable";
import {Infrastructure} from "./infrastructure";
import {FacilityStretch} from "./facility/stretch/facilityStretch";
import {Facility} from "./facility/facility";
import {Vehicle} from "../traveller/model/vehicle";

export class District {
    private streets: Map<number, Street> = new Map<number, Street>();
    private intersections: Map<number, Intersection> = new Map<number, Intersection>();
    private treadables: Map<number, Treadable> = new Map<number, Treadable>();
    private infrastructures: Map<number, Infrastructure> = new Map<number, Infrastructure>();
    private facilities: Map<number, Facility> = new Map<number, Facility>();
    private neighbours: Set<string> = new Set<string>();
    private minX: number = null;
    private minY: number = null;
    private maxX: number = null;
    private maxY: number = null;

    public addStreet(id: number, street: Street) {
        this.streets.set(id, street);
        this.infrastructures.set(id, street);
    }

    public addIntersection(id: number, intersection: Intersection) {
        this.intersections.set(id, intersection);
        this.treadables.set(id, intersection);
        this.infrastructures.set(id, intersection);
    }

    public containsStreet(id: number) {
        return this.streets.has(id);
    }

    public findStreet(id: number): Street {
        return this.streets.get(id);
    }

    public findIntersection(id: number): Intersection {
        return this.intersections.get(id);
    }

    public addInfrastructure(id: number, infrastructure: Infrastructure) {
        this.infrastructures.set(id, infrastructure);
    }

    public addTreadable(id: number, treadable : Treadable) {
        this.treadables.set(id, treadable);
    }

    public findAllStreets(): Array<Street> {
        return Array.from(this.streets.values());
    }

    public setGarageCapacity(capacity: number, facilityId: number) {
        this.findFacility(facilityId)
            .setGarageCapacity(capacity);
    }

    public hasFacility(facilityId : number): boolean {
        return this.facilities.has(facilityId);
    }

    public addFacility(id: number, stretch: FacilityStretch) {
        let facility: Facility = Facility.createByStretch(id, stretch);
        this.facilities.set(id, facility);
    }

    public setNeighbours(neighbours: Array<string>) {
        neighbours.forEach(neighbourId => this.neighbours.add(neighbourId));
    }

    public getNeighbours(): Set<string> {
        return this.neighbours;
    }

    public containsTreadable(id : number): boolean {
        return this.treadables.has(id);
    }

    public findTreadable(id: number): Treadable {
        return this.treadables.get(id);
    }

    public findAllIntersections(): Array<Intersection> {
        return Array.from(this.intersections.values());
    }

    public findFacility(id: number): Facility {
        return this.facilities.get(id);
    }

    public enterPersonIntoFacility(personId: string, facilityId: number) {
        this.findFacility(facilityId)
            .enterPerson(personId);
    }

    public exitPersonFromFacility(personId: string, facilityId: number) {
        this.findFacility(facilityId)
            .exitPerson(personId);
    }

    public parkVehicleIntoFacility(vehicle: Vehicle, passengers: Array<string>, facilityId: number) {
        this.findFacility(facilityId)
            .parkVehicle(vehicle, passengers);
    }

    public takeVehicleFromFacility(vehicleId: string, passengers: Array<string>, facilityId: number) {
        this.findFacility(facilityId)
            .takeVehicle(vehicleId, passengers);
    }

    public countPeopleAtFacility(facilityId: number): number {
        return this.findFacility(facilityId)
            .countPeople();
    }

    public countFreeParksOfFacility(facilityId: number): number {
        return this.findFacility(facilityId)
            .countFreeParks();
    }

    public getGarageCapacityOfFacility(facilityId: number): number {
        return this.findFacility(facilityId)
            .getGarageCapacity();
    }

    public findAllFacilities(): Array<Facility> {
        return Array.from(this.facilities.values());
    }

    public getMinX(): number {
        return this.minX;
    }

    public getMinY(): number {
        return this.minY;
    }

    public getWidthUpperBound(): number {
        return this.maxX - this.minX + 1;
    }

    public getHeightUpperBound(): number {
        return this.maxY - this.minY + 1;
    }

    public updateMinX(x: number) {
        if (this.minX === null) {
            this.minX = x;
        } else {
            this.minX = Math.min(this.minX, x);
        }
    }

    public updateMinY(y: number) {
        if (this.minY === null) {
            this.minY = y;
        } else {
            this.minY = Math.min(this.minY, y);
        }
    }

    public updateMaxX(x: number) {
        if (this.maxX === null) {
            this.maxX = x;
        } else {
            this.maxX = Math.max(this.maxX, x);
        }
    }

    public updateMaxY(y: number) {
        if (this.maxY === null) {
            this.maxY = y;
        } else {
            this.maxY = Math.max(this.maxY, y);
        }
    }
}