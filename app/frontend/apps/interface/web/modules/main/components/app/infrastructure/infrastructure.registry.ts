import {Injectable} from '@angular/core';
import {Intersection} from './intersection/intersection';
import {Street} from "./street/street";
import {Treadable} from "./treadable";
import {District} from './district';
import {Facility} from "./facility/facility";
import {Vehicle} from "../traveller/model/vehicle";

@Injectable()
export class InfrastructureRegistry {
    private currentCityId: string = null;
    private currentDistrictId: string = null;
    private districts: Map<string, District> = new Map<string, District>();

    public containsTreadable(id : number): boolean {
        return this.getCurrentDistrict()
            .containsTreadable(id);
    }

    public findTreadable(id: number): Treadable {
        return this.getCurrentDistrict()
            .findTreadable(id);
    }

    public findAllStreets(): Array<Street> {
        return this.getCurrentDistrict()
            .findAllStreets();
    }

    public findAllIntersections(): Array<Intersection> {
        return this.getCurrentDistrict()
            .findAllIntersections();
    }

    public findFacility(id: number): Facility {
        return this.getCurrentDistrict()
            .findFacility(id);
    }

    public enterPersonIntoFacility(personId: string, facilityId: number) {
        return this.getCurrentDistrict()
            .enterPersonIntoFacility(personId, facilityId);
    }

    public exitPersonFromFacility(personId: string, facilityId: number) {
        return this.getCurrentDistrict()
            .exitPersonFromFacility(personId, facilityId);
    }

    public parkVehicleIntoFacility(vehicle: Vehicle, passengers: Array<string>, facilityId: number) {
        return this.getCurrentDistrict()
            .parkVehicleIntoFacility(vehicle, passengers, facilityId);
    }

    public takeVehicleFromFacility(vehicleId: string, passengers: Array<string>, facilityId: number) {
        return this.getCurrentDistrict()
            .takeVehicleFromFacility(vehicleId, passengers, facilityId);
    }

    public countPeopleAtFacility(facilityId: number): number {
        return this.getCurrentDistrict()
            .countPeopleAtFacility(facilityId);
    }

    public countFreeParksOfFacility(facilityId: number): number {
        return this.getCurrentDistrict()
            .countFreeParksOfFacility(facilityId);
    }

    public getGarageCapacityOfFacility(facilityId: number): number {
        return this.getCurrentDistrict()
            .getGarageCapacityOfFacility(facilityId);
    }

    public findAllFacilities(): Array<Facility> {
        return this.getCurrentDistrict()
            .findAllFacilities();
    }

    public hasDistrict(id: string): boolean {
        return this.districts.has(id);
    }

    public getDistrict(id: string): District {
        return this.districts.get(id);
    }

    public addDistrict(id: string, district: District) {
        if (!this.hasDistrict(id)) {
            this.districts.set(id, district);
        }
    }

    public setCurrentDistrict(districtId: string) {
        this.currentDistrictId = districtId;
    }

    public clear() {
        let neighbours: Set<String> = this.getNeighbours();
        let districtsToRemove = Array.from(this.districts.keys())
            .filter(districtId => districtId !== this.currentDistrictId)
            .filter(districtId => !neighbours.has(districtId));
        districtsToRemove.forEach(districtId => this.districts.delete(districtId));
    }

    public getCurrentDistrict(): District {
        return this.districts.get(this.currentDistrictId);
    }

    public getCurrentDistrictId(): string {
        return this.currentDistrictId;
    }

    public getNeighbours(): Set<string> {
        return this.getCurrentDistrict()
            .getNeighbours();
    }

    public getMinX(): number {
        return this.getCurrentDistrict()
            .getMinX();
    }

    public getMinY(): number {
        return this.getCurrentDistrict()
            .getMinY();
    }

    public getWidthUpperBound(): number {
        return this.getCurrentDistrict()
            .getWidthUpperBound();
    }

    public getHeightUpperBound(): number {
        return this.getCurrentDistrict()
            .getHeightUpperBound();
    }

    public getCurrentCityId(): string {
        return this.currentCityId;
    }

    public setCurrentCityId(cityId: string) {
        this.currentCityId = cityId;
    }
}