import {Injectable} from '@angular/core';
import {Observable} from 'rxjs/Observable';
import {InfrastructureRegistry} from "./infrastructure.registry";
import {DistrictModel} from "./viewModel/district.model";
import {Street} from "./street/street";
import {Orientation} from "../shared/orientation";
import {Intersection} from "./intersection/intersection";
import {CardinalDirection} from "../shared/cardinalDirection";
import {DirectionUtils} from "../shared/directionUtils";
import {RoadwayLane} from "./lane/roadwayLane";
import {Stretch} from "./stretch/stretch";
import {ApiService} from "../../../services/api.service";
import {WayModel} from "./viewModel/way.model";
import {IntersectionExitModel} from "./viewModel/intersectionExit.model";
import {Bikeway} from "./way/bikeway";
import {Roadway} from "./way/roadway";
import {Footway} from "./way/footway";
import {StreetBuilder} from "./street/streetBuilder";
import {StreetModel} from "./viewModel/street.model";
import {StretchModel} from "./viewModel/stretch.model";
import {StraightDirection} from "../shared/straightDirection";
import {FacilityModel} from "./facility/viewModel/facility.model";
import {Subject} from "rxjs/Subject";
import {FacilityStretch} from "./facility/stretch/facilityStretch";
import {District} from "./district";
import {BikewayLane} from './lane/bikewayLane';
import {FootwayLane} from "./lane/footwayLane";

@Injectable()
export class InfrastructureLoader {

    private infrastructuresLoaded: Subject<any> = new Subject();

    constructor(private infrastructureRegistry : InfrastructureRegistry,
                private apiService : ApiService) {
    }

    public loadDistrict(cityId : string, districtId : string, callback?: () => void) {
        if (!this.infrastructureRegistry.hasDistrict(districtId)) {
            this.getInfrastructuresRest(cityId, districtId).map(
                districtData => {
                    let district: District = this.buildDistrict(districtData);
                    this.infrastructureRegistry.addDistrict(districtId, district);
                    this.infrastructureRegistry.setCurrentDistrict(districtId);
                }
            ).subscribe(_ => {
                this.infrastructuresLoaded.complete();
                this.infrastructureRegistry.clear();
                let neighbours: Set<string> = this.infrastructureRegistry.getNeighbours();
                this.loadNeighbors(cityId, Array.from(neighbours));
                if (!!callback) {
                    callback();
                }
            });
        } else {
            this.infrastructureRegistry.setCurrentDistrict(districtId);
            this.infrastructuresLoaded.complete();
            this.infrastructureRegistry.clear();
            let neighbours: Set<string> = this.infrastructureRegistry.getNeighbours();
            this.loadNeighbors(cityId, Array.from(neighbours));
            if (!!callback) {
                callback();
            }
        }
    }

    public loadDistrictById(districtId: string, callback: () => void) {
        let cityId: string = this.infrastructureRegistry.getCurrentCityId();
        this.loadDistrict(cityId, districtId, callback);
    }

    private loadNeighbors(cityId: String, neighborIds: Array<string>) {
        if (neighborIds.length > 0) {
            let neighbourId: string = neighborIds.pop();
            if (!this.infrastructureRegistry.hasDistrict(neighbourId)) {
                this.getInfrastructuresRest(cityId, neighbourId).map(
                    districtData => {
                        let district: District = this.buildDistrict(districtData);
                        this.infrastructureRegistry.addDistrict(neighbourId, district);
                        this.loadNeighbors(cityId, neighborIds);
                    }
                ).subscribe();
            } else {
                this.loadNeighbors(cityId, neighborIds);
            }
        }
    }

    public observeInfrastructure() : Observable<any> {
        return this.infrastructuresLoaded;
    }

    private getInfrastructuresRest(cityId : String, districtId : String): Observable<any> {
        return this.apiService.getJson('/acts/api/cities/' + cityId + "/districts/" + districtId);
    }

    private buildDistrict(districtData : DistrictModel) {
        let district: District = new District();

        for (let s of districtData.streets) {
            let streetBuilder: StreetBuilder = new StreetBuilder(s.id);

            let roadways: WayModel[] = s.roadways;
            this.deserializeRoadway(s.id, streetBuilder, roadways, district);

            let bikeways: WayModel[] = s.bikeways;
            this.deserializeBikeways(s.id, streetBuilder, bikeways, district);

            let footways: WayModel[] = s.footways;
            this.deserializeFootways(s.id, streetBuilder, footways, district);

            streetBuilder.withOrientation(Orientation[s.orientation]);

            this.loadFacilityIds(s, streetBuilder, district);

            let street: Street = streetBuilder.build();
            district.addStreet(street.getId(), street);
        }
        for (let i of districtData.intersections) {
            let intersection: Intersection = new Intersection(i.id);
            district.addIntersection(i.id, intersection);

            let intersectionExits: IntersectionExitModel[] = i.exits;
            if (!intersectionExits) {
                throw new Error("No one exit is been defined for intersection with id = " + intersection.getId());
            }

            if (intersectionExits.length < 2) {
                throw new Error("The intersection with id = " + intersection.getId() + " should have at least 2 exits");
            }

            if (intersectionExits.length > 4) {
                throw new Error("The intersection with id = " + intersection.getId() + " should have at most 4 exits");
            }

            for (let exit of intersectionExits) {

                if (exit.streetId !== undefined && exit.streetId !== null) {
                    if (!exit.direction) {
                        throw new Error("All exits of intersection with id = " + intersection.getId() + " should have a direction");
                    }

                    if (district.containsStreet(exit.streetId)) {
                        let street: Street = district.findStreet(exit.streetId);
                        district.findIntersection(i.id)
                            .connect(street, CardinalDirection[exit.direction]);
                        street
                            .connect(intersection, DirectionUtils.reverseCardinalDirection(CardinalDirection[exit.direction]));
                    } else {
                        district.findIntersection(i.id)
                            .connect(null, CardinalDirection[exit.direction]);
                    }

                    if (exit.trafficLightId !== undefined && exit.trafficLightId !== null) {
                        intersection.addTrafficLight(exit.trafficLightId, CardinalDirection[exit.direction]);
                    }
                }
            }
        }
        this.loadFacilities(districtData.facilities, district);

        district.setNeighbours(districtData.neighbors);

        this.calculateCoordinates(district);

        return district;
    }

    private loadFacilities(facilities: Array<FacilityModel>, district: District) {
        for (let facility of facilities) {
            district.setGarageCapacity(facility.garage.capacity, facility.id);
        }
    }

    private loadFacilityIds(street: StreetModel, streetBuilder: StreetBuilder, district: District) {
        let roadway: WayModel = street.roadways[0];
        for (let lane of roadway.lanes) {
            let stretches: Array<StretchModel> = lane.stretches;
            for (let stretchIndex = 0; stretchIndex < stretches.length; ++stretchIndex){
                let stretch: StretchModel = stretches[stretchIndex];
                let facilityId: number = stretch.decorations.facilityId;
                if (facilityId !== undefined && facilityId !== null) {
                    if (!district.hasFacility(facilityId)) {
                        let facilityDirection: StraightDirection = StraightDirection[lane.direction];
                        let facility: FacilityStretch = new FacilityStretch();
                        facility.setId(facilityId);
                        facility.setDirection(facilityDirection);
                        streetBuilder.withFacility(facility, stretchIndex, facilityDirection);
                        district.addFacility(facilityId, facility);
                        district.addInfrastructure(facilityId, facility);
                    }
                }
            }
        }
    }

    private deserializeRoadway(streetId: number, streetBuilder: StreetBuilder, roadways: WayModel[], district: District) {
        if (!roadways || roadways.length === 0) {
            throw new Error("No one roadway is been defined for street with id = " + streetId);
        }
        let rw: WayModel = roadways[0];
        for (let l of rw.lanes) {
            let roadwayLane: RoadwayLane = RoadwayLane.createFromModel(l);
            this.addTreadablesIntoRegisty(roadwayLane.getStretches(), district);
            streetBuilder.withRoadwayOrdinal(rw.ordinal);
        }
        let roadway: Roadway = Roadway.createFromModel(rw);
        for (let l of rw.lanes) {
            let roadwayLane: RoadwayLane = RoadwayLane.createFromModel(l);
            this.addTreadablesIntoRegisty(roadwayLane.getStretches(), district);
            roadway.addLane(roadwayLane);
        }
        streetBuilder.addWay(rw.ordinal, roadway);
    }

    private deserializeBikeways(streetId: number, streetBuilder: StreetBuilder, bikeways: WayModel[], district: District) {
        if (!bikeways || bikeways.length === 0) {
            throw new Error("No one bikeway is been defined for street with id = " + streetId);
        }
        for (let bw of bikeways) {
            let bikeway: Bikeway = Bikeway.createFromModel(bw);
            for (let l of bw.lanes) {
                let bikewayLane: BikewayLane = BikewayLane.createFromModel(l);
                this.addTreadablesIntoRegisty(bikewayLane.getStretches(), district);
                bikeway.addLane(bikewayLane);
            }
            streetBuilder.addWay(bw.ordinal, bikeway);
        }
    }

    private deserializeFootways(streetId: number, streetBuilder: StreetBuilder, footways: WayModel[], district: District) {
        if (!footways || footways.length === 0) {
            throw new Error("No one footway is been defined for street with id = " + streetId);
        }
        for (let fw of footways) {
            let footway: Footway = Footway.createFromModel(fw);
            for (let l of fw.lanes) {
                let footwayLane: FootwayLane = FootwayLane.createFromModel(l);
                this.addTreadablesIntoRegisty(footwayLane.getStretches(), district);
                footway.addLane(footwayLane);
            }
            streetBuilder.addWay(fw.ordinal, footway);
        }
    }

    public addTreadablesIntoRegisty(stretches : Array<Stretch>, district: District) {
        for (let stretch of stretches) {
            district.addTreadable(stretch.getId(), stretch);
        }
    }

    private calculateCoordinates(district: District) {
        let streets: Array<Street> = district.findAllStreets();

        if (streets.length > 0) {
            let firstStreet: Street = streets[0];
            firstStreet.setRoadwayX(0);
            firstStreet.setRoadwayY(0);

            district.updateMinX(firstStreet.getX());
            district.updateMinY(firstStreet.getY());
            if (firstStreet.getOrientation() == Orientation.HORIZONTAL) {
                district.updateMaxX(firstStreet.getX() + firstStreet.getLength());
                district.updateMaxY(firstStreet.getY() + firstStreet.getWidth());
            } else {
                district.updateMaxX(firstStreet.getX() + firstStreet.getWidth());
                district.updateMaxY(firstStreet.getY() + firstStreet.getLength());
            }

            let intersections: Map<CardinalDirection, Intersection> = firstStreet.getIntersections();
            for (let direction of Array.from(intersections.keys())) {
                let intersection: Intersection = intersections.get(direction);
                this.calculateCoordinatesFromIntersection(intersection, firstStreet, direction, district);
            }
        }
    }

    private calculateCoordinatesFromIntersection(intersection: Intersection,
                                                 sourceStreet: Street,
                                                 entranceDirection: CardinalDirection,
                                                 district: District) {
        if (!intersection.getX() || !intersection.getY()) {
            switch(entranceDirection) {
                case CardinalDirection.WEST:
                    intersection.setRoadwayX(sourceStreet.getRoadway().getX() - intersection.getRoadwayWidth()
                        - sourceStreet.getWaysFirstRoadwayWidth());
                    intersection.setRoadwayY(sourceStreet.getRoadway().getY());
                    break;
                case CardinalDirection.EAST:
                    intersection.setRoadwayX(sourceStreet.getRoadway().getX() + sourceStreet.getLength()
                        + sourceStreet.getWaysFirstRoadwayWidth());
                    intersection.setRoadwayY(sourceStreet.getRoadway().getY());
                    break;
                case CardinalDirection.NORTH:
                    intersection.setRoadwayX(sourceStreet.getRoadway().getX());
                    intersection.setRoadwayY(sourceStreet.getRoadway().getY() - intersection.getRoadwayHeight()
                        - sourceStreet.getWaysFirstRoadwayWidth());
                    break;
                case CardinalDirection.SOUTH:
                    intersection.setRoadwayX(sourceStreet.getRoadway().getX());
                    intersection.setRoadwayY(sourceStreet.getRoadway().getY() + sourceStreet.getLength()
                        + sourceStreet.getWaysFirstRoadwayWidth());
                    break;
            }
            district.updateMinX(intersection.getX());
            district.updateMinY(intersection.getY());
            district.updateMaxX(intersection.getX() + intersection.getWidth());
            district.updateMaxY(intersection.getY() + intersection.getHeight());
        }

        let streets: Map<CardinalDirection, Street> = intersection.getStreets();
        for (let direction of Array.from(streets.keys())) {
            let street: Street = streets.get(direction);
            if (!!street && street !== sourceStreet) {
                this.calculateCoordinatesFromStreet(street, intersection, direction, district);
            }
        }
    }

    private calculateCoordinatesFromStreet(street: Street,
                                           sourceIntersection: Intersection,
                                           exitDirection: CardinalDirection,
                                           district: District) {
        if (!street.getX() || !street.getY()) {
            switch(exitDirection) {
                case CardinalDirection.WEST:
                    street.setRoadwayX(sourceIntersection.getRoadwayX() - street.getLength()
                        - street.getWaysFirstRoadwayWidth());
                    street.setRoadwayY(sourceIntersection.getRoadwayY());
                    break;
                case CardinalDirection.EAST:
                    street.setRoadwayX(sourceIntersection.getRoadwayX() + sourceIntersection.getRoadwayWidth()
                        + street.getWaysFirstRoadwayWidth());
                    street.setRoadwayY(sourceIntersection.getRoadwayY());
                    break;
                case CardinalDirection.NORTH:
                    street.setRoadwayX(sourceIntersection.getRoadwayX());
                    street.setRoadwayY(sourceIntersection.getRoadwayY() - street.getLength()
                        - street.getWaysFirstRoadwayWidth());
                    break;
                case CardinalDirection.SOUTH:
                    street.setRoadwayX(sourceIntersection.getRoadwayX());
                    street.setRoadwayY(sourceIntersection.getRoadwayY() + sourceIntersection.getRoadwayHeight()
                        + street.getWaysFirstRoadwayWidth());
                    break;
            }
            district.updateMinX(street.getX());
            district.updateMinY(street.getY());
            if (street.getOrientation() == Orientation.HORIZONTAL) {
                district.updateMaxX(street.getX() + street.getLength());
                district.updateMaxY(street.getY() + street.getWidth());
            } else {
                district.updateMaxX(street.getX() + street.getWidth());
                district.updateMaxY(street.getY() + street.getLength());
            }

            let intersections: Map<CardinalDirection, Intersection> = street.getIntersections();
            for (let direction of Array.from(intersections.keys())) {
                let intersection: Intersection = intersections.get(direction);
                if (intersection !== sourceIntersection) {
                    this.calculateCoordinatesFromIntersection(intersection, street, direction, district);
                }
            }
        }
    }
}