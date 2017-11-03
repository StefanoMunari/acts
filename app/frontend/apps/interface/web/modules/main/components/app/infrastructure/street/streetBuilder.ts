import {Orientation} from "../../shared/orientation";
import {Way} from "../way/way";
import {Street} from "./street";
import {ResidentialWay} from "../way/residentialWay";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";
import {FacilityStretch} from "../facility/stretch/facilityStretch";

export class StreetBuilder {
    private street: Street;
    private residentialWays: Map<StraightDirection, ResidentialWay> = new Map<StraightDirection, ResidentialWay>();

    constructor(private streetId : number) {
        this.street = new Street(streetId);
    }

    public withOrientation(orientation: Orientation) {
        this.street.setOrientation(orientation);
        this.street.getWays().forEach(way => way.setOrientation(orientation));
    }

    public withRoadwayOrdinal(ordinal: number) {
        this.street.setRoadwayOrdinal(ordinal);
    }

    public addWay(ordinal: number, way: Way) {
        this.street.addWay(ordinal, way);
    }

    public withFacility(facility: FacilityStretch, sliceIndex: number, direction: StraightDirection) {
        if (!this.residentialWays.has(direction)) {
            let way: ResidentialWay = ResidentialWay.create(this.street.getFirstWay(), direction);
            this.residentialWays.set(direction, way);
        }
        let way: ResidentialWay = this.residentialWays.get(direction);
        way.putStretchAt(sliceIndex, facility);
    }

    public build(): Street {
        this.generateMissingResidentialWays();
        this.addResidentialWaysToStreet();
        return this.street;
    }

    private generateMissingResidentialWays() {
        for (let direction of DirectionUtils.getDirectionsByOrientation(this.street.getOrientation())) {
            if (!this.residentialWays.has(direction)) {
                let way: ResidentialWay = ResidentialWay.create(this.street.getFirstWay(), direction);
                this.residentialWays.set(direction, way);
            }
        }
    }

    private addResidentialWaysToStreet() {
        for (let direction of Array.from(this.residentialWays.keys())) {
            let residentialOrdinal: number;
            if (direction === StraightDirection.EAST_WEST
                || direction === StraightDirection.NORTH_SOUTH) {
                residentialOrdinal = this.street.getWayMinKey() - 1;
            } else {
                residentialOrdinal = this.street.getWayMaxKey() + 1;
            }
            let residentialWay: ResidentialWay = this.residentialWays.get(direction);
            residentialWay.setOrdinal(residentialOrdinal);
            this.addWay(residentialOrdinal, residentialWay);
        }
    }
}