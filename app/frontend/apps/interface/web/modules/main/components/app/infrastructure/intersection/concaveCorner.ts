import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {ExitStreet} from "./exitStreet";
import {DistrictExitStreet} from "./districtExitStreet";
import {ExitStreet2} from "./exitStreet2";

export class ConcaveCorner implements Infrastructure {

    constructor(private d1: CardinalDirection,
                private d2: CardinalDirection,
                private intersection: Intersection) {}

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let shapes: Array<ShapeComponent> = [];

        let directions: CardinalDirection[] = [this.d1, this.d2];

        for (let d: number = 0; d < directions.length; ++d) {
            let direction: CardinalDirection = directions[d];
            let otherDirection: CardinalDirection = directions[1 - d];
            let street: Street = this.intersection.getStreets().get(direction);
            let ways: Array<Way> = null;
            let exitStreet: ExitStreet;
            if (street !== null) {

                switch (direction) {
                    case CardinalDirection.WEST:
                    case CardinalDirection.EAST:
                        if (otherDirection === CardinalDirection.NORTH) {
                            ways = street.getWaysFirstRoadway();
                        } else if (otherDirection === CardinalDirection.SOUTH) {
                            ways = street.getWaysAfterRoadway();
                        }
                        break;
                    case CardinalDirection.NORTH:
                    case CardinalDirection.SOUTH:
                        if (otherDirection === CardinalDirection.WEST) {
                            ways = street.getWaysFirstRoadway();
                        } else if (otherDirection === CardinalDirection.EAST) {
                            ways = street.getWaysAfterRoadway();
                        }
                        break;
                    default:
                        throw new Error(direction + " is an unknown Cardinal Direction");
                }

                if (ways === null) {
                    throw new Error(direction + " and " + otherDirection + " directions does not identify a corner");
                }

                if (this.intersection.getStreets().has(otherDirection)) {
                    exitStreet = new ExitStreet(direction, ways, street, this.intersection);
                } else {
                    exitStreet = new ExitStreet2(direction, ways, street, this.intersection);
                }
            } else {
                let otherStreet: Street = this.intersection.getStreets().get(otherDirection);

                switch (otherDirection) {
                    case CardinalDirection.WEST:
                    case CardinalDirection.EAST:
                        if (direction === CardinalDirection.NORTH) {
                            ways = otherStreet.getWaysFirstRoadway();
                        } else if (direction === CardinalDirection.SOUTH) {
                            ways = otherStreet.getWaysAfterRoadway();
                        }
                        break;
                    case CardinalDirection.NORTH:
                    case CardinalDirection.SOUTH:
                        if (direction === CardinalDirection.WEST) {
                            ways = otherStreet.getWaysFirstRoadway();
                        } else if (direction === CardinalDirection.EAST) {
                            ways = otherStreet.getWaysAfterRoadway();
                        }
                        break;
                    default:
                        throw new Error(direction + " is an unknown Cardinal Direction");
                }

                if (ways === null) {
                    throw new Error(direction + " and " + otherDirection + " directions does not identify a corner");
                }

                exitStreet = new DistrictExitStreet(direction, ways, street, this.intersection);
            }
            shapes = shapes.concat(exitStreet.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        return shapes;
    }

}