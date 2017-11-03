import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";

export class IntersectionUtils {
    public static findReferenceDirection(intersection: Intersection, exitDirection: CardinalDirection): CardinalDirection {
        let referenceDirection: CardinalDirection = null;
        switch (exitDirection){
            case CardinalDirection.NORTH:
            case CardinalDirection.SOUTH:
                if (!!intersection.getStreets().get(CardinalDirection.WEST)) {
                    referenceDirection = CardinalDirection.WEST;
                } else if (!!intersection.getStreets().get(CardinalDirection.EAST)) {
                    referenceDirection = CardinalDirection.EAST;
                } else if (!!intersection.getStreets().get(CardinalDirection.NORTH)) {
                    referenceDirection = CardinalDirection.NORTH;
                } else { // southern street is not null
                    referenceDirection = CardinalDirection.SOUTH;
                }
                break;
            case CardinalDirection.EAST:
            case CardinalDirection.WEST:
                if (!!intersection.getStreets().get(CardinalDirection.NORTH)) {
                    referenceDirection = CardinalDirection.NORTH;
                } else if (!!intersection.getStreets().get(CardinalDirection.SOUTH)) {
                    referenceDirection = CardinalDirection.SOUTH;
                } else if (!!intersection.getStreets().get(CardinalDirection.WEST)) {
                    referenceDirection = CardinalDirection.WEST;
                } else {  // eastern street is not null
                    referenceDirection = CardinalDirection.EAST;
                }
        }
        return referenceDirection;
    }
}