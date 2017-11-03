import {CardinalDirection} from "./cardinalDirection";
import {StraightDirection} from "./straightDirection";
import {Orientation} from "./orientation";

export class DirectionUtils {
    public static reverseCardinalDirection(direction: CardinalDirection) {
        switch(direction) {
            case CardinalDirection.WEST:
                return CardinalDirection.EAST;
            case CardinalDirection.EAST:
                return CardinalDirection.WEST;
            case CardinalDirection.NORTH:
                return CardinalDirection.SOUTH;
            case CardinalDirection.SOUTH:
                return CardinalDirection.NORTH;
        }
    }

    public static reverseOrientation(orientation: Orientation): Orientation {
        switch(orientation) {
            case Orientation.VERTICAL:
                return Orientation.HORIZONTAL;
            case Orientation.HORIZONTAL:
                return Orientation.VERTICAL;
        }
    }

    public static getOrientationByDirection(direction : StraightDirection): Orientation{
        switch(direction) {
            case StraightDirection.WEST_EAST:
            case StraightDirection.EAST_WEST:
                return Orientation.HORIZONTAL;
            case StraightDirection.NORTH_SOUTH:
            case StraightDirection.SOUTH_NORTH:
                return Orientation.VERTICAL;
        }
    }

    public static getOrientationByCardinalDirection(direction : CardinalDirection): Orientation{
        switch(direction) {
            case CardinalDirection.WEST:
            case CardinalDirection.EAST:
                return Orientation.HORIZONTAL;
            case CardinalDirection.NORTH:
            case CardinalDirection.SOUTH:
                return Orientation.VERTICAL;
        }
    }

    public static getDirectionsByOrientation(orientation: Orientation) {
        let directions: Array<StraightDirection> = new Array<StraightDirection>();
        switch(orientation) {
            case Orientation.HORIZONTAL:
                directions.push(StraightDirection.EAST_WEST);
                directions.push(StraightDirection.WEST_EAST);
                break;
            case Orientation.VERTICAL:
                directions.push(StraightDirection.NORTH_SOUTH);
                directions.push(StraightDirection.SOUTH_NORTH);
                break;
        }
        return directions;
    }
}