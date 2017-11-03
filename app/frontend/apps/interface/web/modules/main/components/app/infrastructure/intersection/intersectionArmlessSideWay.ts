import {Rect} from "../../draw/rect";
import {Street} from "../street/street";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";
import {IntersectionUtils} from "./intersection.utils";
import {Way} from "../way/way";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";
import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {RectComponent} from "../../draw/rect.component";

export class IntersectionArmlessSideWay implements Infrastructure {
    constructor(private way: Way, private street: Street,
                private offset: number,
                private armlessDirection: CardinalDirection,
                private referenceDirection: CardinalDirection,
                private intersection: Intersection) {}

    public getWay(): Way {
        return this.way;
    }

    public getStreet(): Street {
        return this.street;
    }

    public getOffset(): number{
        return this.offset;
    }

    public getArmlessDirection(): CardinalDirection{
        return this.armlessDirection;
    }

    public getReferenceDirection(): CardinalDirection{
        return this.referenceDirection;
    }

    public getIntersection(): Intersection{
        return this.intersection;
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number,
                     maxYPlusHeight: number,
                     drawer: DrawerAdapter): ShapeComponent[] {

        let x, y, width, height: number;
        let armlessSideWay: Rect = new Rect();
        armlessSideWay.setFill(this.way.getColor());

        switch(DirectionUtils.getOrientationByCardinalDirection(this.armlessDirection)) {
            case Orientation.HORIZONTAL:
                x = this.intersection.getRoadwayX();
                height = this.street.getWidth();
                width = this.way.getWidth();
                break;
            case Orientation.VERTICAL:
                y = this.intersection.getRoadwayY();
                width = this.street.getWidth();
                height = this.way.getWidth();
        }

        switch(this.armlessDirection) {
            case CardinalDirection.EAST:
                x = x + this.offset;
                break;
            case CardinalDirection.WEST:
                x = x - this.offset;
                break;
            case CardinalDirection.SOUTH:
                y = y + this.offset;
                break;
            case CardinalDirection.NORTH:
                y = y - this.offset;
        }

        let armlessOppositeDirection: CardinalDirection = DirectionUtils.reverseCardinalDirection(this.armlessDirection);
        const streets = this.intersection.getStreets();
        let oppositeStreet: Street = streets.get(armlessOppositeDirection);

        switch(DirectionUtils.getOrientationByCardinalDirection(this.armlessDirection)) {
            case Orientation.HORIZONTAL:
                const intersectionY = this.intersection.getY();
                const intersectionHeight = this.intersection.getHeight();
                if (streets.has(CardinalDirection.NORTH) &&
                    streets.get(CardinalDirection.NORTH) === null) {

                    y = intersectionY - oppositeStreet.getWidth() / 2 + intersectionHeight / 2;

                } else if(streets.has(CardinalDirection.SOUTH) &&
                    streets.get(CardinalDirection.SOUTH) === null) {

                    y = intersectionY + intersectionHeight - oppositeStreet.getWidth() / 2 + intersectionHeight / 2;

                } else if (this.referenceDirection === CardinalDirection.NORTH){

                    y = this.street.getY() + this.street.getLength();

                } else if (this.referenceDirection === CardinalDirection.SOUTH){

                    y = intersectionY - this.offset;

                }
                break;
            case Orientation.VERTICAL:
                const intersectionX = this.intersection.getX();
                const intersectionWidth = this.intersection.getWidth();
                if(streets.has(CardinalDirection.WEST) &&
                    streets.get(CardinalDirection.WEST) === null) {

                    x = intersectionX - oppositeStreet.getWidth() / 2 + intersectionWidth / 2;

                } else if(streets.has(CardinalDirection.EAST) &&
                    streets.get(CardinalDirection.EAST) === null) {

                    x = intersectionX + intersectionWidth - oppositeStreet.getWidth() / 2 + intersectionWidth / 2;

                } else if (this.referenceDirection === CardinalDirection.WEST){

                    x = this.street.getX() + this.street.getLength();

                } else if (this.referenceDirection === CardinalDirection.EAST){

                    x = intersectionX - this.offset;

                }
        }

        armlessSideWay.setX(x);
        armlessSideWay.setY(y);
        armlessSideWay.setWidth(width);
        armlessSideWay.setHeight(height);
        armlessSideWay.setMinX(minX);
        armlessSideWay.setMinY(minY);
        armlessSideWay.setMaxXPlusWidth(maxXPlusWidth);
        armlessSideWay.setMaxYPlusHeight(maxYPlusHeight);
        return [RectComponent.create(armlessSideWay, drawer)];
    }

}