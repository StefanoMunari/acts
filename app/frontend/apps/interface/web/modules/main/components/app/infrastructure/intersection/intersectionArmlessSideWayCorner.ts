import {Rect} from "../../draw/rect";
import {Street} from "../street/street";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";
import {Way} from "../way/way";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {RectComponent} from "../../draw/rect.component";
import {IntersectionArmlessSideWay} from "./intersectionArmlessSideWay";

export class IntersectionArmlessSideWayCorner extends IntersectionArmlessSideWay {
    constructor(way: Way, street: Street,
                offset: number, armlessDirection: CardinalDirection,
                referenceDirection: CardinalDirection,
                intersection: Intersection) {
        super(way, street, offset, armlessDirection, referenceDirection, intersection);
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number,
                     maxYPlusHeight: number,
                     drawer: DrawerAdapter): ShapeComponent[] {

        let x: number = 0,
            y: number = 0,
            width: number = 0,
            height: number = 0;
        let armlessSideWay: Rect = new Rect();
        armlessSideWay.setFill(this.getWay().getColor());

        switch(this.getArmlessDirection()) {
            case CardinalDirection.EAST:
                x = x + this.getOffset();
                break;
            case CardinalDirection.WEST:
                x = x - this.getOffset();
                break;
            case CardinalDirection.SOUTH:
                y = y + this.getOffset();
                break;
            case CardinalDirection.NORTH:
                y = y - this.getOffset();
                break;

            default:
                throw new Error(armlessSideWay + " is an unknown Cardinal Direction");
        }

        if (DirectionUtils.getOrientationByCardinalDirection(this.getArmlessDirection()) === Orientation.HORIZONTAL) {
            x = x + this.getIntersection().getRoadwayX();
            width = width + this.getWay().getWidth();

            switch(this.getReferenceDirection()) {
                case CardinalDirection.NORTH:
                    y = this.getStreet().getY() + this.getStreet().getLength();
                    height = y - this.getStreet().getY() + this.getOffset();
                    break;
                case CardinalDirection.SOUTH:
                    y = this.getIntersection().getRoadwayY() - this.getOffset();
                    height = this.getStreet().getY() - y;
                    break;
                case CardinalDirection.WEST:
                case CardinalDirection.EAST:
                    y = this.getStreet().getY();
                    height = this.getStreet().getLength() + this.getOffset();
            }
        } else {
            y = y + this.getIntersection().getRoadwayY();
            height = height + this.getWay().getWidth();

            switch(this.getReferenceDirection()) {
                case CardinalDirection.WEST:
                    x = this.getStreet().getX() + this.getStreet().getLength();
                    width = x - this.getStreet().getX() + this.getOffset();
                    break;
                case CardinalDirection.EAST:
                    x = this.getIntersection().getRoadwayX() - this.getOffset();
                    width = this.getStreet().getX() - x;
                    break;
                case CardinalDirection.NORTH:
                case CardinalDirection.SOUTH:
                    x = this.getStreet().getX();
                    height = this.getStreet().getLength() + this.getOffset();
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