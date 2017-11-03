import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {Rect} from "../../draw/rect";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";
import {Intersection} from "./intersection";
import {RectComponent} from "../../draw/rect.component";

export class ExitStretch implements Infrastructure {

    constructor(private exitDirection: CardinalDirection,
                private way: Way, private street: Street,
                private offset: number, private intersection: Intersection) {}

    public getExitDirection(): CardinalDirection {
        return this.exitDirection;
    }

    public getWay(): Way {
        return this.way;
    }

    public getStreet(): Street {
        return this.getStreet();
    }

    public getOffset(): number{
        return this.offset;
    }

    public getIntersection(): Intersection {
        return this.intersection;
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let x, y, width, height: number;
        let exitStretch: Rect = new Rect();
        exitStretch.setFill(this.way.getColor());

        switch(DirectionUtils.getOrientationByCardinalDirection(this.exitDirection)) {
            case Orientation.HORIZONTAL:
                y = this.way.getY();
                height = this.way.getWidth();
                break;
            case Orientation.VERTICAL:
                x = this.way.getX();
                width = this.way.getWidth();
        }

        const intersectionX = this.intersection.getX();
        const intersectionWidth = this.intersection.getWidth();
        const intersectionHeight = this.intersection.getHeight();
        const intersectionY = this.intersection.getY();

        switch (this.exitDirection) {
            case CardinalDirection.EAST:
                x = intersectionX + intersectionWidth + this.offset;
                width = this.way.getX() - x;
                break;
            case CardinalDirection.WEST:
                x = this.way.getX() + this.way.getLength();
                width = intersectionX - x - this.offset;
                break;
            case CardinalDirection.SOUTH:
                y = intersectionY + intersectionHeight + this.offset;
                height = this.way.getY() - y;
                break;
            case CardinalDirection.NORTH:
                y = this.way.getY() + this.way.getLength();
                height = intersectionY - y - this.offset;
                break;

            default:
                throw new Error(this.exitDirection + " is an unknown Cardinal Direction");
        }

        exitStretch.setX(x);
        exitStretch.setY(y);
        exitStretch.setWidth(width);
        exitStretch.setHeight(height);
        exitStretch.setMinX(minX);
        exitStretch.setMinY(minY);
        exitStretch.setMaxXPlusWidth(maxXPlusWidth);
        exitStretch.setMaxYPlusHeight(maxYPlusHeight);

        return [RectComponent.create(exitStretch, drawer)];
    }

}