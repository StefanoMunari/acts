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
import {ExitStretch} from "./exitStretch";

export class ExitStretch2 extends ExitStretch {

    constructor(exitDirection: CardinalDirection,
                way: Way, street: Street,
                offset: number, intersection: Intersection) {
        super(exitDirection, way, street, offset, intersection);
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let x, y, width, height: number;
        let exitStretch: Rect = new Rect();
        exitStretch.setFill(this.getWay().getColor());

        switch(DirectionUtils.getOrientationByCardinalDirection(this.getExitDirection())) {
            case Orientation.HORIZONTAL:
                y = this.getWay().getY();
                height = this.getWay().getWidth();
                break;
            case Orientation.VERTICAL:
                x = this.getWay().getX();
                width = this.getWay().getWidth();
        }

        const intersectionX = this.getIntersection().getX();
        const intersectionWidth = this.getIntersection().getWidth();
        const intersectionHeight = this.getIntersection().getHeight();
        const intersectionY = this.getIntersection().getY();

        switch (this.getExitDirection()) {
            case CardinalDirection.EAST:
                x = intersectionX + intersectionWidth;
                width = this.getWay().getX() - x;
                break;
            case CardinalDirection.WEST:
                x = this.getStreet().getRoadway().getX() + this.getStreet().getRoadway().getLength();
                width = intersectionX - x;
                break;
            case CardinalDirection.SOUTH:
                y = intersectionY + intersectionHeight;
                height = this.getWay().getY() - y;
                break;
            case CardinalDirection.NORTH:
                y = this.getStreet().getRoadway().getY() + this.getStreet().getRoadway().getLength();
                height = intersectionY - y;
                break;

            default:
                throw new Error(this.getExitDirection() + " is an unknown Cardinal Direction");
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