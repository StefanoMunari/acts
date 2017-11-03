import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {Intersection} from "./intersection";
import {RectComponent} from "../../draw/rect.component";
import {ExitStretch} from "./exitStretch";
import {Rect} from "../../draw/rect";
import {IntersectionUtils} from "./intersection.utils";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";

export class DistrictExitStretch extends ExitStretch {

    constructor(exitDirection: CardinalDirection,
                way: Way, street: Street,
                offset: number, intersection: Intersection) {
        super(exitDirection, way, street, offset, intersection);
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let referenceDirection: CardinalDirection = IntersectionUtils.findReferenceDirection(this.getIntersection(), this.getExitDirection());
        let referenceStreet: Street = this.getIntersection().getStreets().get(referenceDirection);

        let x: number;
        let y: number;
        let width: number;
        let height: number;

        const intersectionX = this.getIntersection().getX();
        const intersectionY = this.getIntersection().getY();
        const intersectionWidth = this.getIntersection().getWidth();
        const intersectionHeight = this.getIntersection().getHeight();

        switch(DirectionUtils.getOrientationByCardinalDirection(this.getExitDirection())) {
            case Orientation.VERTICAL:
                x = intersectionX;
                width = this.getWay().getWidth();
                if (intersectionX < this.getWay().getX()) {
                    x = x + intersectionWidth + this.getOffset();
                } else {
                    x = x - width - this.getOffset();
                }
                break;
            case Orientation.HORIZONTAL:
                y = intersectionY;
                height = this.getWay().getWidth();
                if (intersectionY < this.getWay().getY()) {
                    y = y + intersectionHeight + this.getOffset();
                } else {
                    y = y - height - this.getOffset();
                }
        }

        switch(this.getExitDirection()){
            case CardinalDirection.NORTH:
                y = intersectionY - (referenceStreet.getWidth() / 2 - intersectionHeight / 2);
                height = this.getWay().getY() + this.getWay().getWidth() - y;
                break;
            case CardinalDirection.SOUTH:
                y = this.getWay().getY() + this.getWay().getWidth();
                let endY = intersectionY + intersectionHeight + (referenceStreet.getWidth() / 2 - intersectionHeight / 2);
                height = endY - y;
                break;
            case CardinalDirection.WEST:
                x = intersectionX - (referenceStreet.getWidth() / 2 - intersectionWidth / 2);
                width = this.getWay().getX() - x;
                break;
            case CardinalDirection.EAST:
                x = this.getWay().getX() + this.getWay().getWidth();
                let endX = intersectionX + intersectionWidth + (referenceStreet.getWidth() / 2 - intersectionWidth / 2);
                width = endX - x;
        }

        let stretch: Rect = new Rect();
        stretch.setX(x);
        stretch.setY(y);
        stretch.setWidth(width);
        stretch.setHeight(height);
        stretch.setFill(this.getWay().getColor());
        stretch.setMinX(minX);
        stretch.setMinY(minY);
        stretch.setMaxXPlusWidth(maxXPlusWidth);
        stretch.setMaxYPlusHeight(maxYPlusHeight);

        return [RectComponent.create(stretch, drawer)];
    }

}