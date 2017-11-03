import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Intersection} from "./intersection";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {Infrastructure} from "../infrastructure";
import {RoadwayStretch} from "../stretch/roadwayStretch";
import {DirectionUtils} from "../../shared/directionUtils";
import {IntersectionUtils} from "./intersection.utils";
import {Rect} from "../../draw/rect";
import {Orientation} from "../../shared/orientation";
import {RectComponent} from "../../draw/rect.component";

export class DistrictExitRoadwayStretch implements Infrastructure {

    constructor(private exitDirection: CardinalDirection,
                private intersection: Intersection) {}

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let color = RoadwayStretch.getStandardColor();

        let referenceDirection: CardinalDirection = IntersectionUtils.findReferenceDirection(this.intersection, this.exitDirection);
        let referenceStreet: Street = this.intersection.getStreets().get(referenceDirection);

        let x: number;
        let y: number;
        let width: number;
        let height: number;

            const intersectionX = this.intersection.getX();
            const intersectionY = this.intersection.getY();
            const intersectionWidth = this.intersection.getWidth();
            const intersectionHeight = this.intersection.getHeight();

            switch(DirectionUtils.getOrientationByCardinalDirection(this.exitDirection)) {
            case Orientation.VERTICAL:
                x = intersectionX;
                width = intersectionWidth;
                height = referenceStreet.getWidth() / 2 - intersectionHeight / 2;
                break;
            case Orientation.HORIZONTAL:
                y = intersectionY;
                height = intersectionHeight;
                width = referenceStreet.getWidth() / 2 - intersectionWidth / 2;
        }

        switch(this.exitDirection){
            case CardinalDirection.NORTH:
                y = intersectionY - height;
                break;
            case CardinalDirection.SOUTH:
                y = intersectionY + intersectionHeight;
                break;
            case CardinalDirection.WEST:
                x = intersectionX - width;
                break;
            case CardinalDirection.EAST:
                x = intersectionX + intersectionWidth;
        }

        let stretch: Rect = new Rect();
        stretch.setX(x);
        stretch.setY(y);
        stretch.setWidth(width);
        stretch.setHeight(height);
        stretch.setFill(color);
        stretch.setMinX(minX);
        stretch.setMinY(minY);
        stretch.setMaxXPlusWidth(maxXPlusWidth);
        stretch.setMaxYPlusHeight(maxYPlusHeight);

        return [RectComponent.create(stretch, drawer)];
    }
}