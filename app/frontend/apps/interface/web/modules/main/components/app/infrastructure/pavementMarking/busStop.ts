import {Text} from "../../draw/text";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {Orientation} from "../../shared/orientation";
import {ShapeComponent} from "../../draw/shape.component";
import {Color} from "../../shared/color";
import {TextComponent} from "../../draw/text.component";
import {StraightDirection} from "../../shared/straightDirection";
import {Infrastructure} from "../infrastructure";
import {DirectionUtils} from "../../shared/directionUtils";

export class BusStop implements Infrastructure {

    private x: number;
    private y: number;
    private width: number;
    private height: number;
    private direction: StraightDirection;

    public setX(x: number) {
        this.x = x;
    }

    public getX(): number {
        return this.x;
    }

    public setY(y: number) {
        this.y = y;
    }

    public getY(): number {
        return this.y;
    }

    public setDirection(direction: StraightDirection) {
        this.direction = direction;
    }

    public getDirection(): StraightDirection {
        return this.direction;
    }

    public setWidth(width: number) {
        this.width = width;
    }

    public setHeight(height: number) {
        this.height = height;
    }

    public getWidth(): number {
        return this.width;
    }

    public getHeight(): number {
        return this.height;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let pavementMarking: Text = new Text("BUS");
        pavementMarking.addClass("pavementMarking");

        pavementMarking.setX(this.getX());
        pavementMarking.setY(this.getY());

        if (DirectionUtils.getOrientationByDirection(this.direction) === Orientation.HORIZONTAL) {
            pavementMarking.setWidth(this.getWidth());
            pavementMarking.setHeight(this.getHeight());

            if (this.direction === StraightDirection.WEST_EAST) {
                pavementMarking.setRotation(90);
            } else {
                pavementMarking.setRotation(270);
            }
        } else { // VERTICAL ORIENTATION
            pavementMarking.setWidth(this.getHeight());
            pavementMarking.setHeight(this.getWidth());

            if (this.direction === StraightDirection.NORTH_SOUTH) {
                pavementMarking.setRotation(180);
            }
        }
        pavementMarking.setFill(Color.GOLD);
        pavementMarking.setMinX(minX);
        pavementMarking.setMinY(minY);
        pavementMarking.setMaxXPlusWidth(maxXPlusWidth);
        pavementMarking.setMaxYPlusHeight(maxYPlusHeight);

        drawer.createSVG("canvas", {
            x: minX,
            y: minY,
            width: maxXPlusWidth,
            height: maxYPlusHeight
        });

        let busStopGroup = drawer.createGroup();
        drawer.addClass("busStop", busStopGroup);

        return [TextComponent.create(pavementMarking, drawer, busStopGroup)];
    }
}