import {Rect} from "../../draw/rect";
import {ShapeComponent} from "../../draw/shape.component";
import {RectComponent} from "../../draw/rect.component";
import {Infrastructure} from "../infrastructure";
import {Treadable} from "../treadable";
import {Traveller} from "../../traveller/model/traveller";
import {TravellerMotion} from "../../traveller/travellerMotion/travellerMotion";
import {Lane} from "../lane/lane";
import {ZebraCrossing} from "../pavementMarking/zebraCrossing";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";
import {Color} from "../../shared/color";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {BikeCrossing} from "../pavementMarking/bikeCrossing";

export abstract class Stretch extends Infrastructure implements Treadable {
    private id: number = null;
    private static standardLength : number = 30;
    private static standardWidth : number = Stretch.standardLength;
    private x : number;
    private y : number;
    private lane : Lane;
    private pedestrianCrossing : ZebraCrossing = null;
    private bikeCrossing : BikeCrossing = null;
    private orientation: Orientation;

    public getId(): number {
        return this.id;
    }

    public getX(): number {
        return this.x;
    }

    public getY(): number {
        return this.y;
    }

    public getLane(): Lane {
        return this.lane;
    }

    public getOrientation(): Orientation {
        return this.orientation;
    }

    public setOrientation(orientation: Orientation) {
        this.orientation = orientation;
    }

    public setId(id: number){
        this.id = id;
    }

    public setLane(lane : Lane) {
        this.lane = lane;
    }

    public static getStandardWidth(): number {
        return Stretch.standardWidth;
    }

    public static getStandardLength(): number {
        return Stretch.standardLength;
    }

    public getWidth(): number {
        return Stretch.standardWidth;
    }

    public getLength(): number {
        return Stretch.standardLength;
    }

    public abstract getColor(): Color;

    public abstract tread(traveller : Traveller, visitor : TravellerMotion);

    public setX(x : number) {
        this.x = x;
        if (this.pedestrianCrossing !== null) {
            this.pedestrianCrossing.setX(x);
        }
        if (this.bikeCrossing !== null) {
            this.bikeCrossing.setX(x);
        }
    }

    public setY(y : number) {
        this.y = y;
        if (this.pedestrianCrossing !== null) {
            this.pedestrianCrossing.setY(y);
        }
        if (this.bikeCrossing !== null) {
            this.bikeCrossing.setY(y);
        }
    }

    public withPedestrianCrossing() {
        this.pedestrianCrossing = new ZebraCrossing();
        this.pedestrianCrossing.setLaneDirection(this.getLane().getDirection());
        this.pedestrianCrossing.setOrientation(DirectionUtils
            .reverseOrientation(this.getOrientation()));
        this.pedestrianCrossing.setWidth(this.getLength());
        this.pedestrianCrossing.setLength(this.getWidth());
        this.pedestrianCrossing.setStroke(Color.WHITE);
    }

    public withBikeCrossing() {
        this.bikeCrossing = new BikeCrossing();
        this.bikeCrossing.setLaneDirection(this.getLane().getDirection());
        this.bikeCrossing.setOrientation(DirectionUtils
            .reverseOrientation(this.getOrientation()));
        this.bikeCrossing.setWidth(this.getLength());
        this.bikeCrossing.setLength(this.getWidth());
        this.bikeCrossing.setStroke(Color.WHITE);
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let rect = new Rect();

        if (this.id !== null) {
            rect.setId("stretch" + this.id);
        }

        rect.setX(this.x);
        rect.setY(this.y);
        if (this.getOrientation() === Orientation.HORIZONTAL) {
            rect.setWidth(this.getLength());
            rect.setHeight(this.getWidth());
        } else {
            rect.setWidth(this.getWidth());
            rect.setHeight(this.getLength());
        }
        rect.setFill(this.getColor());
        rect.setMinX(minX);
        rect.setMinY(minY);
        rect.setMaxXPlusWidth(maxXPlusWidth);
        rect.setMaxYPlusHeight(maxYPlusHeight);

        let shapes : Array<ShapeComponent> = new Array<ShapeComponent>(RectComponent.create(rect, drawer));

        if (!!this.pedestrianCrossing) {
            shapes = shapes.concat(this.pedestrianCrossing.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }
        if (!!this.bikeCrossing) {
            shapes = shapes.concat(this.bikeCrossing.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }
        return shapes;
    }
}