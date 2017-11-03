import {Infrastructure} from '../infrastructure';
import {ShapeComponent} from '../../draw/shape.component';
import {Orientation} from "../../shared/orientation";
import {Lane} from "../lane/lane";
import {StraightDirection} from "../../shared/straightDirection";
import {Color} from "../../shared/color";
import {Stretch} from "../stretch/stretch";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export abstract class Way extends Infrastructure {
    private id: number;
    private height: number;
    private orientation: Orientation;
    private ordinal: number;

    constructor() {
        super();
        this.id = null;
        this.height = 0;
    }

    public abstract getColor(): Color;
    
    public abstract getLanes(): Array<Lane>;

    public setX(x : number) {
        if (this.getOrientation() === Orientation.HORIZONTAL) {
            for (let lane of this.getLanes()) {
                lane.setX(x);
            }
        } else {
            let southNorthLaneFound: boolean = false;
            let setStretchXCoordinatesCounter: number = 0;
            for (let lane of this.getLanes()) {
                if (lane.getDirection() === StraightDirection.SOUTH_NORTH) {
                    southNorthLaneFound = true;
                } else {
                    lane.setX(x + Stretch.getStandardWidth() * setStretchXCoordinatesCounter);
                    ++setStretchXCoordinatesCounter;
                }
            }
            if (southNorthLaneFound) {
                for (let lane of this.getLanes()) {
                    if (lane.getDirection() === StraightDirection.SOUTH_NORTH) {
                        lane.setX(x + Stretch.getStandardWidth() * setStretchXCoordinatesCounter);
                        ++setStretchXCoordinatesCounter;
                    }
                }
            }
        }
    }

    public setY(y : number) {
        if (this.getOrientation() === Orientation.VERTICAL) {
            for (let lane of this.getLanes()) {
                lane.setY(y);
            }
        } else {
            let westEastLaneFound: boolean = false;
            let setStretchYCoordinatesCounter: number = 0;
            for (let lane of this.getLanes()) {
                if (lane.getDirection() === StraightDirection.WEST_EAST) {
                    westEastLaneFound = true;
                } else {
                    lane.setY(y + Stretch.getStandardWidth() * setStretchYCoordinatesCounter);
                    ++setStretchYCoordinatesCounter;
                }
            }
            if (westEastLaneFound) {
                for (let lane of this.getLanes()) {
                    if (lane.getDirection() === StraightDirection.WEST_EAST) {
                        lane.setY(y + Stretch.getStandardWidth() * setStretchYCoordinatesCounter);
                        ++setStretchYCoordinatesCounter;
                    }
                }
            }
        }
    }

    public getId(): number {
        return this.id;
    }

    public getX(): number {
        let smallestStretchX: number = this.getLanes()[0].getStretchWithSmallestCoordinates().getX();
        for (let lane of this.getLanes()) {
            smallestStretchX = Math.min(smallestStretchX, lane.getStretchWithSmallestCoordinates().getX());
        }
        return smallestStretchX;
    }

    public getY(): number {
        let smallestStretchY: number = this.getLanes()[0].getStretchWithSmallestCoordinates().getY();
        for (let lane of this.getLanes()) {
            smallestStretchY = Math.min(smallestStretchY, lane.getStretchWithSmallestCoordinates().getY());
        }
        return smallestStretchY;
    }

    public getWidth(): number {
        let width: number = 0;
        for (let lane of this.getLanes()) {
            width = width + lane.getWidth();
        }
        return width;
    }

    public getLength(): number {
        return this.getLanes()[0].getLength();
    }

    public getLengthAsCountStretches(): number {
        return this.getLanes()[0].countStretches();
    }

    public getOrientation(): Orientation {
        return this.orientation;
    }

    public getOrdinal(): number {
        return this.ordinal;
    }

    public setId(id: number): void {
        this.id = id;
    }

    public setHeight(height: number): void {
        this.height = height;
    }

    public setOrientation(orientation: Orientation): void {
        this.orientation = orientation;
    }

    public setOrdinal(ordinal: number): void {
        this.ordinal = ordinal;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let shapes : Array<ShapeComponent> = new Array<ShapeComponent>();

        for (let lane of this.getLanes()) {
            shapes = shapes.concat(lane.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        return shapes;
    }

    public addLane(lane: Lane) {
        this.getLanes().push(lane);
    }
}