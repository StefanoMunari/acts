import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {ExitStretch} from "./exitStretch";
import {Intersection} from "./intersection";

export class ExitStreet implements Infrastructure {

    constructor(private exitDirection: CardinalDirection,
                private ways: Array<Way>, private street: Street,
                private intersection: Intersection) {}

    public getExitDirection(): CardinalDirection {
        return this.exitDirection;
    }

    public getWays(): Array<Way> {
        return this.ways;
    }

    public getStreet(): Street {
        return this.street;
    }

    public getIntersection(): Intersection {
        return this.intersection;
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): ShapeComponent[] {

        let exitStretches: Array<ShapeComponent> = [];

        let offset: number = 0;
        for (let way of this.ways) {
            let exitStretch: ExitStretch = this.generateExitStretch(way, offset);
            exitStretches = exitStretches.concat(exitStretch.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
            offset = offset + way.getWidth();
        }
        return exitStretches;

    }

    public generateExitStretch(way: Way, offset: number): ExitStretch {
        return new ExitStretch(this.exitDirection, way, this.street, offset, this.intersection);
    }

}