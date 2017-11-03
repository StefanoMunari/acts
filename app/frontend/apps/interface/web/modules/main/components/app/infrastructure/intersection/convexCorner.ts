import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";
import {IntersectionArmlessSideCorner} from "./intersectionArmlessSideCorner";

export class ConvexCorner implements Infrastructure {

    constructor(private d1: CardinalDirection,
                private d2: CardinalDirection,
                private intersection: Intersection) {}

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let shapes: Array<ShapeComponent> = [];

        let directions: CardinalDirection[] = [this.d1, this.d2];

        for (let d: number = 0; d < directions.length; ++d) {
            let direction: CardinalDirection = directions[d];
            const intersectionArmlessSideCorner: IntersectionArmlessSideCorner =
                new IntersectionArmlessSideCorner(direction, this.intersection);
            shapes = shapes.concat(intersectionArmlessSideCorner
                .getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        return shapes;
    }

}