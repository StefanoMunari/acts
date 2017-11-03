import {Stretch} from "../stretch/stretch";
import {ShapeComponent} from "../../draw/shape.component";
import {StraightDirection} from "../../shared/straightDirection";
import {Orientation} from "../../shared/orientation";
import {DirectionUtils} from "../../shared/directionUtils";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export class Lane {
    private stretches: Array<Stretch> = new Array<Stretch>();
    private id: number;
    private direction: StraightDirection = null;

    public setId(id: number){
        this.id = id;
    }

    public hasDirection(): boolean {
        return this.direction !== null;
    }

    public setDirection(direction: StraightDirection) {
        this.direction = direction;
    }

    public calculateOrientation(): Orientation {
        return DirectionUtils.getOrientationByDirection(this.direction);
    }

    public setX(x : number) {
        let orientation : Orientation = this.calculateOrientation();
        let stretches : Array<Stretch>;

        if (this.direction === StraightDirection.EAST_WEST) {
            stretches = this.stretches.slice().reverse();
        } else {
            stretches = this.stretches;
        }

        for (let stretch of stretches) {
            stretch.setX(x);
            if (orientation === Orientation.HORIZONTAL) {
                x = x + stretch.getLength();
            }
        }
    }

    public setY(y : number) {
        let orientation : Orientation = this.calculateOrientation();
        let stretches : Array<Stretch>;

        if (this.direction === StraightDirection.SOUTH_NORTH) {
            stretches = this.stretches.slice().reverse();
        } else {
            stretches = this.stretches;
        }

        for (let stretch of stretches) {
            stretch.setY(y);
            if (orientation === Orientation.VERTICAL) {
                y = y + stretch.getLength();
            }
        }
    }

    public getStretchWithSmallestCoordinates(): Stretch {
        let stretch : Stretch;

        if (!this.hasDirection()
            || this.getDirection() === StraightDirection.WEST_EAST
            || this.getDirection() === StraightDirection.NORTH_SOUTH){
            stretch = this.getFirstStretch();
        } else {
            stretch = this.getLastStretch();
        }

        return stretch;
    }

    public getStretchWithBiggestCoordinates(): Stretch {
        let stretch : Stretch;

        if (this.getDirection() === StraightDirection.EAST_WEST
         || this.getDirection() === StraightDirection.SOUTH_NORTH){
            stretch = this.getFirstStretch();
        } else {
            stretch = this.getLastStretch();
        }

        return stretch;
    }

    public getStretches(): Array<Stretch> {
        return this.stretches;
    }

    public getWidth(): number {
        return this.stretches[0].getWidth();
    }

    public getLength(): number {
        if (this.countStretches() === 0) {
            return 0;
        } else {
            return this.stretches[0].getLength() * this.countStretches();
        }
    }

    public countStretches(): number {
        return this.stretches.length;
    }

    public addStretch(stretch : Stretch) {
        this.stretches.push(stretch);
    }

    public getDirection(): StraightDirection {
        return this.direction;
    }

    public getFirstStretch(): Stretch {
        if (this.countStretches() > 0) {
            return this.stretches[0];
        }
    }

    public getLastStretch(): Stretch {
        if (this.countStretches() > 0) {
            return this.stretches[this.countStretches() - 1];
        }
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let shapes : Array<ShapeComponent> = new Array<ShapeComponent>();
        for (let stretch of this.stretches) {
            shapes = shapes.concat(stretch.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }
        return shapes;
    }

    public getId(): number {
        return this.id;
    }
}