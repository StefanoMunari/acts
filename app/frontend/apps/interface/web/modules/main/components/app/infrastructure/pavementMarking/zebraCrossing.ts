import {PavementMarking} from './pavementMarking';
import {ShapeComponent} from '../../draw/shape.component';
import {Orientation} from "../../shared/orientation";
import {Rect} from "../../draw/rect";
import {RectComponent} from "../../draw/rect.component";
import {StraightDirection} from "../../shared/straightDirection";
import {Color} from "../../shared/color";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export class ZebraCrossing extends PavementMarking {

    static readonly STRIPE_LENGTH : number = 6;
    static readonly FIRST_SPACE : number = 6;
    static readonly SPACE_BETWEEN_STRIPES : number = 7;

    private id: number;
    private x: number;
    private y: number;
    private width: number;
    private length: number;
    private orientation: Orientation;
    private stroke: Color;
    private laneDirection: StraightDirection;

    public setLaneDirection(laneDirection: StraightDirection) {
        this.laneDirection = laneDirection;
    }

    public getLaneDirection(): StraightDirection {
        return this.laneDirection;
    }

    public setStroke(stroke: Color) {
        this.stroke = stroke;
    }

    public getStroke(): Color {
        return this.stroke;
    }

    public getWidth(): number {
        return this.width;
    }

    public getLength(): number {
        return this.length;
    }

    public getX(): number {
        return this.x;
    }

    public getY(): number {
        return this.y;
    }

    public setX(x: number): void {
        this.x = x;
    }

    public setY(y: number): void {
        this.y = y;
    }

    public setWidth(width : number): void {
        this.width = width;
    }

    public getStripeWidth(): number {
        return this.getWidth();
    }

    public setLength(length : number): void {
        this.length = length;
    }

    public setId(id: number) {
        this.id = id;
    }

    public getId(): number {
        return this.id;
    }

    public getOrientation(): Orientation {
        return this.orientation;
    }

    public setOrientation(orientation: Orientation): void {
        this.orientation = orientation;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {

        let shapes: Array<ShapeComponent> = [];
        let stripeX: number = this.getX();
        let stripeY: number = this.getY();
        let stripeWidth: number = 0;
        let stripeHeight: number = 0;

        let stripesBefore: number = ZebraCrossing.FIRST_SPACE;

        while (stripesBefore < this.getLength()) {

            const newLength = stripesBefore + ZebraCrossing.STRIPE_LENGTH;

            if (this.getOrientation() === Orientation.HORIZONTAL) {

                switch (this.laneDirection) {
                    case StraightDirection.NORTH_SOUTH:
                        stripeX = this.getX() + stripesBefore;
                        if (newLength <= this.getLength()) {
                            stripeWidth = ZebraCrossing.STRIPE_LENGTH;
                        } else {
                            stripeWidth = this.getLength() - stripesBefore;
                        }
                        break;
                    case StraightDirection.SOUTH_NORTH:
                        stripeX = this.getX();
                        if (newLength <= this.getLength()) {
                            stripeX = stripeX + this.getLength() - stripesBefore - ZebraCrossing.STRIPE_LENGTH;
                            stripeWidth = ZebraCrossing.STRIPE_LENGTH;
                        } else {
                            stripeWidth = this.getLength() - stripesBefore;
                        }
                }
                stripeHeight = this.getStripeWidth();

            } else { //the crossing orientation is vertical

                switch (this.laneDirection) {
                    case StraightDirection.EAST_WEST:
                        stripeY = this.getY() + stripesBefore;
                        if (newLength <= this.getLength()) {
                            stripeHeight = ZebraCrossing.STRIPE_LENGTH;
                        } else {
                            stripeHeight = this.getLength() - stripesBefore;
                        }
                        break;
                    case StraightDirection.WEST_EAST:
                        stripeY = this.getY();
                        if (newLength <= this.getLength()) {
                            stripeY = stripeY + this.getLength() - stripesBefore - ZebraCrossing.STRIPE_LENGTH;
                            stripeHeight = ZebraCrossing.STRIPE_LENGTH;
                        } else {
                            stripeHeight = this.getLength() - stripesBefore;
                        }
                }
                stripeWidth = this.getStripeWidth();
            }

            let rect = new Rect();

            rect.setX(stripeX);
            rect.setY(stripeY);
            rect.setWidth(stripeWidth);
            rect.setHeight(stripeHeight);
            rect.setFill(this.getStroke());
            rect.setStrokeWidth(0);
            rect.setMinX(minX);
            rect.setMinY(minY);
            rect.setMaxXPlusWidth(maxXPlusWidth);
            rect.setMaxYPlusHeight(maxYPlusHeight);
            rect.addClass("stripe");
            shapes.push(RectComponent.create(rect, drawer));

            stripesBefore = stripesBefore + ZebraCrossing.STRIPE_LENGTH + ZebraCrossing.SPACE_BETWEEN_STRIPES;
        }
        return shapes;
    }
}