import {Orientation} from "../../shared/orientation";
import {ZebraCrossing} from "./zebraCrossing";

export class BikeCrossing extends ZebraCrossing {
    public getX(): number {
        if (this.getOrientation() === Orientation.VERTICAL) {
            return super.getX() + super.getWidth() / 2 - this.getStripeWidth() / 2;
        }
        return super.getX();
    }

    public getY(): number {
        if (this.getOrientation() === Orientation.HORIZONTAL) {
            return super.getY() + super.getWidth() / 2 - this.getStripeWidth() / 2;
        }
        return super.getY();
    }

    public getStripeWidth(): number {
        return super.getWidth() / 3;
    }
}