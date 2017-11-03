import {Stretch} from "./stretch";
import {Color} from "../../shared/color";
import {Vehicle} from "../../traveller/model/vehicle";
import {VehicleMotion} from "../../traveller/travellerMotion/vehicleMotion";
import {BusStop} from "../pavementMarking/busStop";
import {ShapeComponent} from "../../draw/shape.component";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export class RoadwayStretch extends Stretch {
    private busStop: BusStop = null;

    public setX(x : number) {
        super.setX(x);
        if (!!this.busStop) {
            this.busStop.setX(x);
        }
    }

    public setY(y : number) {
        super.setY(y);
        if (!!this.busStop) {
            this.busStop.setY(y);
        }
    }

    public withBusStop() {
        this.busStop = new BusStop();
        this.busStop.setDirection(this.getLane().getDirection());
        this.busStop.setWidth(this.getWidth());
        this.busStop.setHeight(this.getLength());
    }

    public static getStandardColor(): Color {
        return Color.BLACK;
    }

    public getColor(): Color {
        return RoadwayStretch.getStandardColor();
    }

    public tread(traveller : Vehicle, visitor : VehicleMotion) {
        visitor.moveToStretch(this, traveller);
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {

        let shapes = super.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer);

        if (!!this.busStop) {
            shapes = shapes.concat(this.busStop.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        return shapes;
    }
}