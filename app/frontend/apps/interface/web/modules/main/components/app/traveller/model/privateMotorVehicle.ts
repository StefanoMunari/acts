import {Component} from '@angular/core';
import {Vehicle} from "./vehicle";
import {Color} from "../../shared/color";
import {TravellerType} from "../travellerType";
import {TravellerRegistry} from "../traveller.registry";

@Component({
    selector: 'private-motor-vehicle'
})
export class PrivateMotorVehicle extends Vehicle {

    constructor(id : string) {
        super(id);
    }

    public getColor(): Color {
        return TravellerRegistry.CAR_COLOR;
    }

    public getType(): TravellerType {
        return TravellerType.PRIVATE_MOTOR_VEHICLE;
    }
}