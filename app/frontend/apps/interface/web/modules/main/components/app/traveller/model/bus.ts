import {Component} from '@angular/core';
import {Color} from "../../shared/color";
import {TravellerType} from "../travellerType";
import {Vehicle} from "./vehicle";
import {TravellerRegistry} from "../traveller.registry";

@Component({
    selector: 'bus'
})
export class Bus extends Vehicle {

    constructor(id : string) {
        super(id);
    }

    public getColor(): Color {
        return TravellerRegistry.BUS_COLOR;
    }

    public getType(): TravellerType {
        return TravellerType.BUS;
    }
}