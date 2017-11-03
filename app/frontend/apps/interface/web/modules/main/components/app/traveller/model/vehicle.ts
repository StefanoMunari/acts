import {Component} from '@angular/core';
import {Traveller} from './traveller';
import {Color} from "../../shared/color";
import {TravellerType} from "../travellerType";

@Component({
    selector: 'vehicle'
})
export class Vehicle extends Traveller {

    constructor(id: string) {
        super(id);
    }

    public getColor(): Color {
        return Color.WHITE;
    }

    public getType(): TravellerType {
        return TravellerType.VEHICLE;
    }
}