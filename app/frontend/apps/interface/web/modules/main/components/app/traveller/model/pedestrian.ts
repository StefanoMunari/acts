import {Component} from '@angular/core';
import {Traveller} from './traveller';
import {Person} from "../model/person";
import {Color} from "../../shared/color";
import {TravellerType} from "../travellerType";
import {TravellerRegistry} from "../traveller.registry";

@Component({
    selector: 'pedestrian'
})
export class Pedestrian extends Traveller implements Person {

    constructor(id : string) {
        super(id);
    }

    public getColor(): Color {
        return TravellerRegistry.PEDESTRIAN_COLOR;
    }

    public getType(): TravellerType {
        return TravellerType.PEDESTRIAN;
    }
}