import {Injectable} from "@angular/core";
import {Intersection} from "../../infrastructure/intersection/intersection";
import {TravellerMotion} from "./travellerMotion";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {Traveller} from "../model/traveller";

@Injectable()
export class BicycleMotion extends TravellerMotion {

    constructor(private drawer: DrawerAdapter) {
        super();
    }

    public moveToIntersection(intersection : Intersection, traveller : Traveller) {}

    protected getDrawer(): DrawerAdapter {
        return this.drawer;
    }
}