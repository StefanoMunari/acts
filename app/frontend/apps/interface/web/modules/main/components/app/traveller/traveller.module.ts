import {NgModule} from '@angular/core';
import {TravellerDrawer} from "./traveller.drawer";
import {DistrictChannel} from "./channels/district.channel";
import {BicycleMotion} from "./travellerMotion/bicycleMotion";
import {PedestrianMotion} from "./travellerMotion/pedestrianMotion";
import {VehicleMotion} from './travellerMotion/vehicleMotion';
import {DrawerAdapter} from "../draw/drawerAdapter";
import {TravellerRegistry} from "./traveller.registry";
import {TravellerLoader} from "./traveller.loader";
import {InfrastructureLoader} from "../infrastructure/infrastructure.loader";

@NgModule({
    declarations: [TravellerDrawer],
    exports: [TravellerDrawer],
    providers: [
        DistrictChannel,
        DrawerAdapter,
        PedestrianMotion,
        VehicleMotion,
        BicycleMotion,
        TravellerRegistry,
        TravellerLoader
    ]
})
export class TravellerModule{}