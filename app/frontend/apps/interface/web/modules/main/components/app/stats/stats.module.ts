import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {DistrictStatsComponent} from "./district/district-stats.component";
import {InfrastructureModule} from "../infrastructure/infrastructure.module";
import {FacilityStatsComponent} from "./facility/facility-stats.component";
import {FacilitiesStatsComponent} from "./facility/facilities-stats.component";
import {MaterialModule} from "../material/material.module";

@NgModule({
    imports: [
        InfrastructureModule,
        MaterialModule,
        CommonModule
    ],
    declarations: [
        DistrictStatsComponent,
        FacilityStatsComponent,
        FacilitiesStatsComponent
    ],
    exports: [
        DistrictStatsComponent,
        FacilitiesStatsComponent
    ],
    entryComponents: [
        FacilityStatsComponent
    ]
})
export class StatsModule{}