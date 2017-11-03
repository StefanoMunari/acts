import { NgModule } from '@angular/core';
import { DistrictComponent } from './district.component';
import { InfrastructureModule } from '../infrastructure/infrastructure.module';
import { DrawModule } from '../draw/draw.module';
import {TravellerModule} from "../traveller/traveller.module";
import {MaterialModule} from "../material/material.module";
import {WidgetsModule} from "../widgets/widgets.module";
import {StatsModule} from "../stats/stats.module";
import {RouterModule} from "@angular/router";
import {DistrictTravellersComponent} from "./travellers/district-travellers.component";
import {CommonModule} from "@angular/common";
import {TerminationDialog} from "./termination/termination-dialog";

@NgModule({
    declarations: [
        DistrictComponent,
        DistrictTravellersComponent,
        TerminationDialog
    ],
    imports: [
        InfrastructureModule,
        TravellerModule,
        DrawModule,
        MaterialModule,
        StatsModule,
        RouterModule,
        CommonModule,
        WidgetsModule
    ],
    entryComponents: [
        TerminationDialog
    ]
})
export class DistrictModule {}
