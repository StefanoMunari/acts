import {NgModule} from "@angular/core";
import {PursuitComponent} from "./pursuit.component";
import {InfrastructureModule} from "../infrastructure/infrastructure.module";
import {TravellerModule} from "../traveller/traveller.module";
import {DrawModule} from "../draw/draw.module";
import {MaterialModule} from "../material/material.module";
import {RouterModule} from "@angular/router";

@NgModule({
    imports: [
        InfrastructureModule,
        TravellerModule,
        DrawModule,
        MaterialModule,
        RouterModule
    ],
    declarations: [PursuitComponent],
    exports: []
})
export class PursuitModule {}