import {NgModule} from "@angular/core";
import {MaterialModule} from "../material/material.module";
import {ColorsLegendComponent} from "./legend/colors-legend.component";
import {CommonModule} from "@angular/common";

@NgModule({
    declarations: [
        ColorsLegendComponent
    ],
    imports: [
        MaterialModule,
        CommonModule
    ],
    exports: [
        ColorsLegendComponent
    ]
})
export class WidgetsModule {}