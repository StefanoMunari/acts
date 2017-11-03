import {NgModule} from "@angular/core";
import {MaterialModule} from "../material/material.module";
import {CommonModule} from "@angular/common";
import {RouterModule} from "@angular/router";
import {CitiesComponent} from "./cities.component";
import {CitiesService} from "./cities.service";

@NgModule({
    imports: [
        MaterialModule,
        CommonModule,
        RouterModule
    ],
    declarations: [
        CitiesComponent
    ],
    providers: [
        CitiesService
    ]
})
export class CitiesModule {}
