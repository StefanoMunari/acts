import {NgModule} from '@angular/core';
import {DrawModule} from '../draw/draw.module';
import {InfrastructureDrawer} from "./infrastructure.drawer";
import {InfrastructureRegistry} from "./infrastructure.registry";

@NgModule({
    imports: [
        DrawModule
    ],
    declarations: [
        InfrastructureDrawer
    ],
    exports: [
        InfrastructureDrawer
    ],
    providers: [
        InfrastructureRegistry
    ]

})
export class InfrastructureModule {}