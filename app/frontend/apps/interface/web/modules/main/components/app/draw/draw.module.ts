import { NgModule } from '@angular/core';
import { LineComponent } from './line.component';
import { RectComponent } from './rect.component';
import { CircleComponent } from './circle.component';
import { CanvasComponent } from './canvas.component';
import { DrawService } from './draw.service';
import {TriangleComponent} from "./triangle.component";

@NgModule({
    declarations: [
        CanvasComponent,
        LineComponent,
        RectComponent,
        CircleComponent,
        TriangleComponent,
    ],
    providers: [
        DrawService
    ],
    entryComponents: [
        LineComponent,
        RectComponent,
        CircleComponent,
        TriangleComponent
    ],
    exports: [
        CanvasComponent,
        LineComponent,
        RectComponent,
        CircleComponent,
        TriangleComponent
    ]
})
export class DrawModule {}