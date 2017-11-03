import { Injectable } from '@angular/core';
import { ShapeComponent } from './shape.component';

@Injectable()
export class DrawService {
    private shapes: ShapeComponent[];

    public addShape(shape: ShapeComponent) {
        this.shapes.push(shape);
    }

    public getShapes(): ShapeComponent[] {
        return this.shapes;
    }
}