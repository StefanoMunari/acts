import { Component, Input } from '@angular/core';
import { Line } from './line';
import { ShapeComponent } from './shape.component'
import {DrawerAdapter} from "./drawerAdapter";

@Component({
  selector: '[line-component]',
  template: ''/*<svg [attr.class]="shape.getClasses()" [attr.viewBox]="getViewBox()"\
             preserveAspectRatio="xMidYMid meet">\
             <svg:line [attr.x1]="shape.x1" [attr.x2]="shape.x2" \
             [attr.y1]="shape.y1" [attr.y2]="shape.y2" \
             [attr.stroke]="shape.stroke" \
             [attr.stroke-width]="shape.strokeWidth" \
             [attr.stroke-dash-array]="shape.strokeDashArray" >\
             </svg:line>\
             </svg>',
    styles: ['svg.shape {\
                position: absolute;\
                width: 100%;\
                height: 100%;\
                overflow: visible\
            }']*/
})
export class LineComponent implements ShapeComponent {
    @Input() shape: Line;

    getComponent() {
        return LineComponent;
    }

    public static create(line: Line, drawer: DrawerAdapter, group?) {
        let component: LineComponent = new LineComponent();
        drawer.createSVG("canvas", line.getViewBox());

        let l = drawer.drawLine(line.getX1(), line.getY1(), line.getX2(), line.getY2(), group);

        for (let c of line.getClassesArray()) {
            drawer.addClass(c, l);
        }

        drawer.setId(line.getId(), l);
        drawer.setStroke(line.getStroke(), l);
        drawer.setStrokeWidth(line.getStrokeWidth(), l);
        drawer.setStrokeDashArray(line.getStrokeDashArray(), l);
        //component.shape = shape;
        return component;
    }


    public getViewBox(): string {
        return this.shape.getMinX() + " " + this.shape.getMinY() + " "
            + this.shape.getMaxXPlusWidth() + " " + this.shape.getMaxYPlusHeight();
    }
}