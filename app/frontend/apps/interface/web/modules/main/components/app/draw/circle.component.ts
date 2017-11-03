import {Component, Input} from '@angular/core';
import {Circle} from './circle';
import {ShapeComponent} from './shape.component';
import {DrawerAdapter} from "./drawerAdapter";

@Component({
    selector: '[circle-component]',
    template: ''/*<svg [attr.class]="shape.getClasses()" [attr.viewBox]="getViewBox()"\
               preserveAspectRatio="xMidYMid meet">\
               <svg:circle [attr.id]="shape.id" [attr.r]="shape.radius" [attr.cx]="shape.cx" \
               [attr.cy]="shape.cy" [attr.fill]="shape.fill" \
               [attr.stroke]="shape.stroke" \
               [attr.stroke-width]="shape.strokeWidth">\
               </svg:circle>\
               </svg>'*/,
    /*styles: ['svg.shape {\
                position: absolute;\
                width: 100%;\
                height: 100%;\
                overflow: visible\
            }']*/
})
export class CircleComponent implements ShapeComponent {
    @Input() shape: Circle;

    public getComponent() {
        return CircleComponent;
    }

    public static create(shape: Circle, drawer: DrawerAdapter, group?) {
        let component: CircleComponent = new CircleComponent();
        drawer.createSVG("canvas", shape.getViewBox());

        let circle = drawer.drawCircle(group);

        for (let c of shape.getClassesArray()) {
            drawer.addClass(c, circle);
        }

        drawer.setId(shape.getId(), circle);
        drawer.setRadius(shape.getRadius(), circle);
        drawer.setCx(shape.getCx(), circle);
        drawer.setCy(shape.getCy(), circle);
        drawer.setFill(shape.getFill(), circle);
        drawer.setStroke(shape.getStroke(), circle);
        drawer.setStrokeWidth(shape.getStrokeWidth(), circle);
        //component.shape = shape;
        return component;
    }

    public getViewBox(): string {
        return this.shape.getMinX() + " " + this.shape.getMinY() + " "
            + this.shape.getMaxXPlusWidth() + " " + this.shape.getMaxYPlusHeight();
    }
}