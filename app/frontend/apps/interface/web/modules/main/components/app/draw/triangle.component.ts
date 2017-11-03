import { Component, Input } from '@angular/core';
import { ShapeComponent } from './shape.component';
import {Triangle} from "./triangle";
import {DrawerAdapter} from "./drawerAdapter";

@Component({
    selector: '[triangle-component]',
    template: ''/*<svg [attr.class]="shape.getClasses()" [attr.viewBox]="getViewBox()"\
               preserveAspectRatio="xMidYMid meet" xmlns:svg="http://www.w3.org/1999/html">\
               <svg:polygon [attr.id]="shape.id" [attr.points]="getPoints()" \
               [attr.fill]="shape.fill" [attr.stroke]="shape.fill">\
               </svg:polygon>\
               </svg>',
    styles: ['svg.shape {\
                position: absolute;\
                width: 100%;\
                height: 100%;\
                overflow: visible;\
            }\
            svg.facility{\
                z-index:100000;\
            }']*/
})
export class TriangleComponent implements ShapeComponent {
    @Input() shape: Triangle;

    public getComponent() {
        return TriangleComponent;
    }

    public static create(triangle: Triangle, drawer: DrawerAdapter, group?) {
        let component: TriangleComponent = new TriangleComponent();
        drawer.createSVG("canvas", triangle.getViewBox());

        let t = drawer.drawPolygon(triangle.getPoints(), group);

        for (let c of triangle.getClassesArray()) {
            drawer.addClass(c, t);
        }

        drawer.setId(triangle.getId(), t);
        drawer.setFill(triangle.getFill(), t);
        drawer.setStroke(triangle.getFill(), t);
        //component.shape = shape;
        return component;
    }

    public getViewBox(): string {
        return this.shape.getMinX() + " " + this.shape.getMinY() + " "
            + this.shape.getMaxXPlusWidth() + " " + this.shape.getMaxYPlusHeight();
    }

    public getPoints(): string {
        return this.shape.getP1().getX() + "," + this.shape.getP1().getY() + " " +
                this.shape.getP2().getX() + "," + this.shape.getP2().getY() + " " +
                this.shape.getP3().getX() + "," + this.shape.getP3().getY();
    }
}