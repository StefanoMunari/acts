import { Component, Input } from '@angular/core';
import { Rect } from './rect';
import { ShapeComponent } from './shape.component';
import {DrawerAdapter} from "./drawerAdapter";

@Component({
    selector: '[rect-component]',
    template: ''//,
                /*'<svg [attr.class]="shape.getClasses()" [attr.viewBox]="getViewBox()"\
               preserveAspectRatio="xMidYMid meet">\
               <svg:rect [attr.id]="shape.id" [attr.x]="shape.x" [attr.y]="shape.y" \
               [attr.width]="shape.width" [attr.height]="shape.height" \
               [attr.fill]="shape.fill"\
               [attr.stroke]="shape.fill">\
               </svg:rect>\
               </svg>',*/
    /*styles: ['svg.shape {\
                position: absolute;\
                width: 100%;\
                height: 100%;\
                overflow: visible;\
                facility{z-index:100000;}\
            }\
            svg.facility{\
                z-index:100000;\
            }']*/
})
export class RectComponent implements ShapeComponent {
    @Input() shape: Rect;

    public getComponent() {
        return RectComponent;
    }

    public static create(shape: Rect, drawer: DrawerAdapter, group?) {
        let component: RectComponent = new RectComponent();
        drawer.createSVG("canvas", shape.getViewBox());

        let rect = drawer.drawRect(shape.getWidth(), shape.getHeight(), group);

        for (let c of shape.getClassesArray()) {
            drawer.addClass(c, rect);
        }

        drawer.setId(shape.getId(), rect);
        drawer.setX(shape.getX(), rect);
        drawer.setY(shape.getY(), rect);
        drawer.setFill(shape.getFill(), rect);
        if (shape.getStroke() === 'none') {
            drawer.setStroke(shape.getFill(), rect);
        } else {
            drawer.setStroke(shape.getStroke(), rect);
        }
        drawer.setStrokeWidth(shape.getStrokeWidth(), rect);
        //component.shape = shape;
        return component;
    }

    public getViewBox(): string {
        return this.shape.getMinX() + " " + this.shape.getMinY() + " "
            + this.shape.getMaxXPlusWidth() + " " + this.shape.getMaxYPlusHeight();
    }
}