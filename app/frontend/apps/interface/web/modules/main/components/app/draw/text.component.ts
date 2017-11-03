import { Component, Input } from '@angular/core';
import { ShapeComponent } from './shape.component';
import {Text} from './text';
import {DrawerAdapter} from "./drawerAdapter";

@Component({
    selector: '[text-component]',
    template: ''
})
export class TextComponent implements ShapeComponent {
    @Input() shape: Text;

    public getComponent() {
        return TextComponent;
    }

    public static create(shape: Text, drawer: DrawerAdapter, group?) {
        let component: TextComponent = new TextComponent();
        drawer.createSVG("canvas", shape.getViewBox());

        let text = drawer.drawText(shape.getText(), group);

        for (let c of shape.getClassesArray()) {
            drawer.addClass(c, text);
        }

        drawer.setX(shape.getX(), text);
        drawer.setY(shape.getY(), text);

        if (shape.getRotation() !== 0) {
            drawer.rotate(shape.getRotation(), shape.getX() + shape.getWidth() / 2,
                           shape.getY() + shape.getHeight() / 2, text);
        }
        drawer.setFill(shape.getFill(), text);
        if (shape.getStroke() === 'none') {
            drawer.setStroke(shape.getFill(), text);
        } else {
            drawer.setStroke(shape.getStroke(), text);
        }
        drawer.setStrokeWidth(shape.getStrokeWidth(), text);
        //component.shape = shape;
        return component;
    }

    public getViewBox(): string {
        return this.shape.getMinX() + " " + this.shape.getMinY() + " "
            + this.shape.getMaxXPlusWidth() + " " + this.shape.getMaxYPlusHeight();
    }
}