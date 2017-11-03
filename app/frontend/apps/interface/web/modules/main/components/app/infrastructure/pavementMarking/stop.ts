import { Line } from '../../draw/line';
import { PavementMarking } from './pavementMarking';
import { Street } from '../street/street';
import { ShapeComponent } from '../../draw/shape.component';
import { LineComponent } from '../../draw/line.component';
import {Orientation} from "../../shared/orientation";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export class Stop extends PavementMarking {
    private x1: number;
    private y1: number;
    private x2: number;
    private y2: number;

    constructor(x: number, y: number, upRightDirection: boolean,
      orientation: Orientation, roadwayWidth: number) {
        super(/*street, parentId*/);
        this.x1 = x;
        this.y1 = y;
        if (orientation === Orientation.VERTICAL) {
            if (upRightDirection) {
                this.y1 -= 5;
            }
            else
                this.y1 += 5;
            this.x2 = this.x1 + roadwayWidth / 2;
            this.y2 = this.y1;
        }
        else {
            if (upRightDirection)
                this.x1 += 5;
            else
                this.x1 -= 5;
            this.x2 = this.x1;
            this.y2 = this.y1 + roadwayWidth / 2;
        }
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let line = new Line();
        line.setX1(this.x1);
        line.setX2(this.x2);
        line.setY1(this.y1);
        line.setY2(this.y2);
        line.setStroke('rgb(255,255,255)');
        line.setStrokeWidth(10);
        line.setMinX(minX);
        line.setMinY(minY);
        line.setMaxXPlusWidth(maxXPlusWidth);
        line.setMaxYPlusHeight(maxYPlusHeight);
        return [LineComponent.create(line, drawer)];
    }
}