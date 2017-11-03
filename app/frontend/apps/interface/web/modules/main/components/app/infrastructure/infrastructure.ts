import {ShapeComponent} from '../draw/shape.component';
import {DrawerAdapter} from "../draw/drawerAdapter";

export abstract class Infrastructure {
    public abstract getShapes(minX: number, minY: number, maxXPlusWidth: number,
                              maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent>;
}