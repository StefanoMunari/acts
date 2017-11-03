import {Point} from "./point";
import {Shape} from './shape';
import {Color} from "../shared/color";
import {ColorUtils} from "../shared/colorUtils";

export class Triangle extends Shape {
    private p1: Point;
    private p2: Point;
    private p3: Point;
    private fill: string;
    private strokeWidth: number;
    private minX: number;
    private minY: number;
    private maxXPlusWidth: number;
    private maxYPlusHeight: number;

    constructor() {
        super();
        this.fill = 'none';
        this.strokeWidth = 0;
    }

    public setP1(x: number, y: number) {
        this.p1 = new Point(x, y);
    }

    public setP2(x: number, y: number) {
        this.p2 = new Point(x, y);
    }

    public setP3(x: number, y: number) {
        this.p3 = new Point(x, y);
    }


    public getP1(): Point {
        return this.p1;
    }

    public getP2(): Point {
        return this.p2;
    }

    public getP3(): Point {
        return this.p3;
    }

    public setFill(fill: Color) {
      this.fill = ColorUtils.getRgbFromColor(fill);
    }

    public getFill(): string {
        return this.fill;
    }

    public setStrokeWidth(strokeWidth: number) {
      this.strokeWidth = strokeWidth;
    }

    public getMinX(): number {
        return this.minX;
    }

    public getMinY(): number {
        return this.minY;
    }

    public getMaxXPlusWidth(): number {
        return this.maxXPlusWidth;
    }

    public getMaxYPlusHeight(): number {
        return this.maxYPlusHeight;
    }

    public setMinX(minX: number) {
        this.minX = minX;
    }

    public setMinY(minY: number) {
        this.minY = minY;
    }

    public setMaxXPlusWidth(maxXPlusWidth: number) {
        this.maxXPlusWidth = maxXPlusWidth;
    }

    public setMaxYPlusHeight(maxYPlusHeight: number) {
        this.maxYPlusHeight = maxYPlusHeight;
    }

    public getPoints(): string {
        return this.getP1().getX() + "," + this.getP1().getY() + " " +
            this.getP2().getX() + "," + this.getP2().getY() + " " +
            this.getP3().getX() + "," + this.getP3().getY();
    }
}