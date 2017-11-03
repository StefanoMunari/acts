import {Shape} from './shape';
import {Color} from "../shared/color";
import {ColorUtils} from "../shared/colorUtils";

export class Rect extends Shape {
    private x: number;
    private y: number;
    private width: number;
    private height: number;
    private fill: string;
    private stroke: string;
    private strokeWidth: number;
    private minX: number;
    private minY: number;
    private maxXPlusWidth: number;
    private maxYPlusHeight: number;

    constructor() {
        super();
        this.width = 0;
        this.height = 0;
        this.fill = 'none';
        this.stroke = 'none';
        this.strokeWidth = 1;
    }

    public setX(x: number) {
      this.x = x;
    }

    public setY(y: number) {
      this.y = y;
    }

    public setWidth(width: number) {
        this.width = width;
    }

    public setHeight(height: number) {
        if (height < 0) {
            throw new Error("Rect.setHeight(" + height + ")");
        }

        if (height === 0) {
            console.log("WARNING: Rect.setHeight(0)");
        }

        this.height = height;
    }

    public setFill(fill: Color) {
      this.fill = ColorUtils.getRgbFromColor(fill);
    }

    public setStroke(stroke: Color) {
        this.stroke = ColorUtils.getRgbFromColor(stroke);
    }

    public setStrokeWidth(strokeWidth: number) {
        this.strokeWidth = strokeWidth;
    }

    public getX(): number {
        return this.x;
    }

    public getY(): number {
        return this.y;
    }

    public getWidth(): number {
        return this.width;
    }

    public getHeight(): number {
        return this.height;
    }

    public getFill(): string {
        return this.fill;
    }

    public getStroke(): string {
        return this.stroke;
    }

    public getStrokeWidth(): number {
        return this.strokeWidth;
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
}