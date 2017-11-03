import {Shape} from './shape';
import {ColorUtils} from "../shared/colorUtils";
import {Color} from "../shared/color";

export class Circle extends Shape {
    private radius: number;
    private cx: number;
    private cy: number;
    private fill: string;
    private stroke: string;
    private strokeWidth: number;
    private minX: number;
    private minY: number;
    private maxXPlusWidth: number;
    private maxYPlusHeight: number;

    constructor() {
        super();
        this.radius = 0;
        this.cx = 0;
        this.cy = 0;
        this.fill = 'none';
        this.stroke = 'black';
        this.strokeWidth = 0;
    }

    getRadius(): number {
        return this.radius;
    }

    getCx(): number {
        return this.cx;
    }

    getCy(): number {
        return this.cy;
    }

    getFill(): string {
        return this.fill;
    }

    getStroke(): string {
        return this.stroke;
    }

    getStrokeWidth(): number {
        return this.strokeWidth;
    }

    setRadius(radius: number): void {
        this.radius = radius;
    }

    setCx(cx: number): void {
        this.cx = cx;
    }

    setCy(cy: number): void {
        this.cy = cy;
    }

    setFill(fill: Color): void {
      this.fill = ColorUtils.getRgbFromColor(fill);
    }

    setStroke(stroke: Color): void {
        this.stroke = ColorUtils.getRgbFromColor(stroke);
    }

    setStrokeWidth(strokeWidth: number): void {
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
}