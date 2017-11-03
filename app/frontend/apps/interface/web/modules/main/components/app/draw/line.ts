import { Shape } from './shape';

export class Line extends Shape {
    public x1: number;
    public x2: number;
    public y1: number;
    public y2: number;
    public stroke: string;
    public strokeWidth: number;
    public strokeDashArray: string;
    private minX: number;
    private minY: number;
    private maxXPlusWidth: number;
    private maxYPlusHeight: number;

    constructor() {
        super();
        this.stroke = 'black';
        this.strokeWidth = 0;
        this.strokeDashArray = 'none';
    }

    setX1(x1: number): void {
        this.x1 = x1;
    }

    setX2(x2: number): void {
        this.x2 = x2;
    }

    setY1(y1: number): void {
        this.y1 = y1;
    }

    setY2(y2: number): void {
        this.y2 = y2;
    }

    public getX1(): number {
        return this.x1;
    }

    public getX2(): number {
        return this.x2;
    }

    public getY1(): number {
        return this.y1;
    }

    public getY2(): number {
        return this.y2;
    }

    setStroke(stroke: string): void {
        this.stroke = stroke;
    }

    public getStroke(): string {
        return this.stroke;
    }

    setStrokeWidth(strokeWidth: number): void {
        this.strokeWidth = strokeWidth;
    }

    public getStrokeWidth(): number {
        return this.strokeWidth;
    }

    setStrokeDashArray(strokeDashArray: string): void {
        this.strokeDashArray = strokeDashArray;
    }

    public getStrokeDashArray(): string {
        return this.strokeDashArray;
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