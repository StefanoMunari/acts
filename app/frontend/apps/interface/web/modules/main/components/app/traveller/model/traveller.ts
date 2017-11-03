import {TravellerMotion} from "../travellerMotion/travellerMotion";
import {Color} from "../../shared/color";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {CircleComponent} from "../../draw/circle.component";
import {Treadable} from "../../infrastructure/treadable";
import {TravellerType} from "../travellerType";
import {Circle} from "../../draw/circle";

export abstract class Traveller {
    private id: string;
    private x: number = null;
    private y: number = null;
    private pursued: boolean = false;
    private infrastructureVisitor : TravellerMotion;

    constructor(id: string) {
        this.id = id;
    }

    public pursue() {
        this.pursued = true;
    }

    public abstract getColor(): Color;

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let circle = new Circle();
        circle.setId('traveller' + this.getId());
        circle.setRadius(5);
        circle.setCx(this.getX());
        circle.setCy(this.getY());
        if (!!this.pursued) {
            circle.setFill(Color.RED);
            circle.setStroke(Color.RED);
            circle.addClass("pursued");
        } else {
            circle.setFill(this.getColor());
            circle.setStroke(this.getColor());
        }
        circle.setMinX(minX);
        circle.setMinY(minY);
        circle.setMaxXPlusWidth(maxXPlusWidth);
        circle.setMaxYPlusHeight(maxYPlusHeight);
        return [CircleComponent.create(circle, drawer)];
    }

    getId() : string {
        return this.id;
    }

    public getX() : number {
        return this.x;
    }

    public getY() : number {
        return this.y;
    }

    public setX(x : number) {
        this.x = x;
    }

    public setY(y : number) {
        this.y = y;
    }

    public setInfrastructureVisitor(infrastructureVisitor : TravellerMotion) {
        this.infrastructureVisitor = infrastructureVisitor;
    }

    public treadInfrastructure(infrastructure : Treadable) {
        infrastructure.tread(this, this.infrastructureVisitor);
    }

    public moveXTo(x : number, drawer: DrawerAdapter) {
        if (this.x !== null) {
            let traveller = drawer.getShapeById('traveller' + this.getId());
            return drawer.moveCx(this.x, x, traveller);
        }
        this.x = x;
        return null;
    }

    public moveYTo(y: number, drawer: DrawerAdapter) {
        if (this.y !== null) {
            let traveller = drawer.getShapeById('traveller' + this.getId());
            drawer.moveCy(this.y, y, traveller);
        }
        this.y = y;
        return null;
    }

    public abstract getType(): TravellerType;
}