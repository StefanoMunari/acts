import {Color} from "../../shared/color";
import {ShapeComponent} from "../../draw/shape.component";
import {Rect} from "../../draw/rect";
import {Orientation} from "../../shared/orientation";
import {RectComponent} from "../../draw/rect.component";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {Infrastructure} from "../infrastructure";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";
import {Circle} from "../../draw/circle";
import {CircleComponent} from "../../draw/circle.component";

require("./trafficLight.css");

export class TrafficLight implements Infrastructure {

    private x: number;
    private y: number;
    private width: number;
    private direction: StraightDirection;

    constructor(private id: number) {}

    public setX(x: number) {
        this.x = x;
    }

    public getX(): number {
        return this.x;
    }

    public setY(y: number) {
        this.y = y;
    }

    public getY(): number {
        return this.y;
    }

    public setDirection(direction: StraightDirection) {
        this.direction = direction;
    }

    public getDirection(): StraightDirection {
        return this.direction;
    }

    public setWidth(width: number) {
        this.width = width;
    }

    public getWidth(): number {
        return this.width;
    }

    public getLightsContainerWidth(): number {
        return this.getWidth() / 2
    }

    public getLightRadius(): number {
        return (this.getLightsContainerWidth() / 2) - 1;
    }

    public getLength(): number {
        return this.getLightsContainerWidth() * 3;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let lightContainer: Rect = new Rect();
        lightContainer.addClass("lightContainer");
        let greenLight: Circle = new Circle();
        greenLight.addClass("greenLight");
        let yellowLight: Circle = new Circle();
        yellowLight.addClass("yellowLight");
        let redLight: Circle = new Circle();
        redLight.addClass("redLight");

        if (DirectionUtils.getOrientationByDirection(this.direction) === Orientation.HORIZONTAL) {
            lightContainer.setX(this.getX());
            lightContainer.setY(this.getY() + this.getWidth() / 2 - this.getLightsContainerWidth() / 2);
            lightContainer.setWidth(this.getLength());
            lightContainer.setHeight(this.getLightsContainerWidth());
            greenLight.setCy(this.getY() + this.getWidth() / 2);
            yellowLight.setCx(this.getX() + this.getLength() / 2);
            yellowLight.setCy(this.getY() + this.getWidth() / 2);
            redLight.setCy(this.getY() + this.getWidth() / 2);
            switch (this.direction) {
                case StraightDirection.EAST_WEST:
                    greenLight.setCx(this.getX() + this.getLength() * (5/6));
                    redLight.setCx(this.getX() + this.getLength() / 6);
                    break;
                case StraightDirection.WEST_EAST:
                    redLight.setCx(this.getX() + this.getLength() * (5/6));
                    greenLight.setCx(this.getX() + this.getLength() / 6);
            }
        } else { // vertical stretch orientation
            lightContainer.setX(this.getX() + this.getWidth() / 2 - this.getLightsContainerWidth() / 2);
            lightContainer.setY(this.getY());
            lightContainer.setHeight(this.getLength());
            lightContainer.setWidth(this.getLightsContainerWidth());
            greenLight.setCx(this.getX() + this.getWidth() / 2);
            yellowLight.setCx(this.getX() + this.getWidth() / 2);
            yellowLight.setCy(this.getY() + this.getLength() / 2);
            redLight.setCx(this.getX() + this.getWidth() / 2);
            switch (this.direction) {
                case StraightDirection.NORTH_SOUTH:
                    redLight.setCy(this.getY() + this.getLength() * (5/6));
                    greenLight.setCy(this.getY() + this.getLength() / 6);
                    break;
                case StraightDirection.SOUTH_NORTH:
                    greenLight.setCy(this.getY() + this.getLength() * (5/6));
                    redLight.setCy(this.getY() + this.getLength() / 6);
            }
        }
        lightContainer.setStroke(Color.WHITE);
        lightContainer.setStrokeWidth(2);
        lightContainer.setMinX(minX);
        lightContainer.setMinY(minY);
        lightContainer.setMaxXPlusWidth(maxXPlusWidth);
        lightContainer.setMaxYPlusHeight(maxYPlusHeight);

        greenLight.setRadius(this.getLightRadius());
        greenLight.setFill(Color.GREEN);
        greenLight.setStroke(Color.WHITE);
        greenLight.setStrokeWidth(1);
        greenLight.setMinX(minX);
        greenLight.setMinY(minY);
        greenLight.setMaxXPlusWidth(maxXPlusWidth);
        greenLight.setMaxYPlusHeight(maxYPlusHeight);

        yellowLight.setRadius(this.getLightRadius());
        yellowLight.setFill(Color.YELLOW);
        yellowLight.setStroke(Color.WHITE);
        yellowLight.setStrokeWidth(1);
        yellowLight.setMinX(minX);
        yellowLight.setMinY(minY);
        yellowLight.setMaxXPlusWidth(maxXPlusWidth);
        yellowLight.setMaxYPlusHeight(maxYPlusHeight);

        redLight.setRadius(this.getLightRadius());
        redLight.setFill(Color.RED);
        redLight.setStroke(Color.WHITE);
        redLight.setStrokeWidth(1);
        redLight.setMinX(minX);
        redLight.setMinY(minY);
        redLight.setMaxXPlusWidth(maxXPlusWidth);
        redLight.setMaxYPlusHeight(maxYPlusHeight);

        drawer.createSVG("canvas", {
            x: minX,
            y: minY,
            width: maxXPlusWidth,
            height: maxYPlusHeight
        });

        let trafficLightGroup = drawer.createGroup();
        drawer.setId("trafficLight" + this.id, trafficLightGroup);
        drawer.addClass("trafficLight", trafficLightGroup);
        drawer.addClass("stop", trafficLightGroup);

        return new Array<ShapeComponent>(RectComponent.create(lightContainer, drawer, trafficLightGroup))
            .concat(CircleComponent.create(greenLight, drawer, trafficLightGroup))
            .concat(CircleComponent.create(yellowLight, drawer, trafficLightGroup))
            .concat(CircleComponent.create(redLight, drawer, trafficLightGroup));
    }
}