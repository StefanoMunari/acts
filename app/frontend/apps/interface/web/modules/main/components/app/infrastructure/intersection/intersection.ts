import {Rect} from "../../draw/rect";
import {Infrastructure} from '../infrastructure';
import {ShapeComponent} from '../../draw/shape.component';
import {RectComponent} from '../../draw/rect.component';
import {CardinalDirection} from "../../shared/cardinalDirection";
import {RoadwayStretch} from "../stretch/roadwayStretch";
import {Color} from "../../shared/color";
import {Traveller} from "../../traveller/model/traveller";
import {TravellerMotion} from "../../traveller/travellerMotion/travellerMotion";
import {Treadable} from "../treadable";
import {Street} from "../street/street";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {Orientation} from "../../shared/orientation";
import {Stop} from "../pavementMarking/stop";
import {RoadLine} from "../pavementMarking/roadLine";
import {StraightDirection} from "../../shared/straightDirection";
import {TrafficLight} from "../trafficLight/trafficLight";
import {IntersectionArmlessSide} from "./intersectionArmlessSide";
import {ConvexCorner} from "./convexCorner";
import {ExitStreet} from "./exitStreet";
import {ConcaveCorner} from "./concaveCorner";
import {ExitRoadwayStretch} from "./exitRoadwayStretch";
import {DistrictExitRoadwayStretch} from "./districtExitRoadwayStretch";

export class Intersection extends Infrastructure implements Treadable {
    private x: number;
    private y: number;
    private streets: Map<CardinalDirection, Street> = new Map<CardinalDirection, Street>();
    private trafficLights: Map<CardinalDirection, number> = new Map<CardinalDirection, number>();
    private roadway: Rect = new Rect();

    static readonly PAVEMENT_MARKING_DISTANCE: number = 5;

    constructor(private id : number) {
        super();
        this.roadway.setWidth(RoadwayStretch.getStandardWidth() * 2);
        this.roadway.setHeight(RoadwayStretch.getStandardWidth() * 2);
        this.roadway.setFill(Color.BLACK);
        this.roadway.setId("intersection" + id);
    }

    public connect(street: Street, exitDirection: CardinalDirection) {
        this.streets.set(exitDirection, street);
    }

    public addTrafficLight(trafficLightId: number, exitDirection: CardinalDirection) {
        this.trafficLights.set(exitDirection, trafficLightId);
    }

    public getId(): number {
        return this.id;
    }

    public getX(): number {
        return this.x;
    }

    public getY(): number {
        return this.y;
    }

    public setRoadwayX(x: number): void {
        this.roadway.setX(x);
        this.x = x;
    }

    public setRoadwayY(y: number): void {
        this.roadway.setY(y);
        this.y = y;
    }

    public getWidth(): number {
        return this.calculateWidth();
    }

    public getHeight(): number {
        return this.calculateHeight();
    }

    public getRoadwayX(): number {
        return this.roadway.getX();
    }

    public getRoadwayY(): number {
        return this.roadway.getY();
    }

    public getRoadwayHeight(): number {
        return this.roadway.getWidth();
    }

    public getRoadwayWidth(): number {
        return this.roadway.getHeight();
    }

    public getStreets(): Map<CardinalDirection, Street> {
        return this.streets;
    }

    public countStreetsOfDistrict(): number {
        return Array.from(this.streets.values())
            .reduce((acc, current) => current !== null ? acc + 1 : acc, 0);
    }

    public countConnectedStreets(): number {
        return this.streets.size;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                      maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {

        let shapes: Array<ShapeComponent> = [];

        this.roadway.setMinX(minX);
        this.roadway.setMinY(minY);
        this.roadway.setMaxXPlusWidth(maxXPlusWidth);
        this.roadway.setMaxYPlusHeight(maxYPlusHeight);

        shapes.push(RectComponent.create(this.roadway, drawer));

        let missingInfrastructures: Array<Infrastructure> = this.generateMissingStretches();
        for (let infrastructure of missingInfrastructures) {
            shapes = shapes.concat(infrastructure.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        let pavementMarking: Array<ShapeComponent> =
            this.generatePavementMarking(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer);

        return shapes.concat(pavementMarking);
    }

    private generatePavementMarking(minX: number, minY: number, maxXPlusWidth: number,
                          maxYPlusHeight: number, drawer: DrawerAdapter) : Array<ShapeComponent>{
        let shapes: Array<ShapeComponent> = [];
        let stop: Stop;
        let trafficLight: TrafficLight;
        let roadLine: RoadLine;
        let streetsOfDistrictCount: number = this.countStreetsOfDistrict();
        let southernStreet: Street = this.streets.get(CardinalDirection.SOUTH);
        let northernStreet: Street = this.streets.get(CardinalDirection.NORTH);
        let easternStreet: Street = this.streets.get(CardinalDirection.EAST);
        let westernStreet: Street = this.streets.get(CardinalDirection.WEST);

        if (this.streets.has(CardinalDirection.SOUTH)) {

            let trafficLightY: number = this.getY() + this.getHeight();
            let roadLineLength: number;
            if (southernStreet !== null) {
                roadLineLength = southernStreet.getY() - trafficLightY;
            } else {
                let referenceStreet: Street;
                if (!!easternStreet) {
                    referenceStreet = easternStreet;
                } else { // westernStreet is not null
                    referenceStreet = westernStreet;
                }
                roadLineLength = this.getY() + this.getHeight() - (referenceStreet.getWidth() / 2 - this.getHeight() / 2);
            }

            if (this.countConnectedStreets() > 2) {

                let trafficLightId: number = this.trafficLights.get(CardinalDirection.SOUTH);
                if (trafficLightId === undefined || trafficLightId === null) {
                    throw new Error("Missing traffic light id for " + CardinalDirection.SOUTH.toString()
                        + " exit of " + this.getId() + " intersection");
                }
                trafficLight = new TrafficLight(trafficLightId);
                trafficLight.setWidth(this.getWidth() / 2);
                trafficLight.setX(this.getX() + this.getWidth() / 2);
                trafficLight.setY(trafficLightY);
                trafficLight.setDirection(StraightDirection.SOUTH_NORTH);

                roadLine = new RoadLine(this.getX(),
                    Intersection.PAVEMENT_MARKING_DISTANCE + trafficLight.getY() + trafficLight.getLength(),
                    roadLineLength - trafficLight.getLength() - Intersection.PAVEMENT_MARKING_DISTANCE,
                    Orientation.VERTICAL, this.getWidth());

                stop = new Stop(this.getX() + this.getWidth() / 2,
                    Intersection.PAVEMENT_MARKING_DISTANCE + trafficLight.getY() + trafficLight.getLength(),
                    false, Orientation.VERTICAL, this.getWidth());

                shapes = shapes.concat(stop.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer))
                    .concat(trafficLight.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
            } else {
                roadLine = new RoadLine(this.getX(), trafficLightY - this.getHeight() / 2, roadLineLength + this.getHeight() / 2,
                    Orientation.VERTICAL, this.getWidth());
            }
            shapes = shapes.concat(roadLine.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        if (this.streets.has(CardinalDirection.NORTH)) {

            let roadLineY: number;
            if (northernStreet !== null) {
                roadLineY = northernStreet.getY() + northernStreet.getLength();
            } else {
                let referenceStreet: Street;
                if (!!easternStreet) {
                    referenceStreet = easternStreet;
                } else { // westernStreet is not null
                    referenceStreet = westernStreet;
                }
                roadLineY = this.getY() - referenceStreet.getWidth() / 2 + this.getHeight() / 2;
            }
            let roadLineLength: number = this.getY() - roadLineY;

            if (this.countConnectedStreets() > 2) {

                let trafficLightId: number = this.trafficLights.get(CardinalDirection.NORTH);
                if (trafficLightId === undefined || trafficLightId === null) {
                    throw new Error("Missing traffic light id for " + CardinalDirection.NORTH.toString()
                        + " exit of " + this.getId() + " intersection");
                }
                trafficLight = new TrafficLight(trafficLightId);
                trafficLight.setWidth(this.getWidth() / 2);
                trafficLight.setX(this.getX());
                trafficLight.setY(roadLineY + roadLineLength - trafficLight.getLength());
                trafficLight.setDirection(StraightDirection.NORTH_SOUTH);

                roadLine = new RoadLine(this.getX(), roadLineY,
                    roadLineLength - trafficLight.getLength() - Intersection.PAVEMENT_MARKING_DISTANCE,
                    Orientation.VERTICAL, this.getWidth());

                stop = new Stop(this.getX(), trafficLight.getY() - Intersection.PAVEMENT_MARKING_DISTANCE,
                    true, Orientation.VERTICAL, this.getWidth());

                shapes = shapes.concat(stop.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer))
                    .concat(trafficLight.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
            } else {
                roadLine = new RoadLine(this.getX(), roadLineY,
                    roadLineLength + this.getHeight() / 2,
                    Orientation.VERTICAL, this.getWidth());
            }
            shapes = shapes.concat(roadLine.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        if (this.streets.has(CardinalDirection.EAST)) {

            let roadLineX: number = this.getX() + this.getWidth();
            let roadLineLength: number;

            if (easternStreet !== null) {
                roadLineLength = easternStreet.getX() - roadLineX;
            } else {
                let referenceStreet: Street;
                if (!!northernStreet) {
                    referenceStreet = northernStreet;
                } else { // southernStreet is not null
                    referenceStreet = southernStreet;
                }
                roadLineLength = this.getX() + this.getWidth() - (referenceStreet.getWidth() / 2 - this.getWidth() / 2);
            }

            if (this.countConnectedStreets() > 2) {
                let trafficLightId: number = this.trafficLights.get(CardinalDirection.EAST);
                if (trafficLightId === undefined || trafficLightId === null) {
                    throw new Error("Missing traffic light id for " + CardinalDirection.EAST.toString()
                        + " exit of " + this.getId() + " intersection");
                }
                trafficLight = new TrafficLight(trafficLightId);
                trafficLight.setWidth(this.getHeight() / 2);
                trafficLight.setX(roadLineX);
                trafficLight.setY(this.getY());
                trafficLight.setDirection(StraightDirection.EAST_WEST);

                roadLine = new RoadLine(Intersection.PAVEMENT_MARKING_DISTANCE + trafficLight.getX() + trafficLight.getLength(),
                    this.getY(),
                    roadLineLength - trafficLight.getLength() - Intersection.PAVEMENT_MARKING_DISTANCE,
                    Orientation.HORIZONTAL, this.getWidth());

                stop = new Stop(Intersection.PAVEMENT_MARKING_DISTANCE + trafficLight.getX() + trafficLight.getLength(),
                    this.getY(), true, Orientation.HORIZONTAL, this.getWidth());

                shapes = shapes.concat(stop.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer))
                    .concat(trafficLight.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
            } else {
                roadLine = new RoadLine(roadLineX - this.getWidth() / 2, this.getY(), roadLineLength + this.getWidth() / 2,
                    Orientation.HORIZONTAL, this.getWidth());
            }
            shapes = shapes.concat(roadLine.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }

        if (this.streets.has(CardinalDirection.WEST)) {

            let roadLineX: number;
            if (westernStreet !== null) {
                roadLineX = westernStreet.getX() + westernStreet.getLength();
            } else {
                let referenceStreet: Street;
                if (!!northernStreet) {
                    referenceStreet = northernStreet;
                } else { // southernStreet is not null
                    referenceStreet = southernStreet;
                }
                roadLineX = this.getX() - referenceStreet.getWidth() / 2 + this.getWidth() / 2;
            }
            let roadLineLength: number = this.getX() - roadLineX;

            if (this.countConnectedStreets() > 2) {
                let trafficLightId: number = this.trafficLights.get(CardinalDirection.WEST);
                if (trafficLightId === undefined || trafficLightId === null) {
                    throw new Error("Missing traffic light id for " + CardinalDirection.WEST.toString()
                        + " exit of " + this.getId() + " intersection");
                }
                trafficLight = new TrafficLight(trafficLightId);
                trafficLight.setWidth(this.getHeight() / 2);
                trafficLight.setX(roadLineX + roadLineLength - trafficLight.getLength());
                trafficLight.setY(this.getY() + this.getHeight() / 2);
                trafficLight.setDirection(StraightDirection.WEST_EAST);

                roadLine = new RoadLine(roadLineX, this.getY(),
                    roadLineLength - trafficLight.getLength() - Intersection.PAVEMENT_MARKING_DISTANCE,
                    Orientation.HORIZONTAL, this.getWidth());

                stop = new Stop(trafficLight.getX() - Intersection.PAVEMENT_MARKING_DISTANCE, this.getY() + this.getWidth() / 2,
                    false, Orientation.HORIZONTAL, this.getWidth());

                shapes = shapes.concat(stop.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer))
                    .concat(trafficLight.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
            } else {
                roadLine = new RoadLine(roadLineX, this.getY(),
                    roadLineLength + this.getWidth() / 2,
                    Orientation.HORIZONTAL, this.getWidth());
            }
            shapes = shapes.concat(roadLine.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        }
        return shapes;
    }

    private calculateWidth() : number {
        return this.roadway.getWidth();
    }

    private calculateHeight() : number {
        return this.roadway.getHeight();
    }

    public tread (traveller : Traveller, visitor : TravellerMotion) {
        visitor.moveToIntersection(this, traveller);
    }

    private generateMissingStretches(): Array<Infrastructure> {
        let missingStretches: Array<Infrastructure> = [];
        if (this.streets.has(CardinalDirection.WEST)){
            let westExitStretch: Infrastructure;
            let westernStreet: Street = this.streets.get(CardinalDirection.WEST);
            if (westernStreet !== null) {
                westExitStretch = new ExitRoadwayStretch(CardinalDirection.WEST, this);
                missingStretches.push(westExitStretch);

                if (this.streets.has(CardinalDirection.NORTH)) {
                    let northWestCorner: ConcaveCorner = new ConcaveCorner(CardinalDirection.NORTH, CardinalDirection.WEST, this);
                    missingStretches = missingStretches.concat(northWestCorner);
                }
                if (this.streets.has(CardinalDirection.SOUTH)) {
                    let southWestCorner: ConcaveCorner = new ConcaveCorner(CardinalDirection.SOUTH, CardinalDirection.WEST, this);
                    missingStretches = missingStretches.concat(southWestCorner);
                }

            } else if (!!this.streets.get(CardinalDirection.NORTH) ||
                       !!this.streets.get(CardinalDirection.SOUTH)) {

                westExitStretch = new DistrictExitRoadwayStretch(CardinalDirection.WEST, this);
                missingStretches.push(westExitStretch);
            }
        } else { // the intersection has not an exit from west
            if (this.streets.has(CardinalDirection.NORTH)) {
                if (this.streets.has(CardinalDirection.SOUTH)) {
                    let westArmlessSide: IntersectionArmlessSide = new IntersectionArmlessSide(CardinalDirection.WEST, this);
                    missingStretches = missingStretches.concat(westArmlessSide);
                } else { // the intersection has not an exit from west but it has a north one in another district
                    let southWestCorner: ConvexCorner = new ConvexCorner(CardinalDirection.SOUTH, CardinalDirection.WEST, this);
                    missingStretches = missingStretches.concat(southWestCorner);
                }
            } else { // the intersection has not an exit from west and north
                let northWestCorner: ConvexCorner = new ConvexCorner(CardinalDirection.NORTH, CardinalDirection.WEST, this);
                missingStretches = missingStretches.concat(northWestCorner);
            }
        }

        if (this.streets.has(CardinalDirection.EAST)){
            let eastExitStretch: Infrastructure;
            let easternStreet: Street = this.streets.get(CardinalDirection.EAST);
            if (easternStreet !== null) {
                eastExitStretch = new ExitRoadwayStretch(CardinalDirection.EAST, this);
                missingStretches.push(eastExitStretch);

                if (this.streets.has(CardinalDirection.NORTH)) {
                    let northEast: ConcaveCorner = new ConcaveCorner(CardinalDirection.NORTH, CardinalDirection.EAST, this);
                    missingStretches = missingStretches.concat(northEast);
                }
                if (this.streets.has(CardinalDirection.SOUTH)) {
                    let southEast: ConcaveCorner = new ConcaveCorner(CardinalDirection.SOUTH, CardinalDirection.EAST, this);
                    missingStretches = missingStretches.concat(southEast);
                }

            } else if (!!this.streets.get(CardinalDirection.NORTH) ||
                       !!this.streets.get(CardinalDirection.SOUTH)) {

                eastExitStretch = new DistrictExitRoadwayStretch(CardinalDirection.EAST, this);
                missingStretches.push(eastExitStretch);
            }
        } else { // the intersection has not an exit from east
            if (this.streets.has(CardinalDirection.NORTH)) {
                if (this.streets.has(CardinalDirection.SOUTH)) {
                    let eastArmlessSide: IntersectionArmlessSide = new IntersectionArmlessSide(CardinalDirection.EAST, this);
                    missingStretches = missingStretches.concat(eastArmlessSide);
                } else { // the intersection has not an exit from east but it has a north one in another district
                    let southEastCorner: ConvexCorner = new ConvexCorner(CardinalDirection.SOUTH, CardinalDirection.EAST, this);
                    missingStretches = missingStretches.concat(southEastCorner);
                }
            } else { // the intersection has not an exit from east and north
                let northEastCorner: ConvexCorner = new ConvexCorner(CardinalDirection.NORTH, CardinalDirection.EAST, this);
                missingStretches = missingStretches.concat(northEastCorner);
            }
        }

        if (this.streets.has(CardinalDirection.NORTH)) {
            let northExitStretch: Infrastructure;
            if (this.streets.get(CardinalDirection.NORTH) !== null) {

                northExitStretch = new ExitRoadwayStretch(CardinalDirection.NORTH, this);
                missingStretches.push(northExitStretch);

            } else if (!!this.streets.get(CardinalDirection.WEST) ||
                       !!this.streets.get(CardinalDirection.EAST)) {

                northExitStretch = new DistrictExitRoadwayStretch(CardinalDirection.NORTH, this);
                missingStretches.push(northExitStretch);
            }
        } else { // the intersection has not an exit from north
            if (this.streets.has(CardinalDirection.WEST)) {
                if (this.streets.has(CardinalDirection.EAST)) {
                    let northArmlessSide: IntersectionArmlessSide = new IntersectionArmlessSide(CardinalDirection.NORTH, this);
                    missingStretches = missingStretches.concat(northArmlessSide);
                }
            }
        }

        if (this.streets.has(CardinalDirection.SOUTH)) {
            let southExitStretch: Infrastructure;
            if (this.streets.get(CardinalDirection.SOUTH) !== null) {

                southExitStretch = new ExitRoadwayStretch(CardinalDirection.SOUTH, this);
                missingStretches.push(southExitStretch);

            } else if (!!this.streets.get(CardinalDirection.WEST) ||
                       !!this.streets.get(CardinalDirection.EAST)) {

                southExitStretch = new DistrictExitRoadwayStretch(CardinalDirection.SOUTH, this);
                missingStretches.push(southExitStretch);
            }
            missingStretches = missingStretches.concat(southExitStretch);
        } else { // the intersection has not an exit from south
            if (this.streets.has(CardinalDirection.WEST)) {
                if (this.streets.has(CardinalDirection.EAST)) {
                    let southArmlessSide: IntersectionArmlessSide = new IntersectionArmlessSide(CardinalDirection.SOUTH, this);
                    missingStretches = missingStretches.concat(southArmlessSide);
                }
            }
        }
        return missingStretches;
    }
}