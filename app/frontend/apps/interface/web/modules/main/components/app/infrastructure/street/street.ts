import {Orientation} from "../../shared/orientation";
import {Infrastructure} from "../infrastructure";
import {ShapeComponent} from "../../draw/shape.component";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Way} from "../way/way";
import {Intersection} from "../intersection/intersection";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export class Street extends Infrastructure {
    private orientation: Orientation;
    private roadwayOrdinal: number;
    private ways: Map<number, Way> = new Map<number, Way>();
    private intersections: Map<CardinalDirection, Intersection> = new Map<CardinalDirection, Intersection>();

    constructor(private id : number) {
        super();
    }

    public getId(): number {
        return this.id;
    }

    public getX(): number {
        return this.getFirstWay().getX();
    }

    public getY(): number {
        return this.getFirstWay().getY();
    }

    public getWidth(): number {
        return Array.from(this.ways.values())
            .map(way => way.getWidth())
            .reduce((sum, current) => sum + current);
    }

    public getWaysFirstRoadwayWidth(): number {
        return Array.from(this.getWaysFirstRoadway().values())
            .map(way => way.getWidth())
            .reduce((sum, current) => sum + current);
    }

    public getLength(): number {
        return this.getFirstWay().getLength();
    }

    public getWayMinKey(): number {
        return Array.from(this.ways.keys()).sort()[0];
    }

    public getWayMaxKey(): number {
        return Array.from(this.ways.keys()).sort()[this.ways.size - 1];
    }

    public getFirstWay(): Way {
        return this.ways.get(this.getWayMinKey());
    }

    public getLastWay(): Way {
        return this.ways.get(this.getWayMaxKey());
    }

    public getWay(index: number): Way {
        let key: number = this.getWayKeys()[index];
        return this.ways.get(key);
    }

    public getWaysFirstRoadway(): Array<Way> {
        let ways: Array<Way> = Array.from(this.ways.values());
        return ways.filter((way) => {
            return way.getOrdinal() < this.roadwayOrdinal;
        }).sort((way) => way.getOrdinal()).reverse()
    }

    public getWaysAfterRoadway(): Array<Way> {
        let ways: Array<Way> = Array.from(this.ways.values());
        return ways.filter((way) => {
            return way.getOrdinal() > this.roadwayOrdinal;
        }).sort((way) => way.getOrdinal()).reverse()
    }

    public getWays(): Array<Way> {
        return Array.from(this.ways.values());
    }

    public getWayKeys(): Array<number> {
        return Array.from(this.ways.keys());
    }

    public getWayByKey(key: number): Way {
        return this.ways.get(key);
    }

    public getOrientation(): Orientation {
        return this.orientation;
    }

    public getRoadway(): Way {
        return this.ways.get(this.roadwayOrdinal);
    }

    public setOrientation(orientation: Orientation) {
        this.orientation = orientation;
    }

    public setRoadwayOrdinal(ordinal: number) {
        this.roadwayOrdinal = ordinal;
    }

    public connect(intersection: Intersection, entranceDirection: CardinalDirection) {
        this.intersections.set(entranceDirection, intersection);
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let shapes: Array<ShapeComponent> = new Array<ShapeComponent>();
        this.ways.forEach(way => {
            shapes = shapes.concat(way.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
        });
        return shapes;
    }

    public getIntersections(): Map<CardinalDirection, Intersection> {
        return this.intersections;
    }

    public setRoadwayX(x: number) {
        if (this.getOrientation() == Orientation.HORIZONTAL) {
            for (let key of this.getWayKeys()) {
                let way: Way = this.getWayByKey(key);
                way.setX(x);
            }
        } else { // vertical orientation
            let roadway: Way = this.getRoadway();
            roadway.setX(x);
            let distanceBeforeRoadway: number = 0;
            for (let way of this.getWaysFirstRoadway()) {
                distanceBeforeRoadway += way.getWidth();
                way.setX(x - distanceBeforeRoadway);
            }

            let distanceAfterRoadway: number = roadway.getWidth();
            for (let way of this.getWaysAfterRoadway()) {
                way.setX(x + distanceAfterRoadway);
                distanceAfterRoadway += way.getWidth();
            }
        }
    }

    public setRoadwayY(y: number) {
        if (this.getOrientation() == Orientation.HORIZONTAL) {
            let roadway: Way = this.getRoadway();
            roadway.setY(y);

            let distanceBeforeRoadway: number = 0;
            for (let way of this.getWaysFirstRoadway()) {
                distanceBeforeRoadway += way.getWidth();
                way.setY(y - distanceBeforeRoadway);
            }

            let distanceAfterRoadway: number = roadway.getWidth();
            for (let way of this.getWaysAfterRoadway()) {
                way.setY(y + distanceAfterRoadway);
                distanceAfterRoadway += way.getWidth();
            }
        } else { // vertical orientation
            for (let key of this.getWayKeys()) {
                let way: Way = this.getWayByKey(key);
                way.setY(y);
            }
        }
    }

    public addWay(ordinal: number, way: Way) {
        this.ways.set(ordinal, way);
    }
}