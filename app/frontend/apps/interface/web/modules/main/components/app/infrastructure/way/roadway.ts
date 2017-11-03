import {Way} from './way';
import {ShapeComponent} from '../../draw/shape.component';
import {StraightDirection} from "../../shared/straightDirection";
import {RoadwayLane} from "../lane/roadwayLane";
import {WayModel} from "../viewModel/way.model";
import {DirectionUtils} from "../../shared/directionUtils";
import {Color} from "../../shared/color";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {RoadLine} from "../pavementMarking/roadLine";

export class Roadway extends Way {
    private lanes : Array<RoadwayLane> = new Array<RoadwayLane>();

    public static createFromModel(w: WayModel): Roadway {
        let way: Roadway = new Roadway();
        way.setId(w.id);
        way.setOrientation(DirectionUtils.getOrientationByDirection(StraightDirection[w.lanes[0].direction]));
        way.setOrdinal(w.ordinal);
        return way;
    }

    public getLanes(): Array<RoadwayLane> {
        return this.lanes;
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let shapes: Array<ShapeComponent> = super.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer);

        let roadLine: RoadLine = new RoadLine(this.getX(), this.getY(),
            this.getLength(), this.getOrientation(), this.getWidth());

        return shapes.concat(roadLine.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));
    }

    public getColor(): Color {
        return Color.BLACK;
    }
}