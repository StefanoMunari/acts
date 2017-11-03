import {Way} from './way';
import {Color} from "../../shared/color";
import {ResidentialLane} from "../lane/residentialLane";
import {ResidentialStretch} from "../stretch/residentialStretch";
import {StraightDirection} from "../../shared/straightDirection";

export class ResidentialWay extends Way {
    private lanes : Array<ResidentialLane> = new Array<ResidentialLane>();

    public static create(w: Way, direction: StraightDirection) {
        let way: ResidentialWay = new ResidentialWay();
        way.setOrientation(w.getOrientation());
        let residentialLane: ResidentialLane = ResidentialLane.create(direction, w.getLengthAsCountStretches());
        way.addLane(residentialLane);
        return way;
    }

    public getLanes(): Array<ResidentialLane> {
        return this.lanes;
    }

    public getColor(): Color {
        return Color.WHITE;
    }

    public putStretchAt(index: number, stretch: ResidentialStretch) {
        let lane: ResidentialLane = this.getLanes()[0];
        lane.putStretchAt(index, stretch);
    }
}