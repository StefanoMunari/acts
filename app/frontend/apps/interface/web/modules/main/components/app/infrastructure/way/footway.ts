import {Way} from './way';
import {WayModel} from "../viewModel/way.model";
import {FootwayLane} from "../lane/footwayLane";
import {Color} from "../../shared/color";

export class Footway extends Way {
    private lanes : Array<FootwayLane> = new Array<FootwayLane>();

    public static createFromModel(w: WayModel): Footway {
        let way: Footway = new Footway();
        way.setId(w.id);
        way.setOrdinal(w.ordinal);
        return way;
    }

    public getLanes(): Array<FootwayLane> {
        return this.lanes;
    }

    public getColor(): Color {
        return Color.GRAY;
    }
}