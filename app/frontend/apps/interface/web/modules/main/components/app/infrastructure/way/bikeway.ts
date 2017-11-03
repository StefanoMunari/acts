import {Way} from './way';
import {WayModel} from "../viewModel/way.model";
import {BikewayLane} from "../lane/bikewayLane";
import {Color} from "../../shared/color";

export class Bikeway extends Way {
    private lanes : Array<BikewayLane> = new Array<BikewayLane>();

    public static createFromModel(w: WayModel): Bikeway {
        let way: Bikeway = new Bikeway();
        way.setId(w.id);
        way.setOrdinal(w.ordinal);
        return way;
    }

    public getLanes(): Array<BikewayLane> {
        return this.lanes;
    }

    public getColor(): Color {
        return Color.GREEN;
    }
}