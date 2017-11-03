import {Stretch} from "../../infrastructure/stretch/stretch";
import {Traveller} from "../model/traveller";
import {Intersection} from "../../infrastructure/intersection/intersection";
import {DirectionUtils} from "../../shared/directionUtils";
import {Orientation} from "../../shared/orientation";
import {StraightDirection} from "../../shared/straightDirection";
import {DrawerAdapter} from "../../draw/drawerAdapter";

export abstract class TravellerMotion {
    public moveToStretch(stretch : Stretch, traveller : Traveller) {
        let destinationX : number = stretch.getX();
        let destinationY : number = stretch.getY();
        let direction = stretch.getLane().getDirection();

        if (DirectionUtils.getOrientationByDirection(direction) === Orientation.VERTICAL) {
            destinationX = destinationX + stretch.getWidth() / 2;
            destinationY = destinationY + stretch.getLength() / 2;
        } else {
            destinationY = destinationY + stretch.getWidth() / 2;
            destinationX = destinationX + stretch.getLength() / 2;
        }

        let travellerShape = this.getDrawer().getShapeById('traveller' + traveller.getId());
        this.getDrawer().show(travellerShape);

        switch (direction) {
            case StraightDirection.EAST_WEST:
                traveller.moveYTo(destinationY, this.getDrawer());
                traveller.moveXTo(destinationX, this.getDrawer());
                break;
            case StraightDirection.WEST_EAST:
                traveller.moveYTo(destinationY, this.getDrawer());
                traveller.moveXTo(destinationX, this.getDrawer());
                break;
            case StraightDirection.NORTH_SOUTH:
                traveller.moveXTo(destinationX, this.getDrawer());
                traveller.moveYTo(destinationY, this.getDrawer());
                break;
            case StraightDirection.SOUTH_NORTH:
                traveller.moveXTo(destinationX, this.getDrawer());
                traveller.moveYTo(destinationY, this.getDrawer());
                break;
        }
    }

    public enterFacility(stretch: Stretch, traveller: Traveller) {
        this.moveToStretch(stretch, traveller);
        let travellerShape = this.getDrawer().getShapeById('traveller' + traveller.getId());
        this.getDrawer().hide(travellerShape);
    }

    public abstract moveToIntersection(intersection : Intersection, traveller : Traveller);

    protected abstract getDrawer(): DrawerAdapter;
}