import {ResidentialStretch} from "../../stretch/residentialStretch";
import {StraightDirection} from "../../../shared/straightDirection";
import {Color} from "../../../shared/color";
import {DirectionUtils} from "../../../shared/directionUtils";
import {Vehicle} from "../../../traveller/model/vehicle";
import {VehicleMotion} from "../../../traveller/travellerMotion/vehicleMotion";
import {DrawerAdapter} from "../../../draw/drawerAdapter";
import {Triangle} from "../../../draw/triangle";
import {Rect} from "../../../draw/rect";
import {ShapeComponent} from "../../../draw/shape.component";
import {TriangleComponent} from "../../../draw/triangle.component";
import {RectComponent} from "../../../draw/rect.component";
import {Orientation} from "../../../shared/orientation";

require("./facilityStretch.css");

export class FacilityStretch extends ResidentialStretch {
    private direction: StraightDirection;

    private static readonly BASIS_COLORS: Array<Color> = [
        Color.YELLOW,
        Color.TOMATO,
        Color.ROYAL_BLUE,
        Color.LIGHT_SALMON,
        Color.PALE_TURQUOISE,
        Color.SALMON,
        Color.THISTLE,
        Color.BLUE_VIOLET,
        Color.ORANGE,
        Color.POWDER_BLUE,
        Color.PEACH_PUFF,
        Color.PALE_GOLDEN_ROD,
        Color.CORAL,
        Color.GOLD,
        Color.MISTY_ROSE,
        Color.TURQUOISE,
        Color.LIGHT_CORAL,
        Color.LIGHT_GREEN,
        Color.KHAKI,
        Color.LIGHT_SKY_BLUE,
        Color.MOCASSIN,
        Color.AQUA_MARINE,
        Color.PALE_GREEN,
        Color.NAVAJO_WHITE,
        Color.GREEN_YELLOW,
        Color.LIGHT_BLUE,
        Color.BISQUE,
        Color.WHEAT
    ];

    private static readonly ROOF_COLORS: Array<Color> = [
        Color.MAROON,
        Color.DARK_RED,
        Color.BROWN,
        Color.FIREBRICK,
        Color.INDIAN_RED,
        Color.LIGHT_SLATE_GRAY,
        Color.SADDLE_BROWN,
        Color.SIENNA,
        Color.CHOCOLATE,
        Color.PERU,
        Color.SLATE_GRAY
    ];

    public setDirection(direction: StraightDirection) {
        this.direction = direction;
        this.setOrientation(DirectionUtils.getOrientationByDirection(direction));
    }

    public getDirection(): StraightDirection {
        return this.direction;
    }

    public getColor(): Color {
        return Color.RED;
    }

    public tread(traveller : Vehicle, visitor : VehicleMotion) {
        visitor.enterFacility(this, traveller);
    }

    public getShapes(minX: number, minY: number, maxXPlusWidth: number,
                     maxYPlusHeight: number, drawer: DrawerAdapter): Array<ShapeComponent> {
        let basis: Rect = new Rect();
        basis.addClass("basis");
        let roof: Triangle = new Triangle();
        roof.addClass("roof");
        let chimney: Rect = new Rect();
        chimney.addClass("chimney");
        let mainEntrance: Rect = new Rect();
        mainEntrance.addClass("mainEntrance");
        let mainEntranceWidth: number = 8;
        let chimneyWidth: number =6;

        if (this.getOrientation() === Orientation.HORIZONTAL) {
            basis.setWidth(this.getLength());
            basis.setHeight(this.getWidth() / 2);
            chimney.setWidth(chimneyWidth);
            chimney.setHeight(this.getWidth() / 2);
            mainEntrance.setWidth(mainEntranceWidth);
            mainEntrance.setHeight(basis.getHeight() / 2);

            if (this.getDirection() === StraightDirection.WEST_EAST) {
                basis.setX(this.getX());
                basis.setY(this.getY());

                mainEntrance.setX(basis.getX() + basis.getWidth() / 2 - mainEntranceWidth / 2);
                mainEntrance.setY(basis.getY());

                roof.setP1(basis.getX(), basis.getY() + basis.getHeight());
                roof.setP2(basis.getX() + basis.getWidth(), basis.getY() + basis.getHeight());
                roof.setP3(basis.getX() + basis.getWidth() / 2, basis.getY() + this.getWidth());

                chimney.setX(this.getX() + this.getWidth() / 4 - chimneyWidth / 2);
                chimney.setY(this.getY() + basis.getHeight());
            } else {
                basis.setX(this.getX());
                basis.setY(this.getY() + this.getWidth() / 2);

                mainEntrance.setX(basis.getX() + basis.getWidth() / 2 - mainEntranceWidth / 2);
                mainEntrance.setY(basis.getY() + basis.getHeight() / 2);

                roof.setP1(basis.getX(), basis.getY());
                roof.setP2(basis.getX() + basis.getWidth(), basis.getY());
                roof.setP3(basis.getX() + basis.getWidth() / 2, this.getY());

                chimney.setX(this.getX() + this.getWidth() - this.getWidth() / 4 - chimneyWidth / 2);
                chimney.setY(this.getY());
            }
        } else {
            basis.setWidth(this.getWidth() / 2);
            basis.setHeight(this.getLength());
            chimney.setWidth(this.getWidth() / 2);
            chimney.setHeight(chimneyWidth);
            mainEntrance.setHeight(mainEntranceWidth);
            mainEntrance.setWidth(basis.getWidth() / 2);

            if (this.getDirection() === StraightDirection.SOUTH_NORTH) {
                basis.setX(this.getX());
                basis.setY(this.getY());

                mainEntrance.setX(basis.getX());
                mainEntrance.setY(basis.getY() + basis.getHeight() / 2 - mainEntranceWidth / 2);

                roof.setP1(basis.getX() + basis.getWidth(), basis.getY());
                roof.setP2(basis.getX() + basis.getWidth(), basis.getY() + this.getLength());
                roof.setP3(basis.getX() + this.getWidth(), this.getY() + this.getLength() / 2);

                chimney.setX(this.getX() + basis.getWidth());
                chimney.setY(this.getY() + this.getLength() - this.getLength() / 4 - chimneyWidth / 2);
            } else {
                basis.setX(this.getX() + this.getWidth() / 2);
                basis.setY(this.getY());

                mainEntrance.setX(basis.getX() + basis.getWidth() / 2);
                mainEntrance.setY(basis.getY() + basis.getHeight() / 2 - mainEntranceWidth / 2);

                roof.setP1(basis.getX(), basis.getY());
                roof.setP2(basis.getX(), basis.getY() + this.getLength());
                roof.setP3(this.getX(), this.getY() + this.getLength() / 2);

                chimney.setX(this.getX());
                chimney.setY(this.getY() + this.getLength() / 4 - chimneyWidth / 2);
            }
        }
        basis.setFill(FacilityStretch.generateBasisColor(this.getId()));
        basis.setMinX(minX);
        basis.setMinY(minY);
        basis.setMaxXPlusWidth(maxXPlusWidth);
        basis.setMaxYPlusHeight(maxYPlusHeight);

        let roofColor: Color = FacilityStretch.generateRoofColor(this.getId());
        roof.setFill(roofColor);
        roof.setMinX(minX);
        roof.setMinY(minY);
        roof.setMaxXPlusWidth(maxXPlusWidth);
        roof.setMaxYPlusHeight(maxYPlusHeight);

        chimney.setFill(roofColor);
        chimney.setMinX(minX);
        chimney.setMinY(minY);
        chimney.setMaxXPlusWidth(maxXPlusWidth);
        chimney.setMaxYPlusHeight(maxYPlusHeight);

        mainEntrance.setFill(Color.BLACK);
        mainEntrance.setMinX(minX);
        mainEntrance.setMinY(minY);
        mainEntrance.setMaxXPlusWidth(maxXPlusWidth);
        mainEntrance.setMaxYPlusHeight(maxYPlusHeight);

        drawer.createSVG("canvas", {
            x: minX,
            y: minY,
            width: maxXPlusWidth,
            height: maxYPlusHeight
        });

        let facilityGroup = drawer.createGroup();
        drawer.setId("facility" + this.getId(), facilityGroup);
        drawer.addClass("facility", facilityGroup);
        drawer.addMousePassageClass("openWide", facilityGroup);

        return new Array<ShapeComponent>(RectComponent.create(basis, drawer, facilityGroup),
            RectComponent.create(chimney, drawer, facilityGroup),
            TriangleComponent.create(roof, drawer, facilityGroup),
            RectComponent.create(mainEntrance, drawer, facilityGroup));
    }

    private static generateBasisColor(index: number): Color {
        let colorCounter = FacilityStretch.BASIS_COLORS.length;
        let colorIndex: number = index % colorCounter;
        return FacilityStretch.BASIS_COLORS[colorIndex];
    }

    private static generateRoofColor(index: number): Color {
        let colorCounter = FacilityStretch.ROOF_COLORS.length;
        let colorIndex: number = index % colorCounter;
        return FacilityStretch.ROOF_COLORS[colorIndex];
    }
}