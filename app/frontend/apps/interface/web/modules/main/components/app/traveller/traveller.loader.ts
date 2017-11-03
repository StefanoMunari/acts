import {Injectable} from '@angular/core';
import {TravellerRegistry} from "./traveller.registry";
import {InfrastructureRegistry} from "../infrastructure/infrastructure.registry";
import {Traveller} from "./model/traveller";
import {TravellerType} from "./travellerType";
import {Pedestrian} from "./model/pedestrian";
import {Vehicle} from "./model/vehicle";
import {Bicycle} from "./model/bicycle";
import {Bus} from "./model/bus";
import {PrivateMotorVehicle} from "./model/privateMotorVehicle";
import {PedestrianMotion} from "./travellerMotion/pedestrianMotion";
import {VehicleMotion} from "./travellerMotion/vehicleMotion";
import {Treadable} from "../infrastructure/treadable";
import {TravellerModel} from "./viewModel/traveller.model";
import {TravellerFacilityModel} from "./viewModel/travellerFacility.model";
import {TravellerPositionModel} from "./viewModel/travellerPosition.model";
import {Subscription} from "rxjs/Subscription";
import {FacilityStretch} from "../infrastructure/facility/stretch/facilityStretch";
import {DistrictChannel} from './channels/district.channel';
import {BicycleMotion} from "./travellerMotion/bicycleMotion";
import {TrafficLightColorChangeModel} from "../infrastructure/viewModel/trafficLightColorChange.model";
import {Color} from "../shared/color";
import * as $ from "jquery";
import {InfrastructureLoader} from "../infrastructure/infrastructure.loader";
require("./traveller.css");

@Injectable()
export class TravellerLoader {

    private districtConnection    : Subscription;
    private travellerConnection    : Subscription;

    constructor(private infrastructureRegistry : InfrastructureRegistry,
                private travellerRegistry      : TravellerRegistry,
                private pedestrianMotion       : PedestrianMotion,
                private bicycleMotion          : BicycleMotion,
                private vehicleMotion          : VehicleMotion,
                private districtChannel        : DistrictChannel,
                private infrastructureLoader   : InfrastructureLoader) {
    }

    public connectToDistrict(districtId: string, callback?: () => void) {
        this.districtConnection = this.districtChannel.connect(districtId)
            .subscribe(
                user => {

                    this.districtChannel.subscribeOnTravellerMotion(
                        (position : TravellerPositionModel) => {
                            this.pursueCheckAndExecute(position, () => this.moveTraveller(position));
                        }
                    );

                    this.districtChannel.subscribeOnEnterFacility(
                        (traveller : TravellerFacilityModel) => {
                            this.pursueCheckAndExecute(traveller, () => this.enterFacility(traveller));
                        }
                    );

                    this.districtChannel.subScribeOnExitFacility(
                        (traveller : TravellerFacilityModel) => {
                            this.pursueCheckAndExecute(traveller, () => this.exitFacility(traveller));
                        }
                    );

                    this.districtChannel.subScribeOnChangeTrafficLightColor(
                        (trafficLightColorChange : TrafficLightColorChangeModel) =>
                            this.changeTrafficLightColor(trafficLightColorChange)
                    );

                    this.districtChannel.subscribeOnExitDistrict(
                        (traveller : TravellerModel) => this.exitDistrict(traveller)
                    );

                    if (!!callback) {
                        callback();
                    }
                },
                error => {
                    console.log('Please try again');
                }
            ).add(_ => {
                this.districtChannel.disconnect();
            });
    }

    private connectToTraveller(travellerId: string, sourceDistrict: string) {
        this.connectToDistrict(sourceDistrict, () => {this.districtChannel.watchTraveller(travellerId)});
    }

    public pursueTraveller(travellerId: string, sourceCity: string, sourceDistrict: string) {
        if (travellerId === undefined || travellerId === null) {
            throw new Error("missing traveller id");
        }
        if (sourceCity === undefined || sourceCity === null) {
            throw new Error("missing source city");
        }
        if (sourceDistrict === undefined || sourceDistrict === null) {
            throw new Error("missing source district");
        }
        this.infrastructureRegistry.setCurrentCityId(sourceCity);
        this.travellerRegistry.setPursuedTravellerId(travellerId);
        this.connectToTraveller(travellerId, sourceDistrict);
    }

    public disconnectFromDistrict() {
        this.districtConnection.unsubscribe();
    }

    private pursueCheckAndExecute(position: any, action: () => void) {
        if (position.travellerId === undefined || position.travellerId === null) {
            return new Error("Missing traveller id");
        }
        if (position.districtId === undefined || position.districtId === null) {
            return new Error("Missing district id");
        }
        if (this.travellerRegistry.getPursuedTravellerId() === null ||
            position.travellerId !== this.travellerRegistry.getPursuedTravellerId() ||
            position.districtId === this.infrastructureRegistry.getCurrentDistrictId()) {
            action();
        } else {
            this.disconnectFromDistrict();
            this.infrastructureLoader.loadDistrictById(position.districtId, () => {
                this.connectToDistrict(position.districtId);
                action();
            });
        }
    }

    private moveTraveller(position: TravellerPositionModel) {
        if (!this.infrastructureRegistry.containsTreadable(position.infrastructureId)) {
            throw new Error("Missing treadable infrastructure with id = " + position.infrastructureId);
        }

        let infrastructure: Treadable =
            this.infrastructureRegistry.findTreadable(position.infrastructureId);

        let traveller: Traveller;
        if (this.travellerRegistry.containsTraveller(position.travellerId)) {
            traveller = this.travellerRegistry.findTraveller(position.travellerId);
            traveller.treadInfrastructure(infrastructure);
        } else {
            let travellerType: TravellerType = TravellerType[position.travellerType];
            switch (travellerType) {
                case TravellerType.PEDESTRIAN:
                    let pedestrian: Pedestrian = new Pedestrian(position.travellerId);
                    pedestrian.setInfrastructureVisitor(this.pedestrianMotion);
                    this.travellerRegistry.addPerson(position.travellerId);
                    traveller = pedestrian;
                    break;
                case TravellerType.BICYCLE:
                    let bicycle: Bicycle = new Bicycle(position.travellerId);
                    bicycle.setInfrastructureVisitor(this.bicycleMotion);
                    this.travellerRegistry.addVehicle(position.travellerId);
                    traveller = bicycle;
                    break;
                case TravellerType.BUS:
                    let bus: Bus = new Bus(position.travellerId);
                    bus.setInfrastructureVisitor(this.vehicleMotion);
                    this.travellerRegistry.addVehicle(position.travellerId);
                    traveller = bus;
                    break;
                case TravellerType.PRIVATE_MOTOR_VEHICLE:
                    let privateMotorVehicle: PrivateMotorVehicle = new PrivateMotorVehicle(position.travellerId);
                    privateMotorVehicle.setInfrastructureVisitor(this.vehicleMotion);
                    this.travellerRegistry.addVehicle(position.travellerId);
                    traveller = privateMotorVehicle;
                    break;
                case TravellerType.VEHICLE:
                    let vehicle: Vehicle = new Vehicle(position.travellerId);
                    vehicle.setInfrastructureVisitor(this.vehicleMotion);
                    this.travellerRegistry.addVehicle(position.travellerId);
                    traveller = vehicle;
                    break;
                default:
                    throw new Error("Unknown traveller type: " + travellerType + " is not a traveller type");
            }
            traveller.treadInfrastructure(infrastructure);
            if (position.travellerId === this.travellerRegistry.getPursuedTravellerId()) {
                traveller.pursue();
            }
            this.travellerRegistry.addTraveller(position.travellerId, traveller);
            for (let passengerId of position.passengers) {
                this.travellerRegistry.addTraveller(passengerId, new Pedestrian(passengerId));
                this.travellerRegistry.addPerson(passengerId);
            }
        }
    }

    private exitDistrict(traveller: TravellerModel) {
        this.travellerRegistry.removeTraveller(traveller.travellerId);
        if (!traveller.passengers || traveller.passengers.length === 0) {
            this.travellerRegistry.removePerson(traveller.travellerId);
        } else {
            this.travellerRegistry.removeVehicle(traveller.travellerId);
            for (let passengerId of traveller.passengers) {
                this.travellerRegistry.removeTraveller(passengerId);
                this.travellerRegistry.removePerson(passengerId);
            }
        }
    }

    private enterFacility(actionData: TravellerFacilityModel) {
        let travellerId : string = actionData.travellerId;
        let facilityId : number = actionData.facilityId;

        if (!this.travellerRegistry.containsTraveller(travellerId)) {
            throw new Error("Missing traveller with id = " + travellerId);
        }

        if (!this.infrastructureRegistry.containsTreadable(facilityId)) {
            throw new Error("Missing facility with id = " + facilityId);
        }

        if (actionData.carrier === null || actionData.carrier === undefined) {
            throw new Error("Missing 'carrier' for traveller " + travellerId + " entering facility " + facilityId);
        }

        let traveller: Traveller = this.travellerRegistry.findTraveller(travellerId);

        let facility: FacilityStretch =
            this.infrastructureRegistry.findFacility(facilityId).getStretch();

        traveller.treadInfrastructure(facility);

        if (actionData.carrier) {
            if (!actionData.passengers || actionData.passengers.length === 0) {
                throw new Error("Missing 'passengers' for traveller " + travellerId + " entering facility " + facilityId);
            }
            let vehicle: Traveller = this.travellerRegistry.findTraveller(travellerId);
            this.infrastructureRegistry.parkVehicleIntoFacility(vehicle, actionData.passengers, facilityId);
            this.travellerRegistry.addVehicle(travellerId);
            this.travellerRegistry.addPeople(actionData.passengers);
        } else {
            this.infrastructureRegistry.enterPersonIntoFacility(travellerId, facilityId);
            this.travellerRegistry.addPerson(travellerId);
        }
    }

    private exitFacility(actionData: TravellerFacilityModel) {
        let travellerId : string = actionData.travellerId;
        let facilityId : number = actionData.facilityId;

        if (actionData.carrier === null || actionData.carrier === undefined) {
            throw new Error("Missing 'carrier' for traveller " + travellerId + " exiting facility " + facilityId);
        }

        if (actionData.carrier) {
            if (!actionData.passengers || actionData.passengers.length === 0) {
                throw new Error("Missing 'passengers' for traveller " + travellerId + " exiting facility " + facilityId);
            }
            this.infrastructureRegistry.takeVehicleFromFacility(travellerId, actionData.passengers, facilityId);
            this.travellerRegistry.addVehicle(travellerId);
            this.travellerRegistry.addPeople(actionData.passengers);
        } else {
            this.infrastructureRegistry.exitPersonFromFacility(travellerId, facilityId);
            this.travellerRegistry.addPerson(travellerId);
        }
    }

    private changeTrafficLightColor(trafficLightColorChange : TrafficLightColorChangeModel) {
        let trafficLightId: number = trafficLightColorChange.trafficLight;
        let color: Color = Color[trafficLightColorChange.color];
        if (color === Color.GREEN) {
            if ($('#trafficLight' + trafficLightId).hasClass("stop"))
                $('#trafficLight' + trafficLightId).removeClass("stop");
            if (!$('#trafficLight' + trafficLightId).hasClass("go"))
                $('#trafficLight' + trafficLightId).addClass("go");
        } else if (color === Color.RED) {
            if ($('#trafficLight' + trafficLightId).hasClass("go"))
                $('#trafficLight' + trafficLightId).removeClass("go");
            if (!$('#trafficLight' + trafficLightId).hasClass("stop"))
                $('#trafficLight' + trafficLightId).addClass("stop");
        }
    }
}