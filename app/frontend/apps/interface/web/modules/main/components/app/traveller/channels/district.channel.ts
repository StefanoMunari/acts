import {Injectable} from '@angular/core';
import {Observable} from 'rxjs/Observable';
import {Socket, SocketOptions, Channel} from 'phoenix';
import {TravellerPositionModel} from "../viewModel/travellerPosition.model";
import {TravellerModel} from "../viewModel/traveller.model";
import {TravellerFacilityModel} from "../viewModel/travellerFacility.model";
import {TrafficLightColorChangeModel} from "../../infrastructure/viewModel/trafficLightColorChange.model";

@Injectable()
export class DistrictChannel {
    private static readonly CHANNEL_PREFIX: string = "district:";
    private socket: Socket;
    private channel: Channel;

    connect(districtId : String) : Observable<any> {
        return Observable.create(observer => {
            const options: SocketOptions = { params: { }};
            this.socket = new Socket('/socket', options);
            this.socket.onOpen(() => {
                observer.next();
            });
            this.socket.onError((err: Error) => {
                this.socket.disconnect();
                return observer.error(err);
            });
            this.socket.connect();
        })
            .flatMap(() => this.joinChannel(districtId));
    }

    joinChannel(districtId : String): Observable<any> {
        let channelName = DistrictChannel.CHANNEL_PREFIX + districtId;
        this.channel = this.socket.channel(channelName, {});
        let channel: Channel = this.channel;
        return Observable.create(observer => {
            channel.join()
                .receive('ok', resp => {
                    observer.next(resp);
                })
                .receive('error', err => {
                    return observer.error(err);
                });
        });
    }

    public subscribeOnTravellerMotion(travellerMotionAction : (position : TravellerPositionModel) => void) {
        if (!!this.channel) {
            this.channel.on("traveller_motion",
                payload => travellerMotionAction(payload));
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public subscribeOnEnterFacility(enterFacility : (traveller : TravellerFacilityModel) => void) {
        if (!!this.channel) {
            this.channel.on("enter_facility",
                payload => enterFacility(payload));
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public subScribeOnExitFacility(exitFacility : (traveller : TravellerFacilityModel) => void) {
        if (!!this.channel) {
            this.channel.on("exit_facility",
                payload => exitFacility(payload));
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public subScribeOnChangeTrafficLightColor(changeTrafficLightColor :
            (trafficLightColorChange : TrafficLightColorChangeModel) => void) {
        if (!!this.channel) {
            this.channel.on("change_color_tl",
                payload => changeTrafficLightColor(payload));
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public subscribeOnExitDistrict(exitDistrict : (traveller : TravellerModel) => void) {
        if (!!this.channel) {
            this.channel.on("exit_district",
                payload => exitDistrict(payload));
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public subscribeOnShutdown(shutdown: () => void) {
        if (!!this.channel) {
            this.channel.on("shutdown",
                payload => shutdown());
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public watchTraveller(travellerId: string) {
        if (!!this.channel) {
            this.channel.push("watch", {"traveller": travellerId});
        } else {
            throw new Error("Missing " + DistrictChannel.CHANNEL_PREFIX + " channel");
        }
    }

    public disconnect() {
        this.socket.disconnect();
    }
}