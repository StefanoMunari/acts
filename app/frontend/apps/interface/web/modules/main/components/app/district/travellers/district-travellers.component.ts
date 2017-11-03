import {Component, Input} from "@angular/core";
import {TravellerRegistry} from "../../traveller/traveller.registry";
import {Traveller} from "../../traveller/model/traveller";

@Component({
    selector: 'district-travellers',
    template: require('./district-travellers.component.html')
})
export class DistrictTravellersComponent {
    @Input("city-id") cityId: string;
    @Input("district-id") districtId: string;
    @Input("travellers") travellers: Array<Traveller>;
}