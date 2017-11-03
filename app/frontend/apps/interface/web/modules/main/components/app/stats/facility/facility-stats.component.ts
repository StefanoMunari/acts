import {Component, Input} from "@angular/core";
import {Facility} from "../../infrastructure/facility/facility";

@Component({
    selector: 'facility-stats',
    template: require('./facility-stats.component.html'),
    styles: [require('./facility-stats.component.scss')]
})
export class FacilityStatsComponent {
    @Input() facility: Facility;

    constructor(){}

    public generateId(): string {
        return "popupContentFacility" + this.facility.getId();
    }
}