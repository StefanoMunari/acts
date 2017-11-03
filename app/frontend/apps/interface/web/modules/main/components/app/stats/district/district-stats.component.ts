import {Component, Input} from "@angular/core";

@Component({
    selector: 'district-stats',
    template: require('./district-stats.component.html')
})
export class DistrictStatsComponent {
    @Input("stats") stats: number;

    constructor(){}
}