import {
    Component, ViewChild, ComponentFactoryResolver, ViewContainerRef, ComponentRef, AfterViewInit,
    ComponentFactory
} from '@angular/core';
import * as $ from "jquery";
import {InfrastructureLoader} from "../../infrastructure/infrastructure.loader";
import {Facility} from "../../infrastructure/facility/facility";
import {FacilityStatsComponent} from "./facility-stats.component";
import {InfrastructureRegistry} from "../../infrastructure/infrastructure.registry";
import {Subscription} from "rxjs/Subscription";

require("tooltipster/dist/css/tooltipster.bundle.min.css");
require("tooltipster/dist/js/plugins/tooltipster/SVG/tooltipster-SVG.min.js");
require("tooltipster");

@Component({
    selector: 'facilities-stats',
    template: '<template #container></template>',
    styles: [':host {display: none}']
})
export class FacilitiesStatsComponent implements AfterViewInit {
    @ViewChild('container', {read: ViewContainerRef}) container;

    constructor(private componentFactoryResolver: ComponentFactoryResolver,
                private loader: InfrastructureLoader,
                private registry : InfrastructureRegistry) {
    }

    ngAfterViewInit() {
        let subscription: Subscription =
            this.loader.observeInfrastructure().subscribe(
                _ => this.loadInfrastructures(),
                error => console.log(error),
                () => this.loadInfrastructures()
            );
    }

    private loadInfrastructures() {
        for (let facility of this.registry.findAllFacilities()) {
            this.loadFacilityStats(facility);
        }
        let self = this;
        setTimeout(function () {
            for (let facility of self.registry.findAllFacilities()) {
                $("#facility" + facility.getId()).tooltipster({
                    content: $("#popupContentFacility" + facility.getId())
                });
            }
        });
    }

    private loadFacilityStats(facility: Facility) {
        if (!!facility) {
            let componentFactory : ComponentFactory<FacilityStatsComponent> =
                this.componentFactoryResolver.resolveComponentFactory(FacilityStatsComponent);
            let viewContainerRef = this.container;
            let componentRef: ComponentRef<FacilityStatsComponent> = viewContainerRef.createComponent(componentFactory);
            (componentRef.instance).facility = facility;
        }
    }
}