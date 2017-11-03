import {Component} from "@angular/core";
import {InfrastructureLoader} from "../infrastructure/infrastructure.loader";
import {TravellerLoader} from "../traveller/traveller.loader";
import {ActivatedRoute} from "@angular/router";
import {TravellerRegistry} from "../traveller/traveller.registry";
import {DrawerAdapter} from "../draw/drawerAdapter";

@Component({
    selector: 'pursuit-component',
    template: require("./pursuit.component.html"),
    styles: [require('./pursuit.component.scss')],
    providers: [
        InfrastructureLoader,
        DrawerAdapter,
        TravellerLoader
    ]
})
export class PursuitComponent{
    public travellerId: string;
    private params: any;
    private queryParams: any;

    constructor(private infrastructureLoader: InfrastructureLoader,
                private travellerLoader: TravellerLoader,
                private travellerRegistry: TravellerRegistry,
                private route: ActivatedRoute) {}

    ngOnInit() {
        this.params = this.route.params.subscribe(params => {
            this.travellerId = params['travellerId'];

            this.queryParams = this.route.queryParams.subscribe(params => {
                let sourceCity: string = params['sourceCity'];
                let sourceDistrict: string = params['sourceDistrict'];

                this.travellerLoader.pursueTraveller(this.travellerId, sourceCity, sourceDistrict);
                this.infrastructureLoader.loadDistrict(sourceCity, sourceDistrict);
            });
        });
    }

    ngOnDestroy() {
        this.params.unsubscribe();
        this.queryParams.unsubscribe();
        //this.travellerLoader.disconnect();
        this.travellerRegistry.clear();
        //this.infrastructureRegistry.clear();
    }
}