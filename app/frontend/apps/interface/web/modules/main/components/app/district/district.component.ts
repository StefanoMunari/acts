import {Component} from '@angular/core';
import {InfrastructureLoader} from "../infrastructure/infrastructure.loader";
import {TravellerLoader} from "../traveller/traveller.loader";
import {TravellerRegistry} from "../traveller/traveller.registry";
import {ActivatedRoute} from "@angular/router";
import {DrawerAdapter} from "../draw/drawerAdapter";
import {DistrictChannel} from "../traveller/channels/district.channel";
import {TerminationDialog} from "./termination/termination-dialog";
import {MatDialog} from "@angular/material";

@Component({
    selector: 'district-component',
    template: require('./district.component.html'),
    styles: [
        require('./district.component.scss'),
        require('./district-sidenav.component.scss')
    ],
    providers: [
        InfrastructureLoader,
        DrawerAdapter,
        TravellerLoader
    ]
})
export class DistrictComponent {
    public cityId: string;
    public districtId: string;
    public districtName: string;
    private sub: any;

    constructor(private infrastructureLoader: InfrastructureLoader,
                private travellerLoader: TravellerLoader,
                private travellerRegistry: TravellerRegistry,
                private districtChannel: DistrictChannel,
                private dialog: MatDialog,
                private route: ActivatedRoute) {}

    public ngOnInit() {
        this.sub = this.route.params.subscribe(params => {
            this.cityId = params['cityId'];
            this.districtId = params['districtId'];
            this.districtName = this.districtId;
            this.travellerLoader.connectToDistrict(this.districtId,() => {
                this.districtChannel.subscribeOnShutdown(() => {
                    this.openTerminationDialog();
                });
            });
            this.infrastructureLoader.loadDistrict(this.cityId, this.districtId);
        });
    }

    public ngOnDestroy() {
        this.sub.unsubscribe();
        this.travellerLoader.disconnectFromDistrict();
        this.travellerRegistry.clear();
        //this.infrastructureRegistry.clear();
    }

    private openTerminationDialog(): void {
        let dialogRef = this.dialog.open(TerminationDialog, {
            width: '300px'
        });
    }
}