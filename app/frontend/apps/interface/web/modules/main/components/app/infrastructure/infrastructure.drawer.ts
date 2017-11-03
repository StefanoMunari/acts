import {Component, ViewChild, ViewContainerRef, AfterViewInit} from '@angular/core';
import {ShapeComponent} from "../draw/shape.component";
import {Infrastructure} from "./infrastructure";
import {InfrastructureLoader} from "./infrastructure.loader";
import {InfrastructureRegistry} from "./infrastructure.registry";
import {DrawerAdapter} from "../draw/drawerAdapter";
import {Subscription} from "rxjs/Subscription";

@Component({
    selector: 'infrastructure-drawer',
    template: '<template #container></template>'
})
export class InfrastructureDrawer implements AfterViewInit{
    @ViewChild('container', { read: ViewContainerRef }) container;

    constructor(private loader: InfrastructureLoader,
                private registry : InfrastructureRegistry,
                private drawer: DrawerAdapter) {}

    ngAfterViewInit() {
        let subscription: Subscription =
            this.loader.observeInfrastructure().subscribe(
                _ => this.loadInfrastructures(),
                error => console.log(error),
                () => this.loadInfrastructures()
            );
    }

    private loadInfrastructures() {
        let streets: Array<Infrastructure> = this.registry.findAllStreets();
        let intersections: Array<Infrastructure> = this.registry.findAllIntersections();
        for (let infrastructure of streets.concat(intersections)) {
            this.loadInfrastructure(infrastructure);
        }
    }

    private loadInfrastructure(infrastructure: Infrastructure) {
        let shapes: Array<ShapeComponent> = infrastructure.getShapes(
            this.registry.getMinX(), this.registry.getMinY(),
            this.registry.getWidthUpperBound(),
            this.registry.getHeightUpperBound(), this.drawer);
    }
}