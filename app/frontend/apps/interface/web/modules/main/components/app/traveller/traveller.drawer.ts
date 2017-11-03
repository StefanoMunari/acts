import {Component, ViewChild, ComponentFactoryResolver, ViewContainerRef, ComponentRef, AfterViewInit} from '@angular/core';
import {ShapeComponent} from "../draw/shape.component";
import {TravellerRegistry} from "./traveller.registry";
import {Traveller} from "./model/traveller";
import {DrawerAdapter} from "../draw/drawerAdapter";
import {InfrastructureRegistry} from "../infrastructure/infrastructure.registry";

@Component({
    selector: 'traveller-drawer',
    template: '<template #container></template>'
})
export class TravellerDrawer implements AfterViewInit {
    @ViewChild('container', { read: ViewContainerRef }) container;
    private travellerRefs : Set<string> = new Set();

    constructor(private componentFactoryResolver: ComponentFactoryResolver,
                private travellerRegistry: TravellerRegistry,
                private infrastructureRegistry: InfrastructureRegistry,
                private drawer: DrawerAdapter) {}

    private loadTravellers(travellers: Array<Traveller>) {
        for (let traveller of travellers) {
            this.loadTraveller(traveller);
        }
    }

    ngAfterViewInit() {
        this.loadTravellers(this.travellerRegistry.findAllTravellers());
        this.travellerRegistry.onMoveIn().subscribe(travellers => {
                this.loadTravellers(travellers);
            },
            error => console.log(error));
        this.travellerRegistry.onMoveOut().subscribe(travellers => {
                travellers.forEach(travellerId => this.destroyTraveller(travellerId));
            },
            error => console.log(error));
    }

    private loadTraveller(traveller: Traveller) {
        if (!this.travellerRefs.has(traveller.getId())) {
            let shapes: Array<ShapeComponent> = traveller.getShapes(
                this.infrastructureRegistry.getMinX(), this.infrastructureRegistry.getMinY(),
                this.infrastructureRegistry.getWidthUpperBound(),
                this.infrastructureRegistry.getHeightUpperBound(), this.drawer);
            let componentRefs: Array<ComponentRef<ShapeComponent>> = [];
            for (let shape of shapes) {
                componentRefs.push(this.loadShape(shape));
            }
            this.travellerRefs.add(traveller.getId());
        }
    }

    private loadShape(shapeComponent: ShapeComponent) : ComponentRef<ShapeComponent> {
        if (!!shapeComponent) {
            let componentFactory = this.componentFactoryResolver.resolveComponentFactory(shapeComponent.getComponent());
            let viewContainerRef = this.container;
            let componentRef: ComponentRef<ShapeComponent> = viewContainerRef.createComponent(componentFactory);
            (<ShapeComponent>componentRef.instance).shape = shapeComponent.shape;
            return componentRef;
        }
    }

    private destroyTraveller(travellerId : string) {
        let traveller = this.drawer.getShapeById('traveller' + travellerId);
        this.drawer.remove(traveller);
        this.travellerRefs.delete(travellerId);
    }
}