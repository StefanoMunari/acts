import { Component, Input, ViewChild,
        ComponentFactoryResolver, ViewContainerRef, ComponentRef } from '@angular/core';
import { ShapeComponent } from './shape.component';

@Component({
    selector: 'canvas-component',
    template: '<template #container></template>'
})
export class CanvasComponent {
  @Input() shapeComponents: ShapeComponent[];
  @ViewChild('container', { read: ViewContainerRef }) container;

  constructor(private componentFactoryResolver: ComponentFactoryResolver) {}

  ngAfterViewInit() {
    for (let shapeComponent of this.shapeComponents) {
      this.loadComponent(shapeComponent);
    }
  }
  
  private loadComponent(shapeComponent: ShapeComponent) {
      if (!!shapeComponent) {
          let componentFactory = this.componentFactoryResolver.resolveComponentFactory(shapeComponent.getComponent());
          let viewContainerRef = this.container;
          let componentRef: ComponentRef<ShapeComponent> = viewContainerRef.createComponent(componentFactory);
          (componentRef.instance).shape = shapeComponent.shape;
      }
  }
}