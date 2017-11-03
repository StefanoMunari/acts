import { Injectable } from '@angular/core';
let adapteeDrawer = require('svg.js');
let svgPanZoom = require("svg-pan-zoom");

@Injectable()
export class DrawerAdapter {
    private svg = null;
    private mainGroup = null;
    private duration = 200;
    private ease = '>';
    private delay = 0;
    private motionQueues : Map<string, any> = new Map<string, any>();

    public createSVG(container: string, viewbox) {
        if (!this.svg) {
            this.svg = adapteeDrawer(container).viewbox(viewbox);
            this.mainGroup = this.svg.group();
            svgPanZoom('#' + this.svg.id(), {
                zoomEnabled: true,
                controlIconsEnabled: true,
                fit: true,
                center: true,
                viewportSelector: this.mainGroup
            });
        }
        return this.svg;
    }

    public createGroup() {
        return this.mainGroup.group();
    }

    public getId(element): string {
        return element.id();
    }

    public setId(id: string, shape) {
        return shape.id(id);
    }

    public drawRect(width: number, height: number, group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.rect(width, height);
    }

    public drawCircle(group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.circle();
    }

    public drawLine(x1: number, y1: number, x2: number, y2: number, group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.line(x1, y1, x2, y2);
    }

    public drawPolygon(points: string, group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.polygon(points);
    }

    public drawImage(path: string, group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.image(path);
    }

    public drawText(text: String, group?) {
        if(!group) {
            group = this.mainGroup;
        }

        return group.plain(text);
    }

    public rotate(degree: number, cx: number, cy: number, element) {
        return element.rotate(degree, cx, cy);
    }

    public setTextSize(size: number, text) {
        return text.size(size);
    }

    public addClass(c: string, item) {
        return item.addClass(c);
    }

    public setX(x: number, shape) {
        return shape.x(x);
    }

    public setY(y: number, shape) {
        return shape.y(y);
    }

    public setCx(cx: number, shape) {
        return shape.cx(cx);
    }

    public setCy(cy: number, shape) {
        return shape.cy(cy);
    }

    public setRadius(r: number, shape) {
        return shape.radius(r);
    }

    public setFill(color: string, shape) {
        return shape.fill(color);
    }

    public setStroke(color: string, shape) {
        return shape.stroke(color);
    }

    public setStrokeWidth(strokeWidth: number, shape) {
        return shape.attr('stroke-width', strokeWidth);
    }

    public setStrokeDashArray(strokeDashArray: string, shape) {
        let stroke = this.getStroke(shape);
        stroke.dasharray = strokeDashArray;
        return shape.stroke(stroke);
    }

    public getShapeById(id: string) {
        return adapteeDrawer.get(id);
    }

    public setOpacity(opacity: number, shape) {
        return shape.opacity(opacity);
    }

    public setDisplay(display: string, shape) {
        return shape.style('display', display);
    }

    public moveCx(fromCx:number, toCx: number, shape) {
        let self = this;
        let motionsQueue = this.motionQueues.get(shape.id());
        if (!!motionsQueue) {
            motionsQueue = motionsQueue.after(function() {
                self._moveCx(fromCx, toCx, this);
                self.motionQueues.delete(shape.id());
            });
        } else {
            motionsQueue = this._moveCx(fromCx, toCx, shape);
            this.motionQueues.delete(shape.id());
        }
        this.motionQueues.set(shape.id(), motionsQueue);
    }

    public moveCy(fromCy: number, toCy:number, shape) {
        let self = this;
        let motionsQueue = this.motionQueues.get(shape.id());
        if (!!motionsQueue) {
            motionsQueue = motionsQueue.after(function() {
                self._moveCy(fromCy, toCy, this);
                self.motionQueues.delete(shape.id());
            });
        } else {
            motionsQueue = this._moveCy(fromCy, toCy, shape);
            this.motionQueues.delete(shape.id());
        }
        this.motionQueues.set(shape.id(), motionsQueue);
    }

    public remove(shape) {
        let self = this;
        let motionsQueue = this.motionQueues.get(shape.id());
        if (!!motionsQueue) {
            motionsQueue.after(function() {
                self._hide(this);
            }).after(function() {
                this.remove();
                this.motionQueues.delete(shape.id());
            });
        } else {
            this._hide(shape)
                .after(function() {
                    this.remove();
                });
        }
    }

    public hide(shape) {
        let self = this;
        let motionsQueue = this.motionQueues.get(shape.id());
        if (!!motionsQueue) {
            motionsQueue = motionsQueue.after(function() {
                if (self.isShown(shape)) {
                    self._hide(this);
                }
            });
        } else {
            if (self.isShown(shape)) {
                motionsQueue = this._hide(shape);
            }
        }
        this.motionQueues.set(shape.id(), motionsQueue);
    }

    public show(shape) {
        if (!!shape) {
            let self = this;
            let motionsQueue = this.motionQueues.get(shape.id());
            if (!!motionsQueue) {
                motionsQueue = motionsQueue.after(function () {
                    if (self.isHidden(shape)) {
                        self._show(this);
                    }
                });
            } else {
                if (self.isHidden(shape)) {
                    motionsQueue = this._show(shape);
                }
            }
            this.motionQueues.set(shape.id(), motionsQueue);
        }
    }

    public addMousePassageClass(cl, element) {
        element.mouseover(function() {
            this.addClass(cl);
        });
        element.mouseout(function() {
            this.removeClass(cl);
        });
    }

    private getStroke(shape) {
        return {
            color: shape.attr("stroke"),
            width: shape.attr("stroke-width"),
            opacity: shape.attr("stroke-opacity"),
            linecap: shape.attr("stroke-linecap"),
            linejoin: shape.attr("stroke-linejoin"),
            dasharray: "none"
        };
    }

    private _moveCx(fromCx:number, toCx: number, shape) {
        return shape.animate(this.calculateDuration(fromCx, toCx), this.ease, this.delay).cx(toCx);
    }

    private _moveCy(fromCy:number, toCy: number, shape) {
        return shape.animate(this.calculateDuration(fromCy, toCy), this.ease, this.delay).cy(toCy);
    }

    private _hide(shape) {
        return shape.animate(this.duration, this.ease, this.delay).opacity(0);
    }

    private _show(shape) {
        return shape.animate(this.duration, this.ease, this.delay).opacity(1);
    }

    private isHidden(shape) {
        return shape.opacity() === 0;
    }

    private isShown(shape) {
        return shape.opacity() === 1;
    }

    private calculateDuration(source: number, destination: number): number {
        let distance: number = Math.abs(source - destination);
        let duration: number = Math.max(Math.round(distance / 30) * 800, 800);
        return duration;
    }
}