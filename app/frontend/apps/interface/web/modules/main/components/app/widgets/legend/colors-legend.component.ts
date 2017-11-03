import {Component, Input} from "@angular/core";
import {Color} from "../../shared/color";
import {ColorUtils} from "../../shared/colorUtils";

@Component({
    selector: 'colors-legend-component',
    template: require('./colors-legend.component.html'),
    styles: [require('./colors-legend.component.scss')]
})
export class ColorsLegendComponent {
    @Input("items") items: Array<[string, Color]>;

    public getRgbFromColor(color: Color): string {
        return ColorUtils.getRgbFromColor(color);
    }
}