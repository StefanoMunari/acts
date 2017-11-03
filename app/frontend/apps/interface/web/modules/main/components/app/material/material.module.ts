import {NgModule} from "@angular/core";
import {
    MatSidenavModule,
    MatListModule,
    MatToolbarModule,
    MatButtonModule,
    MatGridListModule,
    MatIconModule,
    MatExpansionModule,
    MatDialogModule, MatCardModule
}
    from "@angular/material";
import 'hammerjs';
require("@angular/material/prebuilt-themes/indigo-pink.css");
require("./icon.css");
require("./google-fonts.css");

@NgModule({
    imports: [
        MatToolbarModule,
        MatSidenavModule,
        MatListModule,
        MatButtonModule,
        MatGridListModule,
        MatIconModule,
        MatExpansionModule,
        MatDialogModule,
        MatCardModule
    ],
    exports: [
        MatToolbarModule,
        MatSidenavModule,
        MatListModule,
        MatButtonModule,
        MatGridListModule,
        MatIconModule,
        MatExpansionModule,
        MatDialogModule,
        MatCardModule
    ]
})
export class MaterialModule{}