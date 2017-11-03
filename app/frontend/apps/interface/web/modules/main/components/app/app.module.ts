import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { DistrictModule } from './district/district.module';
import { AppComponent } from './app.component';
import { LoginComponent } from './login/login.component';
import { LoginFormComponent } from './login/login.form.component';
import { APP_GUARD_PROVIDERS } from '../../guards';
import { APP_SERVICE_PROVIDERS } from '../../services';
import { routes } from './app.routes';
import {CitiesModule} from './cities/cities.module';
import {PursuitModule} from "./pursuit/pursuit.module";

@NgModule({
    declarations: [
      AppComponent,
      LoginComponent,  // this should probably be a NgModule
      LoginFormComponent // methinks this should be part of the LoginModule
    ],
    providers: [
      APP_GUARD_PROVIDERS,
      APP_SERVICE_PROVIDERS
    ],
    imports: [
        BrowserModule,
        BrowserAnimationsModule,
        RouterModule.forRoot(routes),
        FormsModule,
        HttpModule,
        DistrictModule,
        CitiesModule,
        PursuitModule
    ],
    bootstrap: [AppComponent]
})
export class AppModule {}
