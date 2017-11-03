import { Routes } from '@angular/router';
import {DistrictComponent} from './district/district.component';
import {CitiesComponent} from "./cities/cities.component";
import {PursuitComponent} from "./pursuit/pursuit.component";

export const routes: Routes = [
  { path: 'dashboard', component: CitiesComponent},
  { path: 'cities/:cityId/districts/:districtId', component: DistrictComponent  },
  { path: 'travellers/:travellerId', component: PursuitComponent },
  { path: '**', redirectTo: '/dashboard' }
];
