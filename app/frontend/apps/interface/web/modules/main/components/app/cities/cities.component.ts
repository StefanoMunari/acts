import {Component} from "@angular/core";
import {CitiesService} from "./cities.service";
import {CityModel} from "./viewModels/city.model";
import {Color} from "../shared/color";
import {ColorUtils} from "../shared/colorUtils";

@Component({
  selector: 'cities',
  template: require('./cities.component.html'),
  styles: [
    require('./cities.component.scss')
  ]
})
export class CitiesComponent{
  public cities: Array<CityModel> = new Array<CityModel>();

  constructor(public citiesService: CitiesService) {
      citiesService.loadCities().subscribe(
          _ => this.cities = citiesService.getCities(),
          e => console.log("error ", e),
          () => this.cities = citiesService.getCities()
      );
  }
}
