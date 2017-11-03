import {Injectable} from "@angular/core";
import {Observable} from "rxjs/Observable";
import {ApiService} from "../../../services/api.service";
import {CitiesModel} from "./viewModels/cities.model";
import {CityModel} from "./viewModels/city.model";
import {Subject} from "rxjs/Subject";
import {Color} from "../shared/color";
import {ColorUtils} from "../shared/colorUtils";

@Injectable()
export class CitiesService{

    private static readonly CITY_DISPLAY_COLORS: Array<Color> = [
        Color.GOLD,
        Color.PEACH_PUFF,
        Color.SALMON,
        Color.LIGHT_GREEN,
        Color.YELLOW,
    ];
    private static readonly L_CITY_DISPLAY_COLORS: Array<Color> = [
        Color.LIGHT_GOLDEN_ROD_YELLOW,
        Color.MISTY_ROSE,
        Color.NAVAJO_WHITE,
        Color.SEA_SHELL,
        Color.LEMON_CHIFFON,
    ];

    private citiesLoaded: Subject<any> = new Subject();
    private cities: Array<CityModel> = null;

    constructor(private apiService: ApiService) {}

    public getCities(): Array<CityModel> {
        return this.cities;
    }

    public loadCities(): Observable<any> {
        if (this.cities === null) {
            this.findCities().map(result => {
                this.cities = result.cities.map(CitiesService.generateColors);
                this.citiesLoaded.complete();
            }).subscribe(
                _ => {},
                error => console.log(error)
            );
        }
        return this.citiesLoaded;
    }

    private findCities(): Observable<CitiesModel> {
        return this.apiService.getJson('/acts/api/cities');
    }

    private static generateColors(city : CityModel): CityModel {
        let index = parseInt(city.id);
        let colorCounter = CitiesService.CITY_DISPLAY_COLORS.length;
        let colorIndex : number = index % colorCounter;
        city.color = ColorUtils.getRgbFromColor(
            CitiesService.CITY_DISPLAY_COLORS[colorIndex]);
        city.lColor = ColorUtils.getRgbFromColor(
            CitiesService.L_CITY_DISPLAY_COLORS[colorIndex]);
        return city;
    }
}
