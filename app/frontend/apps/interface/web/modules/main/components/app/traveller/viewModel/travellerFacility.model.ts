export class TravellerFacilityModel {
    constructor(public travellerId : string,
                public carrier : boolean,
                public passengers : Array<string>,
                public facilityId : number,
                public districtId: number) {}
}