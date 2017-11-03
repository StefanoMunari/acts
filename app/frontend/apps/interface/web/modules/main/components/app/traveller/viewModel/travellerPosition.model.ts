export class TravellerPositionModel {
    constructor(public travellerId : string,
                public travellerType : string,
                public passengers : Array<string>,
                public infrastructureId : number,
                public districtId: string) {}
}