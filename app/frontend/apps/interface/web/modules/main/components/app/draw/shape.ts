export abstract class Shape {
    private id: string;
    private classes: Array<string> = new Array<string>("shape");

    constructor() {
    }

    public setId(id: string): void {
        this.id = id;
    }

    public getId(): string {
        return this.id;
    }

    public getClasses(): string {
        let classes: string = "";

        for (let i: number = 0; i < this.classes.length; ++i) {
            classes += this.classes[i];

            if (i < this.classes.length -1) {
                classes += " ";
            }
        }

        return classes;
    }

    public getClassesArray(): Array<string> {
        return this.classes;
    }

    public addClass(c: string) {
        this.classes.push(c);
    }

    public abstract getMinX(): number;

    public abstract getMinY(): number;

    public abstract getMaxXPlusWidth(): number;

    public abstract getMaxYPlusHeight(): number;

    public getViewBox() {
        return {
            x: this.getMinX(),
            y: this.getMinY(),
            width: this.getMaxXPlusWidth(),
            height: this.getMaxYPlusHeight()
        };
    }
}