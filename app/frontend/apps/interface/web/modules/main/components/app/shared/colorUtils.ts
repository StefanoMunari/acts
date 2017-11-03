import {Color} from "./color";

export class ColorUtils {
    private static calculateColorRgb(): Map<Color, string> {
        let colorRgb : Map<Color, string> = new Map<Color, string>();
        colorRgb.set(Color.GRAY, 'rgb(192,192,192)');
        colorRgb.set(Color.GREEN, 'rgb(109,185,102)');
        colorRgb.set(Color.BLACK, 'rgba(80, 80, 80, 1)');
        colorRgb.set(Color.WHITE, 'rgb(255,255,255)');
        colorRgb.set(Color.RED, 'rgb(255,0,0)');
        colorRgb.set(Color.BLUE, 'rgb(0,0,255)');
        colorRgb.set(Color.SALMON, 'rgb(250,128,114)');
        colorRgb.set(Color.ORANGE, 'rgb(255,165,0)');
        colorRgb.set(Color.YELLOW, 'rgb(255,255,0)');
        colorRgb.set(Color.LIGHT_GREEN, 'rgb(144,238,144)');
        colorRgb.set(Color.TURQUOISE, 'rgb(64,224,208)');
        colorRgb.set(Color.ROYAL_BLUE, 'rgb(65,105,225)');
        colorRgb.set(Color.BLUE_VIOLET, 'rgb(138,43,226)');
        colorRgb.set(Color.MAROON, 'rgb(128,0,0)');
        colorRgb.set(Color.DARK_RED, 'rgb(139,0,0)');
        colorRgb.set(Color.BROWN, 'rgb(165,42,42)');
        colorRgb.set(Color.FIREBRICK, 'rgb(178,34,34)');
        colorRgb.set(Color.INDIAN_RED, 'rgb(205,92,92)');
        colorRgb.set(Color.SADDLE_BROWN, 'rgb(139,69,19)');
        colorRgb.set(Color.SIENNA, 'rgb(160,82,45)');
        colorRgb.set(Color.CHOCOLATE, 'rgb(210,105,30)');
        colorRgb.set(Color.PERU, 'rgb(205,133,63)');
        colorRgb.set(Color.SLATE_GRAY, 'rgb(112,128,144)');
        colorRgb.set(Color.LIGHT_SLATE_GRAY, 'rgb(119,136,153)');
        colorRgb.set(Color.TOMATO, 'rgb(255,99,71)');
        colorRgb.set(Color.CORAL, 'rgb(255,127,80)');
        colorRgb.set(Color.LIGHT_CORAL, 'rgb(240,128,128)');
        colorRgb.set(Color.LIGHT_SALMON, 'rgb(255,160,122)');
        colorRgb.set(Color.GOLD, 'rgb(255,215,0)');
        colorRgb.set(Color.PALE_GOLDEN_ROD, 'rgb(238,232,170)');
        colorRgb.set(Color.KHAKI, 'rgb(240,230,140)');
        colorRgb.set(Color.GREEN_YELLOW, 'rgb(173,255,47)');
        colorRgb.set(Color.PALE_GREEN, 'rgb(152,251,152)');
        colorRgb.set(Color.PALE_TURQUOISE, 'rgb(175,238,238)');
        colorRgb.set(Color.AQUA_MARINE, 'rgb(127,255,212)');
        colorRgb.set(Color.POWDER_BLUE, 'rgb(176,224,230)');
        colorRgb.set(Color.LIGHT_BLUE, 'rgb(173,216,230)');
        colorRgb.set(Color.LIGHT_SKY_BLUE, 'rgb(135,206,250)');
        colorRgb.set(Color.THISTLE, 'rgb(216,191,216)');
        colorRgb.set(Color.ANTIQUE_WHITE, 'rgb(250,235,215)');
        colorRgb.set(Color.BEIGE, 'rgb(245,245,220)');
        colorRgb.set(Color.BISQUE, 'rgb(255,228,196)');
        colorRgb.set(Color.BLANCHED_ALMOND, 'rgb(255,235,205)');
        colorRgb.set(Color.WHEAT, 'rgb(245,222,179)');
        colorRgb.set(Color.CORN_SILK, 'rgb(255,248,220)');
        colorRgb.set(Color.LEMON_CHIFFON, 'rgb(255,250,205)');
        colorRgb.set(Color.LIGHT_GOLDEN_ROD_YELLOW, 'rgb(250,250,210)');
        colorRgb.set(Color.LIGHT_YELLOW, 'rgb(255,255,224)');
        colorRgb.set(Color.MOCASSIN, 'rgb(255,228,181)');
        colorRgb.set(Color.NAVAJO_WHITE, 'rgb(255,222,173)');
        colorRgb.set(Color.PEACH_PUFF, 'rgb(255,218,185)');
        colorRgb.set(Color.MISTY_ROSE, 'rgb(255,228,225)');
        colorRgb.set(Color.LAVENDER_BLUSH, 'rgb(255,240,245)');
        colorRgb.set(Color.LINEN, 'rgb(250,240,230)');
        colorRgb.set(Color.OLD_LACE, 'rgb(253,245,230)');
        colorRgb.set(Color.PAPAYA_WHIP, 'rgb(255,239,213)');
        colorRgb.set(Color.SEA_SHELL, 'rgb(255,245,238)');
        colorRgb.set(Color.MINT_CREAM, 'rgb(245,255,250)');
        return colorRgb;
    }

    private static colorRgb : Map<Color, string> = ColorUtils.calculateColorRgb();

    public static getRgbFromColor(color : Color): string {
        return ColorUtils.colorRgb.get(color);
    }
}