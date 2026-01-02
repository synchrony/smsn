// =============================================================================
// Color Utilities (matching smsn-mode)
// =============================================================================

import { State } from './state.js';

export function parseColor(numericColor) {
    // Convert numeric color (e.g., 0xff0000) to RGB
    const blue = numericColor % 256;
    const green = Math.floor(numericColor / 256) % 256;
    const red = Math.floor(numericColor / 65536);
    return { r: red, g: green, b: blue };
}

export function colorToHex(color) {
    const r = Math.round(color.r).toString(16).padStart(2, '0');
    const g = Math.round(color.g).toString(16).padStart(2, '0');
    const b = Math.round(color.b).toString(16).padStart(2, '0');
    return `#${r}${g}${b}`;
}

export function darkenColor(color, factor) {
    // Darken by multiplying RGB values
    return {
        r: color.r * factor,
        g: color.g * factor,
        b: color.b * factor
    };
}

export function fadeColor(color, weight) {
    // Fade toward white based on weight (lower weight = more faded)
    // This matches smsn-mode's fade function
    function fade(c, w) {
        const low = c + (255 - c) * 0.9375; // weighted toward white
        const high = c;
        return low + (high - low) * w;
    }
    return {
        r: fade(color.r, weight),
        g: fade(color.g, weight),
        b: fade(color.b, weight)
    };
}

export function getNoteColor(source, weight) {
    const sourceConfig = State.sourcesByName[source];
    if (!sourceConfig || !sourceConfig.color) {
        return '#333333'; // fallback
    }

    const baseColor = parseColor(sourceConfig.color);
    // Darken for better readability on white background
    const darkenedColor = darkenColor(baseColor, 0.6);
    // Fade based on weight (lower weight = more faded toward white)
    const fadedColor = fadeColor(darkenedColor, weight);
    return colorToHex(fadedColor);
}

export function getSourceColor(source) {
    const sourceConfig = State.sourcesByName[source];
    if (!sourceConfig || !sourceConfig.color) {
        return '#333333';
    }
    return colorToHex(parseColor(sourceConfig.color));
}
