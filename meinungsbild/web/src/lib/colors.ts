/**
 * Sequential color scale: light → dark blue
 * Clean, professional, colorblind-safe
 */

// Color palette: 5 stops from light to dark blue
const SCALE_COLORS = [
  [239, 243, 255],  // very light blue
  [189, 215, 231],  // light blue
  [107, 174, 214],  // medium blue
  [49, 130, 189],   // blue
  [8, 81, 156],     // dark blue
];

/**
 * Interpolate a normalized value [0,1] to an RGB color
 */
function interpolateColor(t: number): string {
  const v = Math.max(0, Math.min(1, t));
  const idx = v * (SCALE_COLORS.length - 1);
  const lo = Math.floor(idx);
  const hi = Math.min(lo + 1, SCALE_COLORS.length - 1);
  const frac = idx - lo;

  const r = Math.round(SCALE_COLORS[lo][0] + (SCALE_COLORS[hi][0] - SCALE_COLORS[lo][0]) * frac);
  const g = Math.round(SCALE_COLORS[lo][1] + (SCALE_COLORS[hi][1] - SCALE_COLORS[lo][1]) * frac);
  const b = Math.round(SCALE_COLORS[lo][2] + (SCALE_COLORS[hi][2] - SCALE_COLORS[lo][2]) * frac);
  return `rgb(${r}, ${g}, ${b})`;
}

/**
 * Compute min/max from an array of estimates
 */
export function computeRange(estimates: { estimate: number }[]): { min: number; max: number } {
  if (!estimates.length) return { min: 0, max: 1 };
  const values = estimates.map((e) => e.estimate);
  const min = Math.min(...values);
  const max = Math.max(...values);
  // Add small padding so extremes aren't at the very edge
  const pad = (max - min) * 0.02;
  return { min: min - pad, max: max + pad };
}

/**
 * Map an absolute estimate to a color given the data range
 */
export function estimateToColor(value: number, min: number, max: number): string {
  if (max === min) return interpolateColor(0.5);
  const t = (value - min) / (max - min);
  return interpolateColor(t);
}

/**
 * Generate MapLibre fill-color expression for choropleth
 */
export function choroplethExpression(
  property: string,
  estimates: Record<string, number>,
  min: number,
  max: number,
  numericKeys = false
): unknown[] {
  const stops: (string | number)[] = [];
  for (const [code, value] of Object.entries(estimates)) {
    stops.push(numericKeys ? Number(code) : code, estimateToColor(value, min, max));
  }
  return ["match", ["get", property], ...stops, "#e5e7eb"];
}

/**
 * Generate legend stops for a given data range
 */
export function legendStops(min: number, max: number): { value: number; label: string; color: string }[] {
  const steps = 5;
  return Array.from({ length: steps }, (_, i) => {
    const t = i / (steps - 1);
    const value = min + t * (max - min);
    return {
      value,
      label: `${(value * 100).toFixed(0)}%`,
      color: interpolateColor(t),
    };
  });
}
