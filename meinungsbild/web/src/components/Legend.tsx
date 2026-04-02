"use client";

import { legendStops } from "@/lib/colors";

interface LegendProps {
  min: number;
  max: number;
}

export default function Legend({ min, max }: LegendProps) {
  const stops = legendStops(min, max);

  return (
    <div className="space-y-1.5">
      <div
        className="h-2.5 w-full rounded-full"
        style={{
          background: `linear-gradient(to right, ${stops.map((s) => s.color).join(", ")})`,
        }}
      />
      <div className="flex justify-between text-[11px] font-medium text-slate-500">
        <span>{stops[0].label}</span>
        <span>{stops[Math.floor(stops.length / 2)].label}</span>
        <span>{stops[stops.length - 1].label}</span>
      </div>
    </div>
  );
}
