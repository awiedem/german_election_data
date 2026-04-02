"use client";

import { useI18n } from "@/lib/i18n";
import type { GeoLevel } from "@/lib/types";

interface GeoToggleProps {
  value: GeoLevel;
  onChange: (level: GeoLevel) => void;
}

export default function GeoToggle({ value, onChange }: GeoToggleProps) {
  const { t } = useI18n();

  const levels: { value: GeoLevel; labelKey: string }[] = [
    { value: "bundeslaender", labelKey: "labelBundeslaender" },
    { value: "wahlkreise", labelKey: "labelWahlkreise" },
    { value: "kreise", labelKey: "labelKreise" },
  ];

  return (
    <div className="inline-flex rounded-lg bg-slate-100 p-1">
      {levels.map((level) => (
        <button
          key={level.value}
          onClick={() => onChange(level.value)}
          className={`rounded-md px-3 py-1.5 text-sm font-medium transition-all ${
            value === level.value
              ? "bg-white text-slate-900 shadow-sm"
              : "text-slate-500 hover:text-slate-700"
          }`}
        >
          {t(level.labelKey)}
        </button>
      ))}
    </div>
  );
}
