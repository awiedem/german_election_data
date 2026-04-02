"use client";

import { useI18n, type Locale } from "@/lib/i18n";

export default function LanguageToggle() {
  const { locale, setLocale } = useI18n();

  const options: { value: Locale; label: string }[] = [
    { value: "de", label: "DE" },
    { value: "en", label: "EN" },
  ];

  return (
    <div className="inline-flex rounded-full bg-slate-100 p-0.5">
      {options.map((opt) => (
        <button
          key={opt.value}
          onClick={() => setLocale(opt.value)}
          className={`rounded-full px-2.5 py-1 text-xs font-semibold tracking-wide transition-all ${
            locale === opt.value
              ? "bg-white text-slate-900 shadow-sm"
              : "text-slate-500 hover:text-slate-700"
          }`}
        >
          {opt.label}
        </button>
      ))}
    </div>
  );
}
