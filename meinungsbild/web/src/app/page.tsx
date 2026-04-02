"use client";

import { useState, useEffect, useMemo } from "react";
import dynamic from "next/dynamic";
import { I18nProvider, useI18n } from "@/lib/i18n";
import IssueSelector from "@/components/IssueSelector";
import GeoToggle from "@/components/GeoToggle";
import Legend from "@/components/Legend";
import IssueInfo from "@/components/IssueInfo";
import LanguageToggle from "@/components/LanguageToggle";
import { computeRange } from "@/lib/colors";
import type { Issue, GeoLevel, KreisEstimate, BundeslandEstimate, WkrEstimate } from "@/lib/types";

const Map = dynamic(() => import("@/components/Map"), { ssr: false });

type EstimatesData = Record<string, KreisEstimate[] | BundeslandEstimate[] | WkrEstimate[]>;

function HomePage() {
  const { t, locale } = useI18n();
  const [issues, setIssues] = useState<Issue[]>([]);
  const [selectedIssue, setSelectedIssue] = useState<string>("");
  const [geoLevel, setGeoLevel] = useState<GeoLevel>("bundeslaender");
  const [kreisEstimates, setKreisEstimates] = useState<EstimatesData>({});
  const [blEstimates, setBlEstimates] = useState<EstimatesData>({});
  const [wkrEstimates, setWkrEstimates] = useState<EstimatesData>({});
  const [hoveredRegion, setHoveredRegion] = useState<{
    name: string;
    estimate: number;
    code: string;
  } | null>(null);

  useEffect(() => {
    Promise.all([
      fetch("/data/issues.json").then((r) => r.json()),
      fetch("/data/estimates_kreis.json").then((r) => r.json()),
      fetch("/data/estimates_bundesland.json").then((r) => r.json()),
      fetch("/data/estimates_wkr.json").then((r) => r.json()),
    ]).then(([issueData, kreisData, blData, wkrData]) => {
      setIssues(issueData as Issue[]);
      setKreisEstimates(kreisData as EstimatesData);
      setBlEstimates(blData as EstimatesData);
      setWkrEstimates(wkrData as EstimatesData);
      if (Array.isArray(issueData) && issueData.length > 0) {
        setSelectedIssue(issueData[0].issue_id);
      }
    });
  }, []);

  const currentEstimates = geoLevel === "kreise" ? kreisEstimates : geoLevel === "wahlkreise" ? wkrEstimates : blEstimates;
  const selectedIssueMeta = issues.find((i) => i.issue_id === selectedIssue);
  const dataRange = useMemo(() => {
    const data = currentEstimates[selectedIssue];
    if (!data) return { min: 0, max: 1 };
    return computeRange(data as { estimate: number }[]);
  }, [currentEstimates, selectedIssue]);

  const regionCount = useMemo(() => {
    const data = currentEstimates[selectedIssue];
    return data ? data.length : 0;
  }, [currentEstimates, selectedIssue]);

  return (
    <div className="flex h-screen flex-col">
      {/* Header */}
      <header className="border-b border-slate-200 bg-white/80 backdrop-blur-sm px-5 py-3">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <a href="/" className="flex items-center gap-2.5 group">
              <div className="h-8 w-8 rounded-lg bg-gradient-to-br from-blue-600 to-blue-700 flex items-center justify-center shadow-sm">
                <span className="text-white font-bold text-sm">M</span>
              </div>
              <div>
                <h1 className="text-lg font-bold text-slate-900 group-hover:text-blue-700 transition-colors">
                  {t("siteTitle")}
                </h1>
                <p className="text-[11px] text-slate-500 -mt-0.5">
                  {t("siteSubtitle")}
                </p>
              </div>
            </a>
          </div>
          <div className="flex items-center gap-5">
            <nav className="flex gap-4 text-sm">
              <a
                href="/methodik"
                className="text-slate-600 hover:text-blue-700 font-medium transition-colors"
              >
                {t("navMethodology")}
              </a>
              <a
                href="/daten"
                className="text-slate-600 hover:text-blue-700 font-medium transition-colors"
              >
                {t("navData")}
              </a>
              <a
                href="/ueber"
                className="text-slate-600 hover:text-blue-700 font-medium transition-colors"
              >
                {t("navAbout")}
              </a>
            </nav>
            <div className="h-4 w-px bg-slate-200" />
            <LanguageToggle />
          </div>
        </div>
      </header>

      {/* Main content */}
      <div className="flex flex-1 overflow-hidden">
        {/* Sidebar */}
        <aside className="w-80 flex-shrink-0 overflow-y-auto border-r border-slate-200 bg-white">
          <div className="p-4 space-y-5">
            {/* Topic selector */}
            <div>
              <label className="mb-1.5 block text-[11px] font-semibold uppercase tracking-wider text-slate-400">
                {t("labelTopic")}
              </label>
              <IssueSelector
                issues={issues}
                selected={selectedIssue}
                onChange={setSelectedIssue}
              />
            </div>

            {/* Issue details */}
            {selectedIssueMeta && (
              <IssueInfo
                issue={selectedIssueMeta}
                dataRange={dataRange}
                regionCount={regionCount}
              />
            )}

            {/* Divider */}
            <div className="border-t border-slate-100" />

            {/* Geography toggle */}
            <div>
              <label className="mb-1.5 block text-[11px] font-semibold uppercase tracking-wider text-slate-400">
                {t("labelGeography")}
              </label>
              <GeoToggle value={geoLevel} onChange={setGeoLevel} />
            </div>

            {/* Legend */}
            <div>
              <Legend min={dataRange.min} max={dataRange.max} />
            </div>

            {/* Hover info */}
            {hoveredRegion && (
              <div className="rounded-xl border border-slate-200 bg-gradient-to-br from-white to-slate-50 p-4 shadow-sm">
                <p className="text-sm font-semibold text-slate-900">{hoveredRegion.name}</p>
                <p className="text-3xl font-bold text-blue-700 mt-1">
                  {(hoveredRegion.estimate * 100).toFixed(1)}%
                </p>
              </div>
            )}
          </div>
        </aside>

        {/* Map */}
        <main className="flex-1 relative">
          <Map
            geoLevel={geoLevel}
            estimates={currentEstimates}
            selectedIssue={selectedIssue}
            onRegionHover={setHoveredRegion}
            dataRange={dataRange}
          />
        </main>
      </div>
    </div>
  );
}

export default function Home() {
  return (
    <I18nProvider>
      <HomePage />
    </I18nProvider>
  );
}
