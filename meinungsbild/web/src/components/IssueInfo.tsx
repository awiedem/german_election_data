"use client";

import { useI18n } from "@/lib/i18n";
import type { Issue } from "@/lib/types";

interface IssueInfoProps {
  issue: Issue;
  dataRange: { min: number; max: number };
  regionCount: number;
}

function formatBinaryRule(rule: string | undefined, responseType: string, locale: string): string {
  if (!rule) return "—";

  const match = rule.match(/y=1 if ([<>]=?) (\d+)/);
  if (!match) return rule;

  const [, operator, threshold] = match;
  const thresholdNum = parseInt(threshold);

  const scaleMax = responseType === "scale_1_11" ? 11 : responseType === "scale_1_7" ? 7 : 5;
  const isLow = operator.startsWith("<");
  const rangeStr = isLow ? `1–${thresholdNum}` : `${thresholdNum}–${scaleMax}`;

  if (locale === "de") {
    return `Wert ${rangeStr} auf Skala 1–${scaleMax}`;
  }
  return `Score ${rangeStr} on scale 1–${scaleMax}`;
}

export default function IssueInfo({ issue, dataRange, regionCount }: IssueInfoProps) {
  const { locale, t } = useI18n();

  const questionText = locale === "en"
    ? (issue.question_en || issue.question_de)
    : issue.question_de;

  return (
    <div className="space-y-3">
      {/* Issue title */}
      <div>
        <h2 className="text-base font-semibold text-slate-900">
          {locale === "de" ? issue.label_de : issue.label}
        </h2>
      </div>

      {/* Question text */}
      <div className="rounded-lg border border-blue-100 bg-blue-50/50 p-3">
        <p className="text-[11px] font-semibold uppercase tracking-wider text-blue-600/70 mb-1">
          {t("labelQuestion")}
        </p>
        <p className="text-sm leading-relaxed text-slate-700">{questionText}</p>
      </div>

      {/* Scale + coding info */}
      <div className="grid grid-cols-2 gap-2">
        <div className="rounded-lg bg-slate-50 p-2.5">
          <p className="text-[10px] font-semibold uppercase tracking-wider text-slate-400 mb-0.5">
            {t("labelScale")}
          </p>
          <p className="text-xs font-medium text-slate-700">
            {t(issue.response_type)}
          </p>
        </div>
        <div className="rounded-lg bg-slate-50 p-2.5">
          <p className="text-[10px] font-semibold uppercase tracking-wider text-slate-400 mb-0.5">
            {t("labelCoding")}
          </p>
          <p className="text-xs font-medium text-slate-700">
            {formatBinaryRule(issue.binary_rule, issue.response_type, locale)}
          </p>
        </div>
      </div>

      {/* Direction */}
      <div className="rounded-lg bg-slate-50 p-2.5">
        <p className="text-[10px] font-semibold uppercase tracking-wider text-slate-400 mb-0.5">
          {t("labelDirection")}
        </p>
        <p className="text-xs font-medium text-slate-700">
          {t(issue.direction)}
        </p>
      </div>

      {/* Stats */}
      <div className="flex items-center gap-3 pt-1 text-xs text-slate-500">
        <span>{regionCount} {t("regions")}</span>
        <span className="text-slate-300">|</span>
        <span>
          {t("estimateRange")}: {(dataRange.min * 100).toFixed(0)}% – {(dataRange.max * 100).toFixed(0)}%
        </span>
      </div>
    </div>
  );
}
