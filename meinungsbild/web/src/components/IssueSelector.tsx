"use client";

import { useI18n } from "@/lib/i18n";
import type { Issue } from "@/lib/types";

interface IssueSelectorProps {
  issues: Issue[];
  selected: string;
  onChange: (issueId: string) => void;
}

export default function IssueSelector({ issues, selected, onChange }: IssueSelectorProps) {
  const { locale, t } = useI18n();

  // Group by category
  const grouped = issues.reduce<Record<string, Issue[]>>((acc, issue) => {
    const cat = issue.category || "other";
    if (!acc[cat]) acc[cat] = [];
    acc[cat].push(issue);
    return acc;
  }, {});

  return (
    <select
      value={selected}
      onChange={(e) => onChange(e.target.value)}
      className="w-full rounded-lg border border-slate-200 bg-white px-3 py-2.5 text-sm shadow-sm focus:border-blue-500 focus:outline-none focus:ring-2 focus:ring-blue-500/20"
    >
      {Object.entries(grouped).map(([category, categoryIssues]) => (
        <optgroup key={category} label={t(`cat:${category}`)}>
          {categoryIssues.map((issue) => (
            <option key={issue.issue_id} value={issue.issue_id}>
              {locale === "de" ? issue.label_de : issue.label}
            </option>
          ))}
        </optgroup>
      ))}
    </select>
  );
}
