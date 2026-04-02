"use client";

import { createContext, useContext, useState, useCallback, type ReactNode } from "react";

export type Locale = "de" | "en";

const translations = {
  de: {
    // Header
    siteTitle: "Meinungsbild",
    siteSubtitle: "Regionale Meinungsschätzungen für Deutschland",
    navMethodology: "Methodik",
    navData: "Daten",
    navAbout: "Über",

    // Sidebar
    labelTopic: "Thema",
    labelGeography: "Geografie",
    labelBundeslaender: "Bundesländer",
    labelWahlkreise: "Wahlkreise",
    labelKreise: "Kreise",
    labelQuestion: "Fragetext",
    labelScale: "Antwortskala",
    labelCoding: "Kodierung",
    labelDirection: "Interpretation",

    // Scale descriptions
    "scale_1_11": "Skala 1–11",
    "scale_1_7": "Skala 1–7",
    "scale_1_5": "Skala 1–5",
    "binary_yn": "Ja/Nein",

    // Direction labels
    "higher=more restrictive": "Höher = restriktiver",
    "higher=prioritize climate": "Höher = Klima priorisieren",
    "higher=more spending": "Höher = mehr Sozialausgaben",
    "higher=agree": "Höher = Zustimmung",
    "higher=support": "Höher = Zustimmung",
    "higher=support cut": "Höher = für Kürzung",
    "higher=support loosening": "Höher = für Lockerung",
    "higher=support ending": "Höher = für Beendigung",
    "higher=good": "Höher = positiv",
    "higher=satisfied": "Höher = zufrieden",
    "higher=will improve": "Höher = wird besser",
    "higher=improved": "Höher = verbessert",
    "higher=interested": "Höher = interessiert",
    "higher=just": "Höher = gerecht",
    "higher=more fear": "Höher = mehr Angst",
    "higher=more trust": "Höher = mehr Vertrauen",

    // Info panel
    noIssueSelected: "Wählen Sie ein Thema aus",
    regions: "Regionen",
    estimateRange: "Schätzungsbereich",

    // Categories
    "cat:affect": "Ängste",
    "cat:climate": "Klima",
    "cat:culture": "Kultur & Gesellschaft",
    "cat:digital": "Digitales",
    "cat:economy": "Wirtschaft",
    "cat:economy_perception": "Wirtschaftslage",
    "cat:energy": "Energie",
    "cat:engagement": "Politisches Engagement",
    "cat:eu": "Europa",
    "cat:foreign": "Außenpolitik",
    "cat:government": "Regierung",
    "cat:immigration": "Zuwanderung",
    "cat:institutions": "Institutionen & Demokratie",
    "cat:justice": "Gerechtigkeit",
    "cat:populism": "Populismus",
    "cat:social": "Soziales",
    "cat:transport": "Verkehr",
    "cat:trust": "Vertrauen",

    // Methodology page
    methodTitle: "Methodik",
    methodSubtitle: "Wie wir regionale Meinungsschätzungen erstellen",
    backToMap: "Zurück zur Karte",
  },
  en: {
    siteTitle: "Meinungsbild",
    siteSubtitle: "Subnational Opinion Estimates for Germany",
    navMethodology: "Methodology",
    navData: "Data",
    navAbout: "About",

    labelTopic: "Topic",
    labelGeography: "Geography",
    labelBundeslaender: "Federal States",
    labelWahlkreise: "Electoral Districts",
    labelKreise: "Counties",
    labelQuestion: "Question",
    labelScale: "Response Scale",
    labelCoding: "Coding",
    labelDirection: "Interpretation",

    "scale_1_11": "Scale 1–11",
    "scale_1_7": "Scale 1–7",
    "scale_1_5": "Scale 1–5",
    "binary_yn": "Yes/No",

    "higher=more restrictive": "Higher = more restrictive",
    "higher=prioritize climate": "Higher = prioritize climate",
    "higher=more spending": "Higher = more spending",
    "higher=agree": "Higher = agree",
    "higher=support": "Higher = support",
    "higher=support cut": "Higher = support cut",
    "higher=support loosening": "Higher = support loosening",
    "higher=support ending": "Higher = support ending",
    "higher=good": "Higher = positive",
    "higher=satisfied": "Higher = satisfied",
    "higher=will improve": "Higher = will improve",
    "higher=improved": "Higher = improved",
    "higher=interested": "Higher = interested",
    "higher=just": "Higher = just",
    "higher=more fear": "Higher = more fear",
    "higher=more trust": "Higher = more trust",

    noIssueSelected: "Select a topic",
    regions: "Regions",
    estimateRange: "Estimate range",

    "cat:affect": "Fears",
    "cat:climate": "Climate",
    "cat:culture": "Culture & Society",
    "cat:digital": "Digital",
    "cat:economy": "Economy",
    "cat:economy_perception": "Economic Perceptions",
    "cat:energy": "Energy",
    "cat:engagement": "Political Engagement",
    "cat:eu": "Europe",
    "cat:foreign": "Foreign Policy",
    "cat:government": "Government",
    "cat:immigration": "Immigration",
    "cat:institutions": "Institutions & Democracy",
    "cat:justice": "Justice",
    "cat:populism": "Populism",
    "cat:social": "Social Policy",
    "cat:transport": "Transport",
    "cat:trust": "Trust",

    methodTitle: "Methodology",
    methodSubtitle: "How we create subnational opinion estimates",
    backToMap: "Back to map",
  },
} as const;

type TranslationKey = keyof typeof translations.de;

interface I18nContextType {
  locale: Locale;
  setLocale: (l: Locale) => void;
  t: (key: string) => string;
}

const I18nContext = createContext<I18nContextType>({
  locale: "de",
  setLocale: () => {},
  t: (key: string) => key,
});

export function I18nProvider({ children }: { children: ReactNode }) {
  const [locale, setLocale] = useState<Locale>("de");

  const t = useCallback(
    (key: string): string => {
      const dict = translations[locale] as Record<string, string>;
      return dict[key] ?? key;
    },
    [locale]
  );

  return (
    <I18nContext.Provider value={{ locale, setLocale, t }}>
      {children}
    </I18nContext.Provider>
  );
}

export function useI18n() {
  return useContext(I18nContext);
}
