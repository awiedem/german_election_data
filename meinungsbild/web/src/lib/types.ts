export interface Issue {
  issue_id: string;
  label: string;
  label_de: string;
  category: string;
  question_de: string;
  question_en: string;
  response_type: string;
  binary_rule: string;
  direction: string;
}

export interface KreisEstimate {
  county_code: string;
  county_name: string;
  state_code: string;
  state_name: string;
  estimate: number;
  pop: number;
}

export interface BundeslandEstimate {
  state_code: string;
  state_name: string;
  estimate: number;
  pop: number;
}

export interface WkrEstimate {
  wkr_nr: number;
  estimate: number;
  pop: number;
}

export type GeoLevel = "kreise" | "bundeslaender" | "wahlkreise";
