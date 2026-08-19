## normalise_party: map raw party name → snake_case column name ------------
## Aligned with federal elections pipeline naming where possible.
normalise_party <- function(pname) {
  p <- trimws(pname)
  p_up <- toupper(p)
  # Compact form: strip internal spaces/dots so letter-spaced acronyms used by
  # some Wahlkreis sources (RP writes "C D U", "S P D", "F D P", "N P D", "R E P",
  # "P B C", "F W G ...") collapse to the canonical party. Exact-match only (safe).
  p_cmp <- gsub("[[:space:].]+", "", p_up)
  if (p_cmp == "CDU")                                   return("cdu")
  if (p_cmp == "CSU")                                   return("csu")
  if (p_cmp == "SPD")                                   return("spd")
  if (p_cmp %in% c("FDP", "FDPDVP"))                    return("fdp")
  if (p_cmp == "NPD")                                   return("npd")
  if (p_cmp == "DVU")                                   return("dvu")
  if (p_cmp == "DKP")                                   return("dkp")
  if (p_cmp == "PBC")                                   return("pbc")
  if (p_cmp == "REP")                                   return("rep")
  if (p_cmp == "SSW")                                   return("ssw")
  if (grepl("^FWG", p_cmp))                             return("freie_wahler")
  # Major parties
  if (p_up == "CDU")                                    return("cdu")
  if (grepl("CHRISTLICH.DEMOKRATISCHE UNION", p_up))    return("cdu")
  if (p_up == "CSU")                                    return("csu")
  if (grepl("CHRISTLICH.SOZIALE UNION", p_up))          return("csu")
  if (p_up == "SPD")                                    return("spd")
  if (grepl("SOZIALDEMOKRATISCHE PARTEI", p_up))        return("spd")
  # Greens (incl. Grüne-led 1990 East coalitions BÜ90/GRÜNE, GRÜ-NF).
  # NB: "^DIE GR[UÜ]NE" (not "^DIE GR") so "Die Grauen" (grey/pensioners) is NOT
  # swallowed here and correctly falls through to the `graue` rule below.
  if (grepl("^GR[UÜ]NE|^GR[UÜ].?NF|^B[UÜ]NDNIS\\s*90.*GR|B[UÜ].*90.*GR[UÜ]|^DIE GR[UÜ]NE", p_up)) return("gruene")
  if (grepl("^FDP|^F[.]D[.]P", p_up))                  return("fdp")
  if (grepl("^FDP/DVP", p_up))                          return("fdp")
  if (grepl("FREIE DEMOKRATISCHE PARTEI", p_up))        return("fdp")
  if (grepl("LINKE|^PDS|^LL.PDS|^DIE LINKE", p_up))    return("linke_pds")
  if (grepl("^AFD$|^AFD[[:space:]]|ALTERNATIVE F[UÜ]R DEUTSCHLAND", p_up)) return("afd")
  if (grepl("^BSW$|^BSW[[:space:]]|WAGENKNECHT", p_up)) return("bsw")
  # Far right
  if (grepl("^NPD", p_up))                              return("npd")
  if (grepl("^REP$|REPUBLIKANER", p_up))                 return("rep")
  if (grepl("^DVU$", p_up))                              return("dvu")
  if (grepl("^III[.]|^DER III", p_up))                   return("iii_weg")
  if (grepl("^DIE.?RECHTE", p_up))                        return("die_rechte")
  if (grepl("HEIMAT$", p_up))                            return("npd")
  # Far left
  if (grepl("^DKP$", p_up))                              return("dkp")
  if (grepl("DEUTSCHE KOMMUNISTISCHE PARTEI", p_up))     return("dkp")
  if (grepl("^MLPD$", p_up))                             return("mlpd")
  if (grepl("^SGP$", p_up))                              return("sgp")
  # Smaller parties (aligned with federal pipeline)
  if (grepl("^PIRATEN", p_up))                           return("piraten")
  if (grepl("^FREIE W[AÄ]HLER|^FW$|^FWG", p_up))       return("freie_wahler")
  if (grepl("^BVB.*FREIE", p_up))                        return("freie_wahler")
  if (grepl("[OÖ]DP$|^[OÖ]DP[[:space:]]", p_up))        return("odp")
  if (grepl("[OÖ]KOLOGISCH.{0,4}DEMOKRATISCHE", p_up)) return("odp")  # tolerate "-"/"- "/word-wrap
  if (grepl("^DIE.?PARTEI$|^PARTEI$", p_up))              return("die_partei")
  # Die PARTEI's FULL legal name ("Partei für Arbeit, Rechtsstaat, Tierschutz,
  # Elitenförderung und basisdemokratische Initiative", used verbatim by Saarland)
  # must be caught here, BEFORE the TIERSCHUTZ catch-all below, or it collapses into
  # the Tierschutzpartei. "Elitenförderung" is a unique token for this party.
  if (grepl("ELITENF[OÖ]RDERUNG|RECHTSSTAAT.*TIERSCHUTZ.*ELITEN", p_up)) return("die_partei")
  if (grepl("TIER.?SCHUTZ.*ALLIANZ", p_up))              return("tierschutzallianz")
  if (grepl("TIERSCHUTZ.*HIER", p_up))                   return("tierschutz_hier")
  if (grepl("TIERSCHUTZ", p_up))                          return("tierschutz")
  if (grepl("^V.PARTEI", p_up))                          return("v_partei3")
  if (grepl("^SSW$", p_up))                              return("ssw")
  if (grepl("^VOLT$", p_up))                             return("volt")
  if (grepl("^DIEBASIS|^DIE BASIS", p_up))               return("diebasis")
  if (grepl("B[UÜ]NDNIS\\s*C$|^B[UÜ]NDNIS C[[:space:]]", p_up)) return("bundnis_c")
  if (grepl("B[UÜ]NDNIS.*DEUTSCHLAND", p_up))            return("bundnis_deutschland")
  if (grepl("^FAMILIE$|^FAMILIEN", p_up))                 return("familie")
  if (grepl("^BAYERNPARTEI$|^BP$", p_up))                 return("bp")
  if (grepl("^SCHILL|^PRO$|^RECHTSSTAATLICHE|RECHTSSTAATLICH.*OFFENSIVE", p_up)) return("schill")
  if (grepl("^STATT", p_up))                              return("statt_partei")
  if (grepl("^GRAUE|^DIE GRAUEN", p_up))                  return("graue")
  if (grepl("^ZENTRUM$", p_up))                            return("zentrum")
  if (grepl("^FREIE SACHSEN", p_up))                       return("freie_sachsen")
  if (grepl("WERTEUNION|^WU$", p_up))                     return("werteunion")
  if (grepl("^TEAM TODENHÖFER|^TEAM TODENH", p_up))       return("team_todenhofer")
  if (grepl("^HUMANISTEN|^DIE HUMANISTEN", p_up))          return("die_humanisten")
  if (grepl("^VIOLETTEN|^DIE VIOLETTEN", p_up))            return("violetten")
  if (grepl("^VOLKSAB.?STIMMUNG", p_up))                   return("volksabstimmung")
  if (grepl("^LKR$|^LIBERAL.KONSERVATIV", p_up))           return("lkr")
  if (grepl("^PRO DEUTSCHLAND|^PRO NRW|^PRO CHEMNITZ", p_up)) return("pro_deutschland")
  if (grepl("^RENTNER|^RRP", p_up))                        return("rentner")
  if (grepl("^BIG$", p_up))                                return("big")
  if (grepl("^UNABH[AÄ]NGIGE", p_up))                     return("unabhangige")
  if (grepl("^AUFBRUCH", p_up))                            return("aufbruch")
  if (grepl("^BFB$|^B[UÜ]RGER F[UÜ]R", p_up))            return("bfb")
  if (grepl("^BGE$", p_up))                                return("bge")
  if (grepl("^B[UÜ]SO$|B[UÜ]RGERRECHTSBEWEGUNG.SOLIDARIT", p_up)) return("bueso")
  if (grepl("GESUNDHEITS.*FORSCHUNG", p_up))                return("gesundheitsforschung")
  if (grepl("^MERA25", p_up))                              return("mera25")
  if (grepl("^PATRIOTEN", p_up))                            return("patrioten")
  if (grepl("^PBC$", p_up))                                return("pbc")
  if (grepl("PARTEI BIBELTREUER CHRISTEN", p_up))          return("pbc")
  if (grepl("^PDV$|^PARTEI DER VERNUNFT", p_up))           return("partei_der_vernunft")
  if (grepl("^DSU$", p_up))                                return("dsu")
  if (grepl("^AB JETZT", p_up))                             return("ab_jetzt")
  if (grepl("^AD.?DEMOKRATEN", p_up))                        return("ad_demokraten")
  if (grepl("^DIB$|^DEMOKRATIE IN BEWEGUNG", p_up))         return("dib")
  if (grepl("^DM$|^DEUTSCHE MITTE", p_up))                   return("dm")
  if (grepl("^MENSCHLICHE WELT", p_up))                      return("menschliche_welt")
  # Oekologische Linke. NRW writes "OekoLinX" (2010, 2022), Hessen "OekoLinX
  # Hessen" - the same party with a state suffix, like "FREIE WAEHLER
  # Mecklenburg-Vorpommern" above, so both land in `oekolinx`. NRW's earlier
  # "OekoLi" (2000, 2005) is deliberately NOT folded in here: that would move
  # already-published NRW columns, and audit section 12 already surfaces the
  # oekoli/oekolinx split for a separate decision.
  if (grepl("^[OÖ]KOLINX", p_up))                            return("oekolinx")
  if (grepl("GARTENPARTEI", p_up))                            return("gartenpartei")
  if (grepl("^NATUR.?GESETZ", p_up))                          return("naturgesetz")
  if (grepl("^DA$|^DEMOKRATIEALTERNATIVE", p_up))              return("da")
  if (grepl("^50.?PLUS", p_up))                                return("50plus")
  if (grepl("^GB.?BHE", p_up))                                 return("gb_bhe")
  if (grepl("^B[UÜ]NDNIS.?DKP.?KPD", p_up))                   return("buendnis_dkp_kpd")
  if (grepl("^OFFEN.*SIVE.*D", p_up))                          return("offensive_d")
  if (grepl("^CHR.?L$", p_up))                                 return("chr_l")
  # source-side residual / grouped labels, EXACT match only ("Sonstige Partei"
  # must keep its own key): the SL long-series table and HE 2013 report a
  # combined "Sonstige" figure, SH 2005 the same thing as "Andere"; all map to
  # the one source-residual column. SL additionally groups "Wählergruppen"
  if (p_cmp == "SONSTIGE")                                     return("sonstige")
  if (p_cmp == "ANDERE")                                       return("sonstige")
  if (p_cmp == "WÄHLERGRUPPEN")                                return("waehlergruppen")
  # Fallback: clean to snake_case (warn about potential collisions)
  cleaned <- tolower(p)
  cleaned <- gsub("[^a-z0-9äöüß]+", "_", cleaned)
  cleaned <- gsub("ä", "ae", cleaned)
  cleaned <- gsub("ö", "oe", cleaned)
  cleaned <- gsub("ü", "ue", cleaned)
  cleaned <- gsub("ß", "ss", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  message(sprintf("  normalise_party fallback: '%s' -> '%s' (add explicit mapping to avoid collisions)", p, cleaned))
  return(cleaned)
}
