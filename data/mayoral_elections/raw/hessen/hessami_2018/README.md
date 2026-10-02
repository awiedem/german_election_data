# Hessen mayoral candidate names 1993–2012 (Hessami 2018)

**Source.** Hessami, Zohal (2018). "Accountability and Incentives of Appointed and
Elected Public Officials." *Review of Economics and Statistics* 100(1): 51–64.
Replication data: Harvard Dataverse, doi:[10.7910/DVN/FZWOMK](https://doi.org/10.7910/DVN/FZWOMK),
licence **CC0**. Downloaded 2026-10-01; files copied here verbatim.

| File | sha256 (first 16) | Content |
|---|---|---|
| `mayorelections.dta` | `d7d9ae18d0f86075` | 1,793 Bürgermeister/OB election rounds 1993–2012 in all 426 Gemeinden; up to 11 candidates per round with name, Wahlvorschlagsträger and votes; the elected person and their gender. Obtained by the author from the Hessisches Statistisches Landesamt, division "Wahlen und Bevölkerung". |
| `collected.dta` | `b14cd508f798a533` | The author's list of mayoral elections ("D") and council appointments ("I") with the elected mayor's name; runs into 2013. |
| `README.pdf` | | The author's description of the replication files. |

**Why the `.dta` files are not in git.** They name every candidate, including the
losers, who are private individuals. GERDA publishes the elected persons' names and
keeps the other names in the restricted twin only (`final_restricted/`), as for
Sachsen-Anhalt. The licence would allow redistribution (CC0); the restriction is a
data-protection choice. To rebuild, download the two files from the DOI above into
this folder.

**How the pipeline uses them.** `code/mayoral_elections/00_he_hessami_parse.py`
parses the names into `../he_hessami_parsed.csv` (gitignored).
`00_he_hist_parse.py` then grafts them onto the HSL historical workbook
(`Direktwahlen_in_Hessen_seit_1993.xlsx`). The two sources agree on every one of
the 1,793 rounds: same AGS, date and round, the candidates in the same order as the
workbook's Wahlvorschlag blocks, and identical votes. The graft is therefore keyed on
the ballot position and checks the votes.

- Winners' names go into the public `he_hist_parsed.csv`.
- All other names go into the gitignored `../he_hist_restricted_names.csv`, which
  01b joins into the restricted twin only.
- `collected.dta` contributes the names of 11 winners of 2013 elections. Each is
  checked against the workbook's winner party, gender and official term counters.

**Known slips in the source** (handled and reported by the parser):
- 14 of 4,345 name strings break the "Last, First" convention.
- 5 runoff names are spelled differently from their first round.
- One 1993 winner field names the second-placed candidate.
- Schlüchtern 2010-05-30 valid votes are 30 above the candidates' sum, the same
  slip as in the HSL workbook's printed shares.
