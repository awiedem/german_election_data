# Draft release-note fragment: NRW 1966/1970

Corrected the 185 Nordrhein-Westfalen district observations for the 1966 and
1970 state elections from the original official tables. The repair fixes
electorate, voters, valid/invalid votes, party attribution and shares, turnout,
and erroneous postal-only/excessive-turnout flags. All district values reconcile
with the printed regional and statewide totals. Statewide turnout is 76.5%
(1966) and 73.5% (1970).

The records describe historical counties and county-free cities, including
postal votes. Their existing synthetic, year-specific `050xx000` codes are
preserved and are not official municipal AGS. A source transcription and
district/code map accompany the repair. No counts remain unreadable.

Updated `state_unharm` CSV, RDS and Excel downloads. All other election rows are
unchanged. The harmonized downloads exclude these two NRW elections and require
no numeric update. Raw sources remain unchanged. See
[the repair and provenance note](nrw_1966_1970_repair.md) for source pages,
reconciliation, and reproducible validation.

Integration note: coordinate this fragment with the separate missingness and
historical-coverage work. This branch does not publish a release or update the
website/package files.
