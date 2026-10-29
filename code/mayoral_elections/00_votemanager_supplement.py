#!/usr/bin/env python3
"""Offline HE/BW integration of the verified 2026-10-27 collection.

import-review reads the immutable research handoff and its saved JSON cache.
build regenerates pipeline inputs from the durable public numeric extract, with
an optional restricted twin. No network requests, guesses from percentages, or
changes to the original collector outputs. Aggregate/write-in and Nein totals
remain in the source extract; only individual candidates enter the pipeline.
"""
import argparse
import csv
import hashlib
import importlib.util
import json
import re
import unicodedata
from collections import Counter, defaultdict
from datetime import date
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / 'data/mayoral_elections/raw'
DEST = RAW / 'votemanager'
META = ROOT / 'data/mayoral_elections/metadata'
COUNTS = ['eligible_voters', 'number_voters', 'valid_votes', 'invalid_votes']
PERSONAL = ['candidate_name', 'candidate_last_name', 'candidate_first_name']
KEY = ['ags', 'election_date', 'round']


def read(path):
    with Path(path).open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))


def write(path, rows, fields=None):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = fields or list(dict.fromkeys(k for r in rows for k in r))
    with path.open('w', encoding='utf-8', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
        w.writeheader()
        w.writerows(rows)


def key(r):
    return tuple(r[c] for c in KEY)


def truth(x):
    return str(x).lower() == 'true'


def flag(x):
    return 'TRUE' if x else 'FALSE'


def norm(x):
    x = str(x).replace('ß', 'ss')
    x = unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode().lower()
    x = re.sub(r'\b(?:prof|dr|dipl|apl)\b\.?', '', x)
    return ' '.join(re.findall('[a-z]+', x))


spec = importlib.util.spec_from_file_location('old_bw', Path(__file__).with_name('00_bw_komm_one_scrape.py'))
old_bw = importlib.util.module_from_spec(spec)
spec.loader.exec_module(old_bw)


def split_name(name):
    name = re.sub(r'\s*\((?:Gemeinderat|Gemeinderätin)\)\s*', '', name).strip()
    if ',' in name:
        last, first = name.split(',', 1)
        first = re.sub(r'\b(?:Prof\.|Dr\.|apl\.)\s*', '', first).strip()
        last = re.sub(r'\b(?:Prof\.|Dr\.|apl\.)\s*', '', last).strip()
        return last.strip(), first
    name = re.sub(r'\bapl\.\s*', '', name)
    return old_bw.split_name(name)


def daydiff(a, b):
    return (date.fromisoformat(a) - date.fromisoformat(b)).days


def imported(review, cache):
    rounds = read(review / 'remaining24/all_collected_nonempty_rounds.csv')
    assert Counter(r['state'] for r in rounds) == {'06': 75, '08': 712}
    assert len({key(r) for r in rounds}) == len(rounds)
    groups = defaultdict(list)
    for filename in ['candidates_restricted.csv', 'recovery/legacy_candidates_restricted.csv',
                     'vpn_recheck/additional_candidates_restricted.csv']:
        for r in read(review / filename):
            groups[key(r)].append(r)
    for folder in ['municipal_archive', 'remaining24']:
        for r in read(review / folder / 'additional_candidates_restricted.csv'):
            k = key(r)
            groups[k].append(dict(r, state=r['ags'][:2], ballot_position=str(len(groups[k]) + 1),
                                  label_short=r['source_label'], label_long='', votes=r['candidate_votes']))

    # One municipal runoff title repeats its first-round date. Correct only
    # the pinned result URL, before linking people or deciding complete cycles.
    for fix in read(META / 'votemanager_source_date_corrections.csv'):
        matches = [r for r in rounds if r['source_url'] == fix['source_url']]
        assert len(matches) == 1
        rd = matches[0]
        assert (rd['ags'], rd['election_date'], rd['round']) == (fix['ags'], fix['old_election_date'], fix['round'])
        old_key = key(rd)
        rd['reported_election_date'] = rd['election_date']
        rd['election_date'] = fix['election_date']
        rd['date_source'] = 'verified_date_correction'
        rd['date_evidence_url'] = fix['evidence_url']
        assert key(rd) not in groups
        groups[key(rd)] = groups.pop(old_key)

    # Obtain full elected-person labels from the original JSON's winner cards;
    # the earlier collection CSV kept only the card title. On first rounds the
    # same component can list the two runoff candidates, so it is not itself a
    # winner flag. It supplies names only; election status is checked below.
    json_sources = {}
    for base in [cache, cache / 'vpn_recheck/legacy_api']:
        for m in read(base / 'source_manifest.csv'):
            if m.get('raw_file'):
                json_sources[m['url']] = (base / m['raw_file'], m['sha256'])
    classified = {r['ags']: r['election_type'] for r in read(RAW / 'baden_wuerttemberg/bw_parsed.csv')}
    old_names = {(r['ags'], r['election_date'], norm(r['candidate_name']))
                 for r in read(RAW / 'baden_wuerttemberg/bw_komm_one_parsed.csv')}
    old_names.update((r['ags'], r['election_date'], norm(r['candidate_name']))
                     for r in read(RAW / 'hessen/he2026_parsed.csv') if r['candidate_name'])
    candidates = []
    issues = []
    for rd in rounds:
        k = key(rd)
        rawrows = groups[k]
        assert len(rawrows) == int(rd['table_rows']), (k, len(rawrows), rd['table_rows'])
        items, outcome = [], ''
        if rd['source_url'] in json_sources:
            path, sha = json_sources[rd['source_url']]
            data = path.read_bytes()
            assert hashlib.sha256(data).hexdigest() == sha
            winner = json.loads(data).get('Komponente', {}).get('gewaehlte_kandidaten') or {}
            items = [re.sub(r'\s*\([^)]*\)\s*$', '', i.get('label', '')).strip()
                     for i in winner.get('items', [])]
            outcome = winner.get('title', '')
        normalized = []
        for r in rawrows:
            label = r['label_short'].strip()
            is_no = bool(re.fullmatch(r'Nein(?:-Stimmen)?', label, re.I))
            aggregate = truth(r['is_aggregate']) or bool(re.fullmatch(r'(?:Sonstige|Andere|freie Zeile|Zusammengefasste Bewerber|Vom Wähler vorgeschlagene Person(?:en)?|D\d+)', label, re.I))
            name, party = label, ''
            if rd['state'] == '06' and not is_no:
                name, sep, party = label.partition(',')
                name, party = name.strip(), party.strip()
            if aggregate or is_no:
                name = ''
            # Scheer's bulletin prints "Surname Given, Residence".
            if rd.get('source_file') == 'scheer_2024_kw08.pdf' and name:
                surname, given = name.split(',')[0].strip().split(' ', 1)
                name = f'{surname}, {given}'
            # A unique full card label can expand an abbreviated surname. Every
            # original name token must occur in the card; no fuzzy surname join.
            matches = [x for x in items if norm(name) and set(norm(name).split()) <= set(norm(x).split())]
            if len(matches) == 1 and len(norm(matches[0])) > len(norm(name)):
                name = matches[0]
            last, first = split_name(name) if name else ('', '')
            if rd['state'] == '06' and party:
                # A self-named Einzelbewerbung is a list type, not a party. Keep
                # own-name labels out of the public candidate_party field.
                if 'einzelbewer' in party.lower() or norm(party) in {norm(last), norm(name)}:
                    party = 'Einzelbewerbung'
            row = {c: rd[c] for c in KEY + ['ags_name', 'state'] + COUNTS}
            row.update(ballot_position=int(r['ballot_position']), candidate_name=name,
                       candidate_last_name=last, candidate_first_name=first, candidate_party=party,
                       candidate_votes=int(r['votes']), is_aggregate=flag(aggregate), is_no=flag(is_no),
                       candidate_voteshare=int(r['votes']) / int(rd['valid_votes']),
                       election_type=classified.get(rd['ags'], 'Bürgermeisterwahl'),
                       source_url=rd['source_url'], source_label=label, outcome_title=outcome,
                       outcome_labels=' | '.join(items), name_was_public=flag((rd['ags'], rd['election_date'], norm(name)) in old_names))
            if rd['state'] == '06':
                # This collection contains no HE Oberbürgermeister/Landrat race.
                assert not rd['ags'].endswith('000')
            normalized.append(row)
        assert sum(x['candidate_votes'] for x in normalized) == int(rd['table_vote_sum'])
        individual = [x for x in normalized if not truth(x['is_aggregate']) and not truth(x['is_no'])]
        assert individual, k
        for rank, r in enumerate(sorted(individual, key=lambda x: -x['candidate_votes']), 1):
            r['candidate_rank'] = rank
        leaders = [x for x in individual if x['candidate_votes'] == max(y['candidate_votes'] for y in individual)]
        assert len(leaders) == 1, ('tied leaders require a source decision', k)
        lead = leaders[0]
        decisive = rd['round'] == 'stichwahl' or lead['candidate_votes'] * 2 > int(rd['valid_votes'])
        elected_items = items if re.search(r'gewählt', outcome, re.I) and not re.search(r'Stichwahl|Kein Kandidat', outcome, re.I) else []
        if elected_items and len(elected_items) == 1:
            assert set(norm(lead['candidate_name']).split()) <= set(norm(elected_items[0]).split()), (k, 'winner card contradicts votes')
            assert decisive, (k, 'winner card without decisive majority')
        rd['round_decisive'] = flag(decisive)
        for r in normalized:
            r.update(n_candidates=len(individual), is_winner=flag(decisive and r is lead),
                     winner_evidence='source_card_and_votes' if elected_items else 'unique_majority_or_runoff_plurality',
                     cycle_start=rd['election_date'], pair_id=f"{rd['ags']}_{rd['election_date']}_{r['ballot_position']}")
        candidates.extend(normalized)
        if rd['check'] != 'PASS':
            issues.append({c: rd[c] for c in KEY + ['ags_name', 'check', 'source_url']})

    first_dates = {(r['ags'], r['election_date']) for r in rounds if r['round'] == 'hauptwahl'}
    assert not any(r['round'] == 'stichwahl' and (r['ags'], r['election_date']) in first_dates for r in rounds), "runoff repeats first-round date"
    by_round = defaultdict(list)
    for c in candidates:
        by_round[key(c)].append(c)
    # Link distinct election cycles, including the two BW municipality-years
    # with more than one first round. Do not group elections by calendar year.
    for sw in sorted(rounds, key=lambda r: r['election_date']):
        if sw['round'] != 'stichwahl':
            continue
        prior = [h for h in rounds if h['ags'] == sw['ags'] and h['round'] == 'hauptwahl'
                 and 0 < daydiff(sw['election_date'], h['election_date']) <= 120]
        if not prior:
            continue
        hw = max(prior, key=lambda x: x['election_date'])
        sw['cycle_start'] = hw['election_date']
        for s in by_round[key(sw)]:
            s['cycle_start'] = hw['election_date']
            if truth(s['is_aggregate']) or truth(s['is_no']):
                continue
            matches = [h for h in by_round[key(hw)] if norm(h['candidate_name']) == norm(s['candidate_name'])
                       and norm(s['candidate_name'])]
            if not matches:
                # Winner cards may expand the same abbreviated surname in only
                # one round. Require identical surname and compatible first name.
                matches = [h for h in by_round[key(hw)] if norm(h['candidate_last_name']) == norm(s['candidate_last_name'])
                           and (not norm(h['candidate_first_name']) or not norm(s['candidate_first_name'])
                                or norm(h['candidate_first_name']) == norm(s['candidate_first_name']))]
            if len(matches) == 1:
                h = matches[0]
                s['pair_id'] = h['pair_id']
                # Propagate the more complete source-backed name, keeping the
                # verbatim labels in the restricted evidence extract.
                fuller = max([h, s], key=lambda x: len(norm(x['candidate_name'])))
                for x in [h, s]:
                    for field in PERSONAL:
                        x[field] = fuller[field]
            elif sw['state'] == '06':
                raise AssertionError(('unsettled HE runoff candidate', key(sw), s['ballot_position']))
    exclusions = {(r['ags'], r['cycle_start']): r for r in read(META / 'votemanager_cycle_exclusions.csv')}
    for r in candidates:
        exception = exclusions.get((r['ags'], r['cycle_start']), {})
        for f in ['flag_superseded', 'flag_decisive_round_missing']:
            r[f] = exception.get(f, 'FALSE')
    winners = {r['pair_id'] for r in candidates if truth(r['is_winner'])
               and not truth(r['flag_decisive_round_missing'])}
    # A losing candidate keeps a public name only if it was public in every
    # round of theirs (user decision 2026-10-29; Dettenheim 2023).
    unpublished = {r['pair_id'] for r in candidates if r['candidate_name'] and not truth(r['name_was_public'])}
    for r in candidates:
        r['cycle_winner'] = flag(r['pair_id'] in winners)
        r['name_withheld'] = flag(bool(r['candidate_name']) and r['pair_id'] not in winners and r['pair_id'] in unpublished)
        if truth(r['cycle_winner']) and not r['candidate_first_name']:
            issues.append(dict(ags=r['ags'], election_date=r['election_date'], round=r['round'],
                               ags_name=r['ags_name'], check='winner_given_name_missing', source_url=r['source_url']))
    for r in rounds:
        r.setdefault('cycle_start', r['election_date'])
    write(DEST / 'source_rounds.csv', rounds)
    write(DEST / 'source_candidates_restricted.csv', candidates)
    public = []
    for r in candidates:
        x = {k: v for k, v in r.items() if k not in {'source_label', 'outcome_title', 'outcome_labels'}}
        if truth(r['name_withheld']):
            for field in PERSONAL:
                x[field] = ''
        public.append(x)
    write(DEST / 'source_candidates_public.csv', public)
    write(META / 'votemanager_collection_flags.csv', issues)
    write(META / 'votemanager_registry_review.csv', read(review / 'remaining24/registry_followup.csv'))
    write(META / 'votemanager_schorndorf_flag.csv', read(review / 'remaining24/partial_rounds.csv'))
    for source, target in [('recovery/legacy_stala_conflicts.csv', 'votemanager_stala_conflicts_html.csv'),
                           ('vpn_recheck/stala_conflicts.csv', 'votemanager_stala_conflicts_json.csv')]:
        write(META / target, read(review / source))
    provenance = []
    manifest_fields = ['source_url', 'final_url', 'sha256', 'retrieved_at', 'content_check']
    for folder in ['municipal_archive', 'remaining24']:
        for m in read(review / folder / 'source_manifest.csv'):
            provenance.append({f: m.get(f, '') for f in manifest_fields})
    for base in [cache, cache / 'vpn_recheck/legacy_api']:
        for m in read(base / 'source_manifest.csv'):
            if m.get('sha256'):
                provenance.append(dict(source_url=m['url'], final_url=m.get('final_url', ''),
                    sha256=m['sha256'], retrieved_at=m.get('retrieved_at', ''), content_check=m.get('status', '')))
    write(DEST / 'source_manifest.csv', provenance, manifest_fields)
    print(json.dumps({'source_rounds': len(rounds), 'source_rows': len(candidates), 'issues': issues}, ensure_ascii=False, indent=2))


def build():
    rounds = read(DEST / 'source_rounds.csv')
    public = read(DEST / 'source_candidates_public.csv')
    candidates = read(DEST / 'source_candidates_restricted.csv') if (DEST / 'source_candidates_restricted.csv').exists() else public
    assert [(key(x), x['ballot_position']) for x in public] == [(key(x), x['ballot_position']) for x in candidates]
    # The restricted companion is allowed to add labels, never change counts,
    # identities between rounds, elected status or the public release decision.
    for p, c in zip(public, candidates):
        for f in set(p) - set(PERSONAL):
            assert p[f] == c[f], (key(p), f)
    rg = {key(r): r for r in rounds}
    by_round = defaultdict(list)
    for c in candidates:
        by_round[key(c)].append(c)
    for k, rr in by_round.items():
        rd = rg[k]
        if rd['check'] == 'PASS':
            assert sum(int(c['candidate_votes']) for c in rr) + int(rd['no_votes'] or 0) == int(rd['valid_votes'])
            assert int(rd['valid_votes']) + int(rd['invalid_votes']) == int(rd['number_voters']) <= int(rd['eligible_voters'])
    # Where a municipal table's counts disagree with StaLA, StaLA takes
    # precedence unless the Gemeinde's final notice confirms the table (user
    # decision 2026-10-29). Such a round falls back to StaLA like Schorndorf;
    # a decisive one takes its whole cycle with it. One exception keeps the
    # table where StaLA demonstrably omits a district (Küssaberg 2021).
    conflicts = {key(r) for f in ['votemanager_stala_conflicts_html.csv', 'votemanager_stala_conflicts_json.csv']
                 for r in read(META / f)}
    conflicts |= {key(r) for r in read(META / 'votemanager_stala_conflicts_additional.csv')
                  if r['decision'] == 'stala_conflict'}
    decisions = {key(r): r['decision'] for r in read(META / 'votemanager_stala_conflict_decisions.csv')}
    assert set(decisions) == conflicts and set(decisions.values()) <= {'stala', 'municipal_final_notice', 'municipal_stala_omits_district'}
    stala_first = {k for k, d in decisions.items() if d == 'stala'}
    assert all(rg[k]['check'] == 'PASS' for k in stala_first)
    accepted = [c.copy() for c in candidates if rg[key(c)]['check'] == 'PASS' and key(c) not in stala_first
                and not truth(c['is_aggregate']) and not truth(c['is_no'])]
    for c in accepted:
        c.update(election_year=c['election_date'][:4],
                 state_name='Hessen' if c['state'] == '06' else 'Baden-Württemberg',
                 turnout=round(int(c['number_voters']) / int(c['eligible_voters']), 6),
                 source='votemanager_municipal_2026-10-27')
        for f in ['source_label', 'outcome_title', 'outcome_labels']:
            c.pop(f, None)
    # Leonberg's election result uses the pre-2019 surname. Keep the published
    # canonical identity in the pipeline; the original label remains in raw.
    for c in accepted:
        if c['ags'] == '08115028' and c['election_date'] == '2017-09-24' and truth(c['cycle_winner']):
            assert norm(c['candidate_name']) in {'martin kaufmann', 'cohn martin'}
            c.update(candidate_name='Cohn, Martin', candidate_last_name='Cohn', candidate_first_name='Martin')
    # Determine complete BW cycles from accepted rounds, not municipality-year.
    complete = {(r['ags'], r['cycle_start']) for r in rounds if r['state'] == '08'
                and r['check'] == 'PASS' and key(r) not in stala_first and truth(r['round_decisive'])}
    for c in accepted:
        c['cycle_complete'] = flag((c['ags'], c['cycle_start']) in complete)
    # StaLA supplies a winner-only first-round shell for some recovered runoffs
    # (notably Schorndorf and Schwäbisch Hall). Give that shell the same opaque
    # link so rebuilding without restricted names produces identical pairings.
    fallback_keys = []
    accepted_dates = {(c['ags'], c['election_date']) for c in accepted if truth(c['cycle_complete'])}
    stala = read(RAW / 'baden_wuerttemberg/bw_parsed.csv')
    for c in accepted:
        if c['state'] != '08' or c['round'] != 'stichwahl' or not truth(c['is_winner']) or truth(c['flag_decisive_round_missing']):
            continue
        possible = [h for h in stala if h['ags'] == c['ags'] and h['round'] == 'hauptwahl'
                    and (h['ags'], h['election_date']) not in accepted_dates
                    and 0 < daydiff(c['election_date'], h['election_date']) <= 120]
        if not possible:
            continue
        h = max(possible, key=lambda x: x['election_date'])
        assert norm(h['candidate_last_name']) == norm(c['candidate_last_name']), (key(c), 'fallback surname')
        assert norm(h['candidate_first_name']).split()[0] == norm(c['candidate_first_name']).split()[0], (key(c), 'fallback given name')
        fallback_keys.append({**{f: h[f] for f in KEY}, 'pair_key': c['pair_id']})
    write(RAW / 'baden_wuerttemberg/bw_komm_one_fallback_pair_keys.csv', fallback_keys, KEY + ['pair_key'])
    for state, subdir, filename in [('06', 'hessen', 'he_votemanager_parsed'),
                                    ('08', 'baden_wuerttemberg', 'bw_komm_one_augmented')]:
        rows = [c for c in accepted if c['state'] == state]
        names = [{k: c[k] for k in KEY + ['ballot_position'] + PERSONAL}
                 for c in rows if truth(c['name_withheld'])]
        pubrows = []
        for c in rows:
            x = c.copy()
            if truth(c['name_withheld']):
                for f in PERSONAL:
                    x[f] = ''
            pubrows.append(x)
        write(RAW / subdir / (filename + '.csv'), pubrows)
        write(RAW / subdir / (filename + '_names_restricted.csv'), names, KEY + ['ballot_position'] + PERSONAL)
        print(state, len({key(r) for r in rows}), 'accepted rounds;', len(rows), 'individual candidate rows;', len(names), 'withheld labels')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['import-review', 'build'])
    parser.add_argument('--review', type=Path)
    parser.add_argument('--cache', type=Path)
    args = parser.parse_args()
    if args.command == 'import-review':
        assert args.review and args.cache
        imported(args.review, args.cache)
    else:
        build()
