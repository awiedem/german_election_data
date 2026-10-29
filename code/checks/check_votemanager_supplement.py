#!/usr/bin/env python3
"""Regression checks for the municipal supplement and its built public outputs."""
import csv
import importlib.util
import shutil
import tempfile
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / 'data/mayoral_elections/raw'
FINAL = ROOT / 'data/mayoral_elections/final'
META = ROOT / 'data/mayoral_elections/metadata'


def read(path):
    with path.open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))


def key(r):
    return r['ags'], r['election_date'], r['round']


def main():
    source = read(RAW / 'votemanager/source_candidates_public.csv')
    rounds = read(RAW / 'votemanager/source_rounds.csv')
    assert Counter(r['state'] for r in rounds) == {'06': 75, '08': 712}
    grouped = defaultdict(list)
    for r in source:
        grouped[key(r)].append(r)
        assert 'source_label' not in r and 'outcome_labels' not in r
        if r['name_withheld'] == 'TRUE':
            assert all(not r[c] for c in ['candidate_name', 'candidate_first_name', 'candidate_last_name'])
    for r in rounds:
        if r['check'] == 'PASS':
            assert sum(int(c['candidate_votes']) for c in grouped[key(r)]) + int(r['no_votes'] or 0) == int(r['valid_votes'])
    he = read(RAW / 'hessen/he_votemanager_parsed.csv')
    bw = read(RAW / 'baden_wuerttemberg/bw_komm_one_augmented.csv')
    assert len({key(r) for r in he}) == 75
    assert len({key(r) for r in bw}) == 702
    assert len({key(r) for r in bw if r['cycle_complete'] == 'TRUE'}) == 700
    excluded = {('08317029', '2024-10-13', 'hauptwahl'),
                ('08127076', '2021-07-04', 'hauptwahl'),
                ('08119067', '2021-11-07', 'hauptwahl')}
    assert not excluded.intersection(key(r) for r in bw)
    schorndorf = read(META / 'votemanager_schorndorf_flag.csv')
    assert len(schorndorf) == 1 and schorndorf[0]['check'] == 'CANDIDATE_TABLE_ONLY_CONFLICT'
    unharm = {key(r): r for r in read(FINAL / 'mayoral_unharm.csv')}
    for r in he + [r for r in bw if r['cycle_complete'] == 'TRUE']:
        u = unharm[key(r)]
        for c in ['eligible_voters', 'number_voters', 'valid_votes', 'invalid_votes']:
            assert float(u[c]) == float(r[c]), (key(r), c)
    for k in excluded:
        assert k in unharm, ('StaLA fallback lost', k)
    assert float(unharm[('08119067', '2021-11-07', 'hauptwahl')]['valid_votes']) == 13570
    for fix in read(META / 'bw_municipal_date_corrections.csv'):
        assert (fix['ags'], fix['old_election_date'], fix['old_round']) not in unharm
        assert (fix['ags'], fix['election_date'], fix['round']) in unharm
    assert ('08325009', '2022-10-16', 'stichwahl') not in unharm
    # Every StaLA/municipal count conflict follows its recorded decision: StaLA,
    # or the municipal table where a final notice confirms it.
    decisions = {key(r): r['decision'] for r in read(META / 'votemanager_stala_conflict_decisions.csv')}
    conflicts = read(META / 'votemanager_stala_conflicts_html.csv') + read(META / 'votemanager_stala_conflicts_json.csv')
    conflicts += [dict(r, portal_value=r['municipal_value']) for r in read(META / 'votemanager_stala_conflicts_additional.csv')
                  if r['decision'] == 'stala_conflict']
    assert {key(r) for r in conflicts} == set(decisions) and len(decisions) == 16
    for r in conflicts:
        want = r['stala_value'] if decisions[key(r)] == 'stala' else r['portal_value']
        assert float(unharm[key(r)][r['field']]) == float(want), (key(r), r['field'], decisions[key(r)])
    stala = read(RAW / 'baden_wuerttemberg/bw_parsed.csv')
    for rows in [stala, bw]:
        d = [r for r in rows if r['ags'] == '08327010' and r['election_date'] == '2023-06-18' and r['candidate_last_name'] == 'Biselli']
        assert len(d) == 1 and int(d[0]['candidate_votes']) == 887 and int(d[0]['valid_votes']) == 1301
    candidates = read(FINAL / 'mayoral_candidates.csv')
    dc = [r for r in candidates if r['ags'] == '08327010' and r['election_date'] == '2023-06-18' and r['candidate_last_name'] == 'Biselli']
    assert len(dc) == 1 and float(dc[0]['candidate_votes_hw']) == 887
    # Both distinct contests in the same municipality-year must survive.
    for ags, dates in [('08127063', {'2021-03-14', '2021-09-12'}),
                       ('08317150', {'2022-05-15', '2022-08-14'}),
                       ('06432015', {'2026-03-15', '2026-08-09'})]:
        assert dates <= {r['election_date'] for r in candidates if r['ags'] == ags}
    panel = read(FINAL / 'mayor_panel.csv')
    for ags, d in [('08127063', '2021-03-14'), ('08317150', '2022-05-15')]:
        failed = [r for r in candidates if r['ags'] == ags and r['election_date'] == d]
        assert failed and all(r['flag_decisive_round_missing'] == 'TRUE' and r['is_winner'] in ('NA', '') for r in failed)
        assert not any(r['ags'] == ags and r['election_date'] == d for r in panel)
    for correction in read(META / 'votemanager_person_id_date_corrections.csv'):
        hit = [r for r in panel if r['ags'] == correction['ags'] and r['election_date'] == correction['election_date']]
        assert len(hit) == 1 and hit[0]['person_id'] == correction['person_id']
    assert all(r['valid_votes'] not in ('', 'NA') for r in unharm.values() if r['ags'].startswith('06') and r['election_date'].startswith('2026'))
    # Rebuild in isolation with only the public sources: private names must not
    # affect numeric inputs, winner identities, or opaque runoff/fallback links.
    spec = importlib.util.spec_from_file_location('supplement', ROOT / 'code/mayoral_elections/00_votemanager_supplement.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    with tempfile.TemporaryDirectory(prefix='gerda-vm-public-') as td:
        module.RAW = Path(td)
        module.DEST = Path(td) / 'votemanager'
        for rel in ['votemanager/source_rounds.csv', 'votemanager/source_candidates_public.csv',
                    'baden_wuerttemberg/bw_parsed.csv']:
            target = Path(td) / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(RAW / rel, target)
        module.build()
        for rel in ['hessen/he_votemanager_parsed.csv', 'baden_wuerttemberg/bw_komm_one_augmented.csv',
                    'baden_wuerttemberg/bw_komm_one_fallback_pair_keys.csv']:
            assert (Path(td) / rel).read_bytes() == (RAW / rel).read_bytes(), ('public-only rebuild differs', rel)
    print('PASS: source accounting, 75 HE / 702 BW accepted tables, 700 complete BW rounds,')
    print('      fallback exclusions, Schorndorf flag, 16 StaLA conflict decisions, Denkingen 887,')
    print('      separate same-year elections, all HE 2026 rounds now have counts, and public source name redaction.')


if __name__ == '__main__':
    main()
