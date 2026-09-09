#!/usr/bin/env python3
"""Export all tracked, current final tables to Excel; Python standard library + R.

RDS supplies column types; CSV supplies the published values. For RDS-only tables
the R helper writes a temporary CSV. An Artifact Tool workbook supplies styles;
worksheet XML is streamed into ZIP members to avoid materializing huge tables.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import io
import json
import math
import os
from pathlib import Path
import re
import subprocess
import tempfile
import urllib.request
import xml.etree.ElementTree as ET
from xml.sax.saxutils import escape
import zipfile

ROOT = Path(__file__).resolve().parents[1]
NS = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
REL = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
MAX_ROWS = 1_048_576
MAX_COLS = 16_384
MISSING = {'', 'NA', 'NaN'}
IDENTIFIER = re.compile(r'^(ags(?:_\d+)?|county(?:_code(?:_\d+)?)?|state|bwbez|wkr_nr|prior_2021_wkr_nr|person_id)$')
METADATA = set('''eligible_voters eligible_voters_orig number_voters number_voters_orig
valid_votes invalid_votes voters_wo_blockingnotice voters_blockingnotice voters_par25_2
voters_w_ballot blocked_voters_orig unique_mailin unique_multi_mailin joint_mailin
voters_wo_sperrvermerk voters_w_sperrvermerk voters_par24_2 voters_w_wahlschein
pop_weight area_weight voters_weight blocked_weight area_cw pop_cw emp_cw weights
area population area_ags population_ags employees_ags pop_density_ags
area_cty population_cty employees_cty pop_density_cty total_votes total_votes_incogruence
election_year year n_predecessors'''.split())
SHARES = set('''turnout turnout_wo_mailin turnout_sw vote_share total_vote_share
winner_voteshare winning_margin margin_change candidate_voteshare_hw candidate_voteshare_sw
candidate_gender_prob candidate_migration_bg_prob candidate_surname_county_share
perc_total_votes_incogruence perc_total_votes_incongruence'''.split())
WIDE_PREFIXES = ('federal_muni_', 'federal_cty_', 'federal_wkr_', 'state_',
                 'ltw_wkr_', 'municipal_', 'county_elec_', 'european_muni_')


def column_letter(index):
    result = ''
    while index:
        index, rem = divmod(index - 1, 26)
        result = chr(65 + rem) + result
    return result


def column_kind(name, r_kind, stem):
    if IDENTIFIER.fullmatch(name):
        return 'text'
    if r_kind in ('text', 'date', 'datetime'):
        return r_kind
    if name in SHARES:
        return 'share'
    if r_kind == 'boolean':
        return 'boolean'
    if name == 'election_year' or name == 'year' or name.startswith(('flag_', 'replaced_0_with_na_')):
        return 'integer'
    wide = stem.startswith(WIDE_PREFIXES) and not stem.endswith('_long') and stem != 'federal_muni_raw'
    is_meta = name in METADATA or name.startswith(('flag_', 'replaced_0_with_na_', 'seats_'))
    # `pop` is a population covariate in federal municipality files, but also a
    # real party name in other families. Do not classify by name globally.
    is_meta |= name == 'pop' and stem.startswith('federal_muni_')
    if wide and not is_meta:
        return 'share'
    return 'number'


def excel_date(value, kind):
    value = value.replace(' UTC', '+00:00').replace('Z', '+00:00')
    day = dt.datetime.fromisoformat(value)
    if day.tzinfo:
        day = day.astimezone(dt.timezone.utc).replace(tzinfo=None)
    serial = (day - dt.datetime(1899, 12, 30)).total_seconds() / 86400
    if day < dt.datetime(1900, 3, 1):
        serial -= 1  # Excel's fictitious 1900-02-29
    return format(serial, '.17g')


def xml_text(value):
    if any(ord(c) < 32 and c not in '\t\n\r' for c in value):
        raise ValueError('XML control character in source text')
    if len(value) > 32767:
        raise ValueError('Cell exceeds Excel text limit; refusing to truncate')
    return escape(value).replace('\r', '&#13;')


def cell_xml(address, value, kind, styles):
    if value in MISSING:
        return ''
    style = styles[kind]
    if kind == 'text':
        # Explicit inline text also prevents Excel interpreting =/+/-/@ as formulas.
        return f'<c r="{address}" s="{style}" t="inlineStr"><is><t xml:space="preserve">{xml_text(value)}</t></is></c>'
    if kind == 'boolean':
        if value not in ('TRUE', 'FALSE', 'True', 'False', 'true', 'false', '0', '1'):
            raise ValueError(f'Invalid boolean {value!r} at {address}')
        value = '1' if value.lower() in ('true', '1') else '0'
        cell_type = 'b'
    elif kind in ('date', 'datetime'):
        value = excel_date(value, kind)
        cell_type = 'n'
    else:
        number = float(value)
        if not math.isfinite(number):
            # Preserve deliberate infinities as text; OOXML numeric cells cannot hold them.
            return cell_xml(address, value, 'text', styles)
        if not re.fullmatch(r'[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?', value):
            raise ValueError(f'Invalid number {value!r} at {address}')
        if kind == 'number' and number.is_integer():
            style = styles['count']
        cell_type = 'n'
    return f'<c r="{address}" s="{style}" t="{cell_type}"><v>{value}</v></c>'


def template_styles(template):
    sheet = ET.fromstring(template.read('xl/worksheets/sheet1.xml'))
    cells = {c.attrib['r']: c.attrib['s'] for c in sheet.findall(f'.//{{{NS}}}c')}
    styles = dict(zip(('text', 'number', 'share', 'date', 'boolean', 'integer', 'datetime', 'count'),
                      (cells[f'{c}2'] for c in 'ABCDEFGH')))
    styles['header'] = cells['A1']
    return styles


def data_paths(root):
    tracked = subprocess.check_output(['git', 'ls-files', '-z', 'data/**/final/*.csv',
                                       'data/**/final/*.rds'], cwd=root).decode().split('\0')
    # Exact final/ parent excludes archives, processed intermediates, raw inputs,
    # and final_restricted. Only tracked public tables enter the release.
    return sorted({Path(p).with_suffix('') for p in tracked if p and Path(p).parent.name == 'final'})


def source_csv(root, relative, temp):
    path = root / relative.with_suffix('.csv')
    if not path.exists():
        return temp / (relative.name + '.rds.csv')
    with path.open('rb') as f:
        prefix = f.read(200)
    if not prefix.startswith(b'version https://git-lfs.github.com/spec/v1'):
        return path
    digest = re.search(rb'oid sha256:([a-f0-9]{64})', prefix)[1].decode()
    expected_size = int(re.search(rb'size (\d+)', prefix)[1])
    target = temp / (relative.name + '.csv')
    url = 'https://media.githubusercontent.com/media/awiedem/german_election_data/refs/heads/main/' + relative.with_suffix('.csv').as_posix()
    urllib.request.urlretrieve(url, target)
    if target.stat().st_size != expected_size or sha256(target) != digest:
        raise ValueError(f'LFS content does not match local pointer: {relative}')
    return target


def sha256(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def write_excel(source, target, schema, template_path, stem, max_rows=MAX_ROWS):
    """Stream a complete CSV into one or more sheets, with repeated headers."""
    with source.open(encoding='utf-8-sig', newline='') as f:
        headers = next(csv.reader(f))
    if headers != list(schema['columns']):
        raise ValueError(f'CSV/RDS column mismatch: {source}')
    if len(headers) > MAX_COLS or len(set(headers)) != len(headers):
        raise ValueError(f'Too many or duplicate columns: {source}')
    kinds = [column_kind(h, schema['columns'][h], stem) for h in headers]
    letters = [column_letter(i + 1) for i in range(len(headers))]
    target.parent.mkdir(parents=True, exist_ok=True)
    pending = target.with_suffix('.xlsx.tmp')
    counts = []
    try:
        # Classic ZIP avoids Excel's repair warnings for some ZIP64 containers.
        # Oversized future exports fail atomically instead of emitting such files.
        with zipfile.ZipFile(template_path) as template, zipfile.ZipFile(pending, 'w', zipfile.ZIP_DEFLATED, compresslevel=6, allowZip64=False) as z:
            styles = template_styles(template)
            # Keep Artifact Tool's styles/theme; data, dimensions, panes and filters
            # are emitted below. No sample cells, cached strings or tables survive.
            style_tree = ET.fromstring(template.read('xl/styles.xml'))
            # Artifact Tool's boolean preview uses modern in-cell checkboxes.
            # Plain boolean values work in Excel 2019; discard those extensions.
            for xf in style_tree.findall(f'.//{{{NS}}}xf'):
                for extension in xf.findall(f'{{{NS}}}extLst'):
                    xf.remove(extension)
            z.writestr('xl/styles.xml', ET.tostring(style_tree, encoding='utf-8', xml_declaration=True))
            z.writestr('xl/theme/theme1.xml', template.read('xl/theme/theme1.xml'))
            with source.open(encoding='utf-8-sig', newline='') as f:
                reader = csv.reader(f)
                next(reader)
                current = next(reader, None)
                while current is not None or not counts:
                    sheet_index = len(counts) + 1
                    with z.open(f'xl/worksheets/sheet{sheet_index}.xml', 'w') as binary:
                        out = io.TextIOWrapper(binary, encoding='utf-8', newline='')
                        out.write(f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="{NS}">')
                        out.write('<sheetViews><sheetView showGridLines="0" workbookViewId="0"><pane xSplit="1" ySplit="1" topLeftCell="B2" activePane="bottomRight" state="frozen"/></sheetView></sheetViews><sheetFormatPr defaultRowHeight="15"/><cols>')
                        for i, (name, kind) in enumerate(zip(headers, kinds), 1):
                            width = min(42, max(16, len(name) + 2))
                            if kind == 'datetime':
                                width = max(width, 24)
                            if kind == 'text' and ('name' in name or 'party' in name):
                                width = max(width, 32)
                            out.write(f'<col min="{i}" max="{i}" width="{width}" customWidth="1"/>')
                        out.write('</cols><sheetData><row r="1" ht="48" customHeight="1">')
                        for letter, name in zip(letters, headers):
                            out.write(f'<c r="{letter}1" s="{styles["header"]}" t="inlineStr"><is><t>{xml_text(name)}</t></is></c>')
                        out.write('</row>')
                        count = 0
                        while current is not None and count < max_rows - 1:
                            if len(current) != len(headers):
                                raise ValueError(f'Ragged CSV row {sum(counts) + count + 2}: {source}')
                            row = count + 2
                            cells = ''.join(cell_xml(f'{letter}{row}', value, kind, styles)
                                            for letter, value, kind in zip(letters, current, kinds))
                            out.write(f'<row r="{row}">{cells}</row>')
                            count += 1
                            current = next(reader, None)
                        out.write(f'</sheetData><autoFilter ref="A1:{letters[-1]}{count+1}"/></worksheet>')
                        out.flush()
                        out.detach()
                    counts.append(count)
            if sum(counts) != schema['rows']:
                raise ValueError(f'CSV/RDS row mismatch: {source}: CSV={sum(counts)}, RDS={schema["rows"]}')
            sheets = ''.join(f'<sheet name="Data{(" " + str(i)) if len(counts)>1 else ""}" sheetId="{i}" r:id="rId{i}"/>' for i in range(1, len(counts)+1))
            z.writestr('xl/workbook.xml', f'<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="{NS}" xmlns:r="{REL}"><bookViews><workbookView/></bookViews><sheets>{sheets}</sheets></workbook>')
            rels = ''.join(f'<Relationship Id="rId{i}" Type="{REL}/worksheet" Target="worksheets/sheet{i}.xml"/>' for i in range(1,len(counts)+1))
            rels += f'<Relationship Id="styles" Type="{REL}/styles" Target="styles.xml"/><Relationship Id="theme" Type="{REL}/theme" Target="theme/theme1.xml"/>'
            z.writestr('xl/_rels/workbook.xml.rels', f'<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">{rels}</Relationships>')
            z.writestr('_rels/.rels', f'<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="workbook" Type="{REL}/officeDocument" Target="xl/workbook.xml"/></Relationships>')
            overrides = ''.join(f'<Override PartName="/xl/worksheets/sheet{i}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' for i in range(1,len(counts)+1))
            z.writestr('[Content_Types].xml', '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/><Override PartName="/xl/theme/theme1.xml" ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>' + overrides + '</Types>')
        os.replace(pending, target)
    finally:
        pending.unlink(missing_ok=True)
    return {'rows': sum(counts), 'columns': len(headers), 'sheets': len(counts),
            'percentage_columns': [h for h,k in zip(headers,kinds) if k == 'share']}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--only', nargs='+', help='Dataset stems to rebuild (default: all current final tables)')
    parser.add_argument('--force', action='store_true', help='Rebuild even if source and exporter hashes are unchanged')
    args = parser.parse_args()
    root = args.root.resolve()
    paths = data_paths(root)
    if args.only:
        unknown = set(args.only) - {p.name for p in paths}
        if unknown:
            parser.error(f'Unknown public datasets: {sorted(unknown)}')
        paths = [p for p in paths if p.name in args.only]
    template = root / 'code/shared/excel_template.xlsx'
    helper = root / 'code/shared/excel_schema.R'
    exporter_hash = hashlib.sha256((sha256(Path(__file__)) + sha256(template) + sha256(helper)).encode()).hexdigest()
    manifest_path = root / 'docs/excel_exports.json'
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    with tempfile.TemporaryDirectory(prefix='gerda-excel-') as tmp:
        temp = Path(tmp)
        rds_paths = [p.with_suffix('.rds').as_posix() for p in paths]
        (temp/'paths.txt').write_text('\n'.join(rds_paths)+'\n')
        subprocess.run(['Rscript', '--vanilla', str(helper), str(temp/'paths.txt'), str(temp/'schema.json'), str(temp)], cwd=root, check=True)
        schemas = json.loads((temp/'schema.json').read_text())
        for relative in paths:
            source = source_csv(root, relative, temp)
            target = root / relative.with_suffix('.xlsx')
            source_hash = sha256(source)
            rds_hash = sha256(root / relative.with_suffix('.rds'))
            key = relative.as_posix()
            previous = manifest.get(key, {})
            if not args.force and target.exists() and previous.get('source_sha256') == source_hash and previous.get('rds_sha256') == rds_hash and previous.get('exporter_sha256') == exporter_hash and previous.get('xlsx_sha256') == sha256(target):
                print(f'Unchanged {relative.name}', flush=True)
                continue
            print(f'Exporting {relative.name}', flush=True)
            result = write_excel(source, target, schemas[relative.with_suffix('.rds').as_posix()], template, relative.name)
            result.update(source_sha256=source_hash, rds_sha256=rds_hash, exporter_sha256=exporter_hash,
                          xlsx_sha256=sha256(target), bytes=target.stat().st_size,
                          source=relative.with_suffix('.csv' if (root/relative.with_suffix('.csv')).exists() else '.rds').as_posix())
            manifest[key] = result
            pending = manifest_path.with_suffix('.json.tmp')
            pending.write_text(json.dumps(manifest, indent=2, ensure_ascii=False)+'\n')
            os.replace(pending, manifest_path)
            print(f'  {result["rows"]:,} rows, {result["columns"]} columns; {result["bytes"]/1e6:.1f} MB', flush=True)


if __name__ == '__main__':
    main()
