#!/usr/bin/env python3
"""Verify rebuilt state XLSX exports, including every BB/MV 1990/1994 cell."""
import csv
from datetime import datetime, timezone
from decimal import Decimal
import json
from pathlib import Path
import re
import xml.etree.ElementTree as ET
from xml.parsers import expat
import zipfile

import check_excel_exports as existing

ROOT = existing.ROOT
NAMES = ('state_unharm', 'state_harm_21', 'state_harm_23', 'state_harm_25', 'state_harm')


def selected_rows(key, info, source):
    expected = {}
    with source.open(encoding='utf-8-sig', newline='') as handle:
        reader = csv.reader(handle)
        headers = next(reader)
        state, year = headers.index('state'), headers.index('election_year')
        for number, row in enumerate(reader, 2):
            if row[state] in ('12', '13') and row[year] in ('1990', '1994'):
                expected[number] = row
    assert expected, f'No BB/MV 1990/1994 source rows: {key}'
    count = 0
    checked = set()
    with zipfile.ZipFile(ROOT / (key + '.xlsx')) as workbook:
        style_tree = ET.fromstring(workbook.read('xl/styles.xml'))
        formats = {int(el.attrib['numFmtId']): el.attrib['formatCode']
                   for el in style_tree.findall(f'{{{existing.NS}}}numFmts/{{{existing.NS}}}numFmt')}
        styles = [formats.get(int(el.attrib['numFmtId']), 'General')
                  for el in style_tree.find(f'{{{existing.NS}}}cellXfs')]
        state = {'row': 0, 'cell': None, 'text': False, 'columns': set()}
        parser = expat.ParserCreate(namespace_separator='|')

        def start(tag, attrs):
            tag = tag.rsplit('|', 1)[-1]
            if tag == 'row':
                state['row'] = int(attrs['r'])
                state['columns'] = set()
            elif tag == 'c' and state['row'] in expected:
                state['cell'] = {'r': attrs['r'], 't': attrs.get('t', 'n'),
                                 's': int(attrs.get('s', 0)), 'value': ''}
            elif tag in ('v', 't'):
                state['text'] = True

        def end(tag):
            nonlocal count
            tag = tag.rsplit('|', 1)[-1]
            if tag in ('v', 't'):
                state['text'] = False
            elif tag == 'c' and state['cell'] is not None:
                cell = state['cell']
                column = 0
                for char in re.match('[A-Z]+', cell['r'])[0]:
                    column = column * 26 + ord(char) - 64
                column -= 1
                assert column not in state['columns'], (key, 'duplicate cell', cell['r'])
                state['columns'].add(column)
                original = expected[state['row']][column]
                value, fmt = cell['value'], styles[cell['s']]
                context = (key, cell['r'], headers[column], value, original)
                if cell['t'] in ('inlineStr', 'str'):
                    assert value == original, context
                elif cell['t'] == 'b':
                    assert value == ('1' if original.lower() in ('true', '1') else '0'), context
                elif fmt.startswith('yyyy-mm-dd'):
                    day = datetime.fromisoformat(original.replace(' UTC', '+00:00').replace('Z', '+00:00'))
                    if day.tzinfo:
                        day = day.astimezone(timezone.utc).replace(tzinfo=None)
                    serial = (day - datetime(1899, 12, 30)).total_seconds() / 86400
                    if day < datetime(1900, 3, 1):
                        serial -= 1
                    assert abs(float(value) - serial) < 1e-9, context
                else:
                    assert Decimal(value) == Decimal(original), context
                if headers[column] in info['percentage_columns']:
                    assert fmt == '0.00%', context
                count += 1
                state['cell'] = None
            elif tag == 'row' and state['row'] in expected:
                wanted = {i for i, value in enumerate(expected[state['row']]) if value not in existing.EMPTY}
                assert state['columns'] == wanted, (key, state['row'], 'missing or extra cells')
                checked.add(state['row'])

        def text(value):
            if state['text'] and state['cell'] is not None:
                state['cell']['value'] += value

        parser.StartElementHandler = start
        parser.EndElementHandler = end
        parser.CharacterDataHandler = text
        with workbook.open('xl/worksheets/sheet1.xml') as xml:
            for block in iter(lambda: xml.read(1 << 20), b''):
                parser.Parse(block, False)
            parser.Parse(b'', True)
    assert checked == set(expected), (key, 'missing selected rows')
    return {'selected_rows': len(expected), 'selected_populated_cells': count,
            'selected_blank_cells': len(expected) * len(headers) - count}


def main():
    manifest = json.loads((ROOT / 'docs/excel_exports.json').read_text())
    results = []
    for name in NAMES:
        key = 'data/state_elections/final/' + name
        info = manifest[key]
        source = ROOT / info['source']
        result = existing.verify((key, info, source, None))
        result.update(selected_rows(key, info, source))
        results.append(result)
        print(f'Checked every cell in {result["selected_rows"]:,} BB/MV 1990/1994 rows: {name}', flush=True)
    destination = ROOT / 'data/data_checks/bb_1990_1994/excel_verification.json'
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(json.dumps({'passed': True, 'workbooks': results}, indent=2) + '\n')
    print(f'PASS: {len(results)} state workbooks', flush=True)


if __name__ == '__main__':
    main()
