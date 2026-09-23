#!/usr/bin/env python3
"""Check the state export, including every cell in the added BW 1952 rows."""
import csv
from datetime import datetime
from decimal import Decimal
import json
from pathlib import Path
import re
import xml.etree.ElementTree as ET
from xml.parsers import expat
import zipfile

from check_excel_exports import verify, NS, EMPTY

ROOT = Path(__file__).resolve().parents[2]
KEY = 'data/state_elections/final/state_unharm'


def main():
    manifest = json.loads((ROOT / 'docs/excel_exports.json').read_text())
    info = manifest[KEY]
    source = ROOT / (KEY + '.csv')
    general = verify((KEY, info, source, None))
    expected = {}
    with source.open(newline='', encoding='utf-8-sig') as f:
        reader = csv.reader(f)
        headers = next(reader)
        state_col, year_col = headers.index('state'), headers.index('election_year')
        for row_number, row in enumerate(reader, 2):
            if row[state_col] == '08' and row[year_col] == '1952':
                expected[row_number] = row
    assert len(expected) == 1111
    checked = set()
    context = {'row': 0, 'cell': None, 'text': False, 'columns': set()}
    with zipfile.ZipFile(ROOT / (KEY + '.xlsx')) as z:
        tree = ET.fromstring(z.read('xl/styles.xml'))
        formats = {int(e.attrib['numFmtId']): e.attrib['formatCode']
                   for e in tree.findall(f'{{{NS}}}numFmts/{{{NS}}}numFmt')}
        styles = [formats.get(int(e.attrib['numFmtId']), 'General')
                  for e in tree.find(f'{{{NS}}}cellXfs')]
        parser = expat.ParserCreate(namespace_separator='|')

        def start(tag, attrs):
            tag = tag.rsplit('|', 1)[-1]
            if tag == 'row':
                context['row'] = int(attrs['r'])
                context['columns'] = set()
            elif context['row'] in expected:
                if tag == 'c':
                    context['cell'] = dict(attrs, value='')
                elif tag in ('v', 't'):
                    context['text'] = True

        def end(tag):
            tag = tag.rsplit('|', 1)[-1]
            if tag in ('v', 't'):
                context['text'] = False
            if context['row'] not in expected:
                return
            if tag == 'c':
                cell = context['cell']
                label = re.match('[A-Z]+', cell['r'])[0]
                column = 0
                for char in label:
                    column = column * 26 + ord(char) - 64
                column -= 1
                context['columns'].add(column)
                original = expected[context['row']][column]
                value = cell['value']
                assert original not in EMPTY, (cell['r'], 'unexpected nonmissing cell')
                if cell.get('t') in ('inlineStr', 'str'):
                    assert value == original, (cell['r'], value, original)
                elif styles[int(cell.get('s', '0'))].startswith('yyyy-mm-dd'):
                    serial = (datetime.fromisoformat(original) - datetime(1899, 12, 30)).days
                    assert float(value) == serial
                else:
                    assert Decimal(value) == Decimal(original), (cell['r'], value, original)
                context['cell'] = None
            elif tag == 'row':
                wanted = {i for i, value in enumerate(expected[context['row']]) if value not in EMPTY}
                assert context['columns'] == wanted, context['row']
                checked.add(context['row'])

        def text(value):
            if context['text'] and context['cell'] is not None:
                context['cell']['value'] += value

        parser.StartElementHandler = start
        parser.EndElementHandler = end
        parser.CharacterDataHandler = text
        with z.open('xl/worksheets/sheet1.xml') as xml:
            for block in iter(lambda: xml.read(1 << 20), b''):
                parser.Parse(block, False)
            parser.Parse(b'', True)
    assert checked == set(expected)
    report = {'general_export_validation': general, 'bw_1952_rows_checked': len(checked),
              'bw_1952_columns_checked': len(headers), 'passed': True}
    target = ROOT / 'data/data_checks/bw_1952/excel_validation.json'
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(report, indent=2) + '\n')
    print('BW 1952 Excel: all 1,111 added rows match CSV, including missing cells.')


if __name__ == '__main__':
    main()
