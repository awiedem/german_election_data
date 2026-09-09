#!/usr/bin/env python3
"""Verify every public XLSX: hashes, XML, row/cell counts and sampled source values.

Uses bounded memory even for wide state-election tables. No Excel-writing package
is needed. Optional --previews writes small workbook excerpts for visual QA.
"""
import argparse
from concurrent.futures import ProcessPoolExecutor
import csv
from datetime import datetime, timezone
from decimal import Decimal
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile
import urllib.request
import xml.etree.ElementTree as ET
from xml.parsers import expat
import zipfile

ROOT = Path(__file__).resolve().parents[2]
NS = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
EMPTY = {'', 'NA', 'NaN'}


def sha256(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda:f.read(1<<20),b''):
            h.update(block)
    return h.hexdigest()


def resolve_csv(source, temporary, lfs_objects):
    with source.open('rb') as f:
        prefix=f.read(200)
    if not prefix.startswith(b'version https://git-lfs.github.com/spec/v1'):
        return source
    digest=re.search(rb'oid sha256:([a-f0-9]{64})',prefix)[1].decode()
    size=int(re.search(rb'size (\d+)',prefix)[1])
    cached=lfs_objects/digest[:2]/digest[2:4]/digest
    if not cached.is_file():
        cached=temporary/source.name
        url='https://media.githubusercontent.com/media/awiedem/german_election_data/refs/heads/main/'+source.relative_to(ROOT).as_posix()
        urllib.request.urlretrieve(url,cached)
    assert cached.stat().st_size==size and sha256(cached)==digest, f'LFS object mismatch: {source}'
    return cached


def verify(job):
    key, info, source, preview_dir = job
    xlsx = ROOT / (key+'.xlsx')
    assert sha256(xlsx) == info['xlsx_sha256'], f'Changed Excel file: {key}'
    assert sha256(source) == info['source_sha256'], f'Stale Excel export: {key}'
    assert sha256(ROOT/(key+'.rds')) == info['rds_sha256'], f'Changed RDS schema: {key}'
    sampled_rows = {1,2,3,4, max(2, info['rows']//2+1),info['rows']+1}
    with source.open(encoding='utf-8-sig',newline='') as f:
        reader = csv.reader(f)
        expected = {}
        csv_cells = 0
        row_count = -1
        for row_number, row in enumerate(reader,1):
            row_count += 1
            if row_number == 1:
                headers = row
            else:
                csv_cells += sum(value not in EMPTY for value in row)
            if row_number in sampled_rows:
                expected[row_number] = row
    assert row_count == info['rows']
    with zipfile.ZipFile(xlsx) as z:
        assert info['sheets'] == 1, 'Extend source-row offsets when the first multi-sheet dataset is released'
        style_tree = ET.fromstring(z.read('xl/styles.xml'))
        formats = {int(el.attrib['numFmtId']):el.attrib['formatCode'] for el in style_tree.findall(f'{{{NS}}}numFmts/{{{NS}}}numFmt')}
        styles = [formats.get(int(el.attrib['numFmtId']),'General') for el in style_tree.find(f'{{{NS}}}cellXfs')]
        stats = {'rows':0,'cells':0,'formulas':0}
        samples = {}
        state = {'row':0,'cell':None,'text':False}
        parser = expat.ParserCreate(namespace_separator='|')

        def start(tag, attrs):
            tag = tag.rsplit('|',1)[-1]
            if tag == 'row':
                stats['rows'] += 1
                state['row'] = int(attrs['r'])
                if state['row'] in sampled_rows:
                    samples[state['row']] = []
            elif tag == 'c':
                if state['row'] > 1:
                    stats['cells'] += 1
                if state['row'] in sampled_rows:
                    state['cell'] = {'r':attrs['r'],'t':attrs.get('t','n'),'s':int(attrs.get('s',0)),'value':''}
            elif tag in ('v','t'):
                state['text'] = True
            elif tag == 'f':
                stats['formulas'] += 1

        def end(tag):
            tag = tag.rsplit('|',1)[-1]
            if tag in ('v','t'):
                state['text'] = False
            elif tag == 'c' and state['cell'] is not None:
                samples[state['row']].append(state['cell'])
                state['cell'] = None

        def text(value):
            if state['text'] and state['cell'] is not None:
                state['cell']['value'] += value

        parser.StartElementHandler,parser.EndElementHandler,parser.CharacterDataHandler = start,end,text
        with z.open('xl/worksheets/sheet1.xml') as xml:
            for block in iter(lambda:xml.read(1<<20),b''):
                parser.Parse(block,False)
            parser.Parse(b'',True)
        assert stats['rows'] == info['rows']+1, (key,stats)
        assert stats['cells'] == csv_cells, (key,stats,csv_cells)
        assert stats['formulas'] == 0, key
        for row_number, cells in samples.items():
            source_row = expected[row_number]
            actual_columns = set()
            for cell in cells:
                letter = re.match('[A-Z]+',cell['r'])[0]
                column = 0
                for char in letter:
                    column = column*26+ord(char)-64
                column -= 1
                actual_columns.add(column)
                value, original = cell['value'],source_row[column]
                fmt = styles[cell['s']]
                if cell['t'] in ('inlineStr','str'):
                    assert value == original, (key,cell,original)
                elif cell['t'] == 'b':
                    assert value == ('1' if original.lower() in ('true','1') else '0')
                elif fmt.startswith('yyyy-mm-dd'):
                    day = datetime.fromisoformat(original.replace(' UTC','+00:00').replace('Z','+00:00'))
                    if day.tzinfo:
                        day = day.astimezone(timezone.utc).replace(tzinfo=None)
                    serial = (day-datetime(1899,12,30)).total_seconds()/86400
                    if day < datetime(1900,3,1): serial -= 1
                    assert abs(float(value)-serial)<1e-9, (key,cell,original)
                else:
                    assert Decimal(value) == Decimal(original), (key,cell,original)
                if row_number>1 and headers[column] in info['percentage_columns']:
                    assert fmt == '0.00%', (key,cell)
            assert actual_columns == {i for i,v in enumerate(source_row) if row_number==1 or v not in EMPTY}, key
        if preview_dir:
            # A small excerpt of the actual saved workbook, retaining its styles.
            sheet = ET.Element(f'{{{NS}}}worksheet')
            cols = ET.SubElement(sheet,f'{{{NS}}}cols')
            data = ET.SubElement(sheet,f'{{{NS}}}sheetData')
            choices = list(range(min(5,len(headers))))
            shares = [i for i,h in enumerate(headers) if h in info['percentage_columns']]
            counts = [i for i,h in enumerate(headers) if h in ('number_voters','valid_votes','votes','seats_total','winner_votes')]
            choices = list(dict.fromkeys(choices + counts[:2] + shares[:2]))
            for new, old in enumerate(choices,1):
                ET.SubElement(cols,f'{{{NS}}}col',{'min':str(new),'max':str(new),'width':'28','customWidth':'1'})
            for row_number in (1,2,3,4):
                if row_number not in samples: continue
                row = ET.SubElement(data,f'{{{NS}}}row',{'r':str(row_number),'ht':'40' if row_number==1 else '22','customHeight':'1'})
                lookup = {}
                for cell in samples[row_number]:
                    label = re.match('[A-Z]+',cell['r'])[0]
                    index=0
                    for char in label:index=index*26+ord(char)-64
                    lookup[index-1] = cell
                for new,old in enumerate(choices):
                    if old not in lookup: continue
                    cell=lookup[old]
                    el=ET.SubElement(row,f'{{{NS}}}c',{'r':chr(65+new)+str(row_number),'s':str(cell['s']),'t':cell['t']})
                    if cell['t']=='inlineStr':
                        parent=ET.SubElement(el,f'{{{NS}}}is'); tag='t'
                    else:parent=el; tag='v'
                    ET.SubElement(parent,f'{{{NS}}}{tag}').text=cell['value']
            destination=Path(preview_dir)/(Path(key).name+'.xlsx')
            with zipfile.ZipFile(destination,'w',zipfile.ZIP_DEFLATED) as small:
                for name in z.namelist():
                    small.writestr(name,ET.tostring(sheet,encoding='utf-8',xml_declaration=True) if name=='xl/worksheets/sheet1.xml' else z.read(name))
    print(f'Checked {Path(key).name}: {row_count:,} rows; {csv_cells:,} populated data cells',flush=True)
    return {'dataset':key,'rows':row_count,'populated_cells':csv_cells,'sampled_rows':sorted(expected)}


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--previews',type=Path)
    parser.add_argument('--report',type=Path,default=Path('/tmp/gerda-excel-validation.json'))
    args=parser.parse_args()
    if args.previews:args.previews.mkdir(parents=True,exist_ok=True)
    manifest=json.loads((ROOT/'docs/excel_exports.json').read_text())
    with tempfile.TemporaryDirectory(prefix='gerda-excel-check-') as tmp:
        temp=Path(tmp)
        rds_only=[v['source'] for v in manifest.values() if v['source'].endswith('.rds')]
        if rds_only:
            (temp/'paths.txt').write_text('\n'.join(rds_only)+'\n')
            subprocess.run(['Rscript','--vanilla',str(ROOT/'code/shared/excel_schema.R'),str(temp/'paths.txt'),str(temp/'schema.json'),str(temp)],cwd=ROOT,check=True)
        lfs_path=Path(subprocess.check_output(['git','rev-parse','--git-path','lfs/objects'],cwd=ROOT,text=True).strip())
        if not lfs_path.is_absolute():lfs_path=ROOT/lfs_path
        jobs=[(key,info,resolve_csv(ROOT/info['source'],temp,lfs_path) if info['source'].endswith('.csv') else temp/(Path(info['source']).name+'.csv'),str(args.previews) if args.previews else None) for key,info in manifest.items()]
        with ProcessPoolExecutor(max_workers=2) as pool:
            results=list(pool.map(verify,jobs))
        args.report.write_text(json.dumps(results,indent=2)+'\n')
        print(f'PASS: {len(results)} workbooks; {sum(v["rows"] for v in results):,} rows',flush=True)


if __name__=='__main__':
    main()
