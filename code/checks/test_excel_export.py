"""Regression checks for the public Excel exporter (standard library only)."""
import csv
import importlib.util
from pathlib import Path
import tempfile
import unittest
import xml.etree.ElementTree as ET
import zipfile

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('export_excel', ROOT/'code/export_excel.py')
exporter = importlib.util.module_from_spec(spec)
spec.loader.exec_module(exporter)
NS = {'s': exporter.NS}


class ExcelExportTests(unittest.TestCase):
    def test_shares_counts_and_identifiers(self):
        kind = exporter.column_kind
        self.assertEqual(kind('cdu', 'number', 'federal_muni_raw'), 'number')
        self.assertEqual(kind('cdu_csu', 'number', 'municipal_harm'), 'share')
        self.assertEqual(kind('seats_cdu_csu', 'number', 'municipal_unharm'), 'number')
        self.assertEqual(kind('votes', 'number', 'ltw_wkr_unharm_long'), 'number')
        self.assertEqual(kind('vote_share', 'number', 'ltw_wkr_unharm_long'), 'share')
        self.assertEqual(kind('flag_pct_only', 'number', 'state_harm'), 'integer')
        self.assertEqual(kind('pop', 'number', 'federal_muni_unharm'), 'number')
        self.assertEqual(kind('pop', 'number', 'county_elec_unharm'), 'share')
        self.assertEqual(kind('ags', 'number', 'ags_crosswalks'), 'text')
        self.assertEqual(kind('agsus', 'number', 'county_elec_unharm'), 'share')
        self.assertAlmostEqual(float(exporter.excel_date('1994-03-20 12:30:00', 'datetime')), 34413 + 12.5/24)

    def test_exact_values_missing_dates_formula_text_and_split(self):
        with tempfile.TemporaryDirectory() as tmp:
            source, target = Path(tmp)/'source.csv', Path(tmp)/'test.xlsx'
            headers = ['ags', 'cdu_csu', 'number_voters', 'election_date', 'label', 'flag_ok']
            rows = [['01001000', '0.283533031842976', '43262', '1994-03-20', '=1+1', 'TRUE'],
                    ['01002000', '', '0', '', 'A, B & <C>\nD', 'FALSE'],
                    ['01003000', 'NA', '1234.567', '2026-09-08', '+0123', 'NA']]
            with source.open('w', newline='') as f:
                writer = csv.writer(f); writer.writerow(headers); writer.writerows(rows)
            schema = {'rows':3, 'columns':dict(zip(headers, ['text','number','number','date','text','boolean']))}
            result = exporter.write_excel(source, target, schema, ROOT/'code/shared/excel_template.xlsx', 'municipal_harm', max_rows=3)
            self.assertEqual(result['sheets'], 2)
            with zipfile.ZipFile(target) as z:
                self.assertIsNone(z.testzip())
                self.assertTrue(all(part.extract_version <= 20 for part in z.infolist()))
                for name in z.namelist():
                    if name.endswith(('.xml','.rels')):
                        ET.fromstring(z.read(name))
                one = ET.fromstring(z.read('xl/worksheets/sheet1.xml'))
                cells = {c.attrib['r']: c for c in one.findall('.//s:c',NS)}
                self.assertEqual(cells['A2'].find('s:is/s:t',NS).text, '01001000')
                self.assertEqual(cells['B2'].find('s:v',NS).text, '0.283533031842976')
                self.assertEqual(cells['D2'].find('s:v',NS).text, '34413')
                self.assertEqual(cells['E2'].find('s:is/s:t',NS).text, '=1+1')
                self.assertEqual(cells['E3'].find('s:is/s:t',NS).text, 'A, B & <C>\nD')
                self.assertNotIn('B3',cells)
                self.assertEqual(cells['C3'].find('s:v',NS).text,'0')
                self.assertEqual(one.find('s:autoFilter',NS).attrib['ref'],'A1:F3')
                self.assertEqual(len(one.findall('.//s:f',NS)),0)
                two = ET.fromstring(z.read('xl/worksheets/sheet2.xml'))
                self.assertEqual(two.find('s:autoFilter',NS).attrib['ref'],'A1:F2')
                self.assertEqual(two.find('.//s:c[@r="A2"]/s:is/s:t',NS).text,'01003000')

    def test_mismatch_does_not_replace_existing_export(self):
        with tempfile.TemporaryDirectory() as tmp:
            source, target = Path(tmp)/'source.csv', Path(tmp)/'test.xlsx'
            source.write_text('ags\n01001000\n')
            target.write_bytes(b'existing')
            with self.assertRaisesRegex(ValueError,'row mismatch'):
                exporter.write_excel(source,target,{'rows':2,'columns':{'ags':'text'}},ROOT/'code/shared/excel_template.xlsx','fixture')
            self.assertEqual(target.read_bytes(),b'existing')
            self.assertFalse(target.with_suffix('.xlsx.tmp').exists())


if __name__ == '__main__':
    unittest.main()
