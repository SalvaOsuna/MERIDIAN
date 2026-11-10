"""Check files actually downloaded by browser_activate.cjs."""
import csv
import json
import zipfile
from pathlib import Path
from PIL import Image
from pypdf import PdfReader

root = Path('validation-output/activate')
with (root / 'adaptation.csv').open(newline='') as handle:
    rows = list(csv.DictReader(handle))
assert len(rows) == 4
assert {x['ENV'] for x in rows} == {'Clavet2024','Clavet2025','Hunter2024','Hunter2025'}
assert all(x['Winning_Genotype'] and '%{' not in x['Winning_Genotype'] for x in rows)
with zipfile.ZipFile(root / 'adaptation.xlsx') as workbook:
    assert 'xl/workbook.xml' in workbook.namelist()
plots = []
for i in range(1, 4):
    with Image.open(root / f'plot-{i}.png') as image:
        size = list(image.size)
        image.verify()
    assert '<svg' in (root / f'plot-{i}.svg').read_text(encoding='utf-8')
    pdf = PdfReader(root / f'plot-{i}.pdf')
    assert len(pdf.pages) == 1
    plots.append({'plot': i, 'png_pixels': size, 'pdf_pages': len(pdf.pages)})
logs = json.loads((root / 'final-browser.json').read_text(encoding='utf-8'))
assert len([x for x in logs if x['stage'].startswith('interaction heatmap')]) == 9
hovers = [x for x in logs if x['stage'].startswith('winner hover')]
assert len(hovers) == 9 and all('%{' not in x['text'] and 'Winner:' in x['text'] for x in hovers)
result = {'environments': len(rows), 'traits_with_verified_hovers': len(hovers), 'plots': plots}
(root / 'artifact-verification.json').write_text(json.dumps(result, indent=2))
print(json.dumps(result, indent=2))
