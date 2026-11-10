"""Inspect actual files downloaded by browser_validation.cjs."""
from pathlib import Path
import csv, json, zipfile
from pypdf import PdfReader
import pypdfium2 as pdfium
from PIL import Image

root = Path('validation-output/browser')
report = PdfReader(root / 'example-report.pdf')
text = '\n'.join(p.extract_text() for p in report.pages)
required = ['ANOVA table', 'BLUPS table', 'Stability processing', 'SpATS adjusted means table',
            'Environmental PCA', 'Environmental correlations', 'Composed figure', 'Methods and provenance']
assert all(label in text for label in required), 'Missing selected report content'
assert 'e-' in text, 'Report must preserve small p values'
with zipfile.ZipFile(root / 'panels.zip') as z:
    panels = z.namelist()
    assert len(panels) >= 4 and all('/' not in name and '\\' not in name for name in panels)
with zipfile.ZipFile(root / 'selected-table.xlsx') as z:
    assert 'xl/workbook.xml' in z.namelist()
with (root / 'spatial-across.csv').open(newline='') as f:
    rows = list(csv.DictReader(f))
    assert len(rows) == 20 and all(int(row['N_Envs']) == 5 for row in rows)
    assert 'SE_Descriptive_Mean' in rows[0]
images = {}
for name in ['selected-plot.png', 'selected-plot.tiff']:
    with Image.open(root / name) as im:
        im.verify()
    with Image.open(root / name) as im:
        images[name] = list(im.size)
assert (root / 'selected-plot.pdf').read_bytes().startswith(b'%PDF')
assert '<svg' in (root / 'selected-plot.svg').read_text()
assert '<svg' in (root / 'composed.svg').read_text()
pdf = pdfium.PdfDocument(root / 'example-report.pdf')
for page in [0, 2, len(pdf)-1]:
    pdf[page].render(scale=1.3).to_pil().save(root / f'report-page-{page+1}.png')
result = {'pdf_pages': len(report.pages), 'selected_content_verified': required, 'zip_panels': panels,
          'images': images, 'spatial_genotypes': len(rows), 'environments_per_genotype': 5}
(root / 'artifact-verification.json').write_text(json.dumps(result, indent=2))
print(json.dumps(result, indent=2))
