"""Check isolation and execute a real small inference with the bundled model."""
from pathlib import Path
import json
import sys
import hashlib

ROOT = Path(__file__).resolve().parents[1]
runtime = ROOT / 'runtime' / 'python'
assert Path(sys.executable).resolve().is_relative_to(runtime), sys.executable
assert Path(sys.prefix).resolve() == runtime.resolve(), sys.prefix
assert Path(sys.base_prefix).resolve() == runtime.resolve(), sys.base_prefix
for entry in sys.path:
    if entry:
        assert Path(entry).resolve().is_relative_to(ROOT), entry
from run_benchmark import YOLO, torch, np, Image
import cv2
import scipy
import pandas
import torchvision

weights = ROOT / 'weights' / 'FastSAM-x.pt'
with weights.open('rb') as f:
    digest = hashlib.file_digest(f, 'sha256').hexdigest()
assert digest == '752cadc2828edb1cd4bc4f9eb587100631af06ea2108f4c9ed56df4755701e76', 'Model checksum mismatch'
torch.set_num_threads(4)
model = YOLO(str(weights))
result = model(Image.fromarray(np.full((64, 96, 3), 128, dtype=np.uint8)),
               device='cpu', imgsz=512, conf=.5, verbose=False)
assert len(result) == 1
report = dict(status='PASS', python=sys.version, executable=str(Path(sys.executable).relative_to(ROOT)),
              isolated=bool(sys.flags.isolated), sys_path=[str(Path(p).resolve().relative_to(ROOT)) for p in sys.path],
              torch=torch.__version__, torchvision=torchvision.__version__, weights_sha256=digest,
              real_model_inference=True)
(ROOT/'logs').mkdir(exist_ok=True)
(ROOT/'logs'/'python-check.json').write_text(json.dumps(report, indent=2), encoding='utf-8')
print('PASS: local Python, local libraries and real CPU model inference.')
