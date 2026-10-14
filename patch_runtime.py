# SPDX-License-Identifier: AGPL-3.0-only
"""Make 8.0.120 honor YOLO_CONFIG_DIR without touching the Windows user profile.
Modified 2026-09-29: lazy fallback evaluation only; no model/mask changes.
"""
from pathlib import Path
import hashlib, importlib.metadata, json
root = Path(__file__).resolve().parent
dist = importlib.metadata.distribution('ultralytics')
assert dist.version == '8.0.120'
path = Path(dist.locate_file('ultralytics/yolo/utils/__init__.py'))
old = "Path(os.getenv('YOLO_CONFIG_DIR', get_user_config_dir()))"
new = "Path(os.getenv('YOLO_CONFIG_DIR') or get_user_config_dir())"
content = path.read_text(encoding='utf-8')
before = hashlib.sha256(path.read_bytes()).hexdigest()
if old in content:
    content = content.replace(old,new)
    path.write_text(content,encoding='utf-8')
    (root/'runtime_patch.json').write_text(json.dumps(dict(
        package='ultralytics 8.0.120',file='ultralytics/yolo/utils/__init__.py',
        date='2026-09-29',old=old,new=new,sha256_before=before,
        sha256_after=hashlib.sha256(path.read_bytes()).hexdigest(),
        reason='Eager default argument creates AppData directory even when YOLO_CONFIG_DIR is set; lazy fallback only.'),indent=2),encoding='utf-8')
else:
    assert new in content
print('Runtime config patch ready')
