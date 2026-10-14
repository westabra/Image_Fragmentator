"""Package only checksummed source files and create a GitHub draft release."""
import hashlib
import json
import os
from pathlib import Path
import urllib.request
import zipfile

root = Path.cwd()
manifest = json.loads((root / 'PACKAGE_SHA256.json').read_text(encoding='utf-8-sig'))
files = sorted(manifest)
for name in files:
    path = root / name
    if not path.resolve().is_relative_to(root.resolve()):
        raise RuntimeError('Unsafe path: ' + name)
    if hashlib.sha256(path.read_bytes()).hexdigest() != manifest[name]:
        raise RuntimeError('Checksum mismatch: ' + name)
    if name.split('/')[0] in {'runtime', 'R-library', 'weights', 'data', 'logs'}:
        raise RuntimeError('Local installation file in release: ' + name)
archive = root / 'Image_Fragmentator_7.7.0_Lite.zip'
with zipfile.ZipFile(archive, 'w', zipfile.ZIP_DEFLATED, compresslevel=9) as z:
    for name in files + ['PACKAGE_SHA256.json']:
        z.write(root / name, 'Image_Fragmentator_7.7.0_Lite/' + name)
if archive.stat().st_size > 2_000_000:
    raise RuntimeError('Unexpected archive size: compact release must remain under 2 MB')
checksum = root / 'SHA256SUMS.txt'
checksum.write_text(hashlib.sha256(archive.read_bytes()).hexdigest() + '  ' + archive.name + '\n', encoding='ascii')
base = 'https://api.github.com/repos/' + os.environ['GITHUB_REPOSITORY']
def request(url, payload=None, mime='application/json'):
    headers = {'Authorization': 'Bearer ' + os.environ['GH_RELEASE_TOKEN'],
               'Accept': 'application/vnd.github+json', 'Content-Type': mime,
               'X-GitHub-Api-Version': '2022-11-28'}
    data = json.dumps(payload).encode() if isinstance(payload, dict) else payload
    with urllib.request.urlopen(urllib.request.Request(url, data=data, headers=headers)) as response:
        return json.load(response)
for release in request(base + '/releases'):
    if release['tag_name'] == 'v7.7.0':
        raise RuntimeError('Release v7.7.0 already exists; refusing to replace it')
release = request(base + '/releases', {'tag_name': 'v7.7.0', 'target_commitish': os.environ['GITHUB_SHA'],
    'name': 'Image Fragmentator 7.7.0 Lite', 'body': (root / 'RELEASE_NOTES.md').read_text(),
    'draft': True, 'prerelease': False})
upload = release['upload_url'].split('{')[0]
for path, mime in [(archive, 'application/zip'), (checksum, 'text/plain')]:
    request(upload + '?name=' + path.name, path.read_bytes(), mime)
print(release['html_url'])
with open(os.environ['GITHUB_STEP_SUMMARY'], 'a') as out:
    out.write(f"Draft release: {release['html_url']}\n\nZIP size: {archive.stat().st_size:,} bytes\n")
