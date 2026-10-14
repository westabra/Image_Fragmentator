# SPDX-License-Identifier: AGPL-3.0-only
"""Optional Shiny backend using GrainSight's Apache-2.0 segmentation function.
2026-09-29: headless wrapper, empty-output guard, overlap policy and grid export.
Upstream source and notices are retained in upstream/grainsight.
"""
from pathlib import Path
import argparse, importlib.util, json, time
from types import SimpleNamespace
from run_benchmark import ROOT, YOLO, CaptureModel, exclusive_labels, sha256, np, Image, torch
from mask_geometry import resize_nearest

class SafeCapture(CaptureModel):
    def __call__(self, image, **kwargs):
        result = super().__call__(image, **kwargs)
        if result[0].masks is None:
            return [SimpleNamespace(masks=SimpleNamespace(data=torch.zeros((0,image.height,image.width))))]
        return result

def segment(request):
    input_path = Path(request['input'])
    out = Path(request['output'])
    if not input_path.is_file():
        raise ValueError('Input image does not exist')
    size = int(request.get('imgsz',1024)); conf = float(request.get('conf',.5))
    iou = float(request.get('iou',.7)); minimum = int(request.get('min_area',50))
    max_det = int(request.get('max_det',500))
    if size not in (512,1024,2048) or not .01<=conf<=.99 or not .1<=iou<=.95:
        raise ValueError('Invalid resolution, confidence or IoU setting')
    if not 1<=max_det<=1000 or not 1<=minimum<=1000000:
        raise ValueError('Invalid object limit or minimum area')
    if out.exists() and any(out.iterdir()):
        raise ValueError('Output folder must be empty')
    out.mkdir(parents=True,exist_ok=True)
    weights = ROOT/'weights/FastSAM-x.pt'
    if not weights.is_file():
        raise ValueError('FastSAM-x weights are missing; run the documented backend setup')
    img = Image.open(input_path).convert('RGB')
    if min(img.size)<16 or max(img.size)>2000:
        raise ValueError('Input/ROI must be at least 16 pixels on each side and at most 2000 pixels')
    started = time.perf_counter()
    torch.set_num_threads(4); torch.manual_seed(0); np.random.seed(0)
    model = SafeCapture(YOLO(str(weights)))
    spec = importlib.util.spec_from_file_location('grainsight_segment',ROOT/'upstream/grainsight/src/segmentation/segment.py')
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    module.fast_process = lambda **kwargs: kwargs['image']
    native_image, tensor = module.segment_everything(
        img,model,torch.device('cpu'),input_size=size,iou_threshold=iou,
        conf_threshold=conf,better_quality=False,max_det=max_det)
    masks = tensor.cpu().numpy().astype(np.uint8)
    native = exclusive_labels(masks)
    labels = resize_nearest(native,img.size)
    counts = np.bincount(labels.ravel())
    drop = np.flatnonzero(counts<minimum); drop = drop[drop>0]
    labels[np.isin(labels,drop)] = 0
    # Preserve the original proposal IDs after filtering; no post-filter reassignment.
    np.savetxt(out/'labels.csv',labels,fmt='%d',delimiter=',')
    Image.fromarray(labels).save(out/'labels.png')
    np.savez_compressed(out/'raw_masks_packed.npz',packed=np.packbits(masks,axis=2),shape=np.array(masks.shape))
    native_image.save(out/'model_input.png')
    result = model.result[0]
    info = dict(backend='GrainSight / FastSAM-x',imgsz=size,conf=conf,iou=iou,
        min_area=minimum,max_det=max_det,raw_count=len(masks),
        kept_count=int((np.unique(labels)>0).sum()),input_wh=list(img.size),native_wh=list(native_image.size),
        overlap_pixels_native=int((masks.sum(axis=0)>1).sum()),
        device='cpu',elapsed_seconds=time.perf_counter()-started,
        input_sha256=sha256(input_path),weights_sha256=sha256(weights),
        upstream_commit='a3b5b587c24229363fc4c9c81176d7526c4de3fc',
        confidence=result.boxes.conf.cpu().numpy().tolist(),
        label_policy='smaller masks first, stable index ties; center-nearest grid; area filter in working pixels',
        units='working pixels; calibration is applied by Image Fragmentator')
    (out/'metadata.json').write_text(json.dumps(info,indent=2),encoding='utf-8')
    print(json.dumps(info),flush=True)
    return info

if __name__=='__main__':
    p=argparse.ArgumentParser(); p.add_argument('--request',required=True); a=p.parse_args()
    segment(json.loads(Path(a.request).read_text(encoding='utf-8-sig')))
