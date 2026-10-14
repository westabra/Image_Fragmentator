# SPDX-License-Identifier: AGPL-3.0-only
"""Reproducible local FastSAM pilot. The verified R application is never modified."""
from pathlib import Path
import argparse, hashlib, importlib.util, importlib.metadata, json, os, time

ROOT = Path(__file__).resolve().parent
os.environ['MPLBACKEND'] = 'Agg'
os.environ['MPLCONFIGDIR'] = str(ROOT / '.matplotlib')
os.environ['YOLO_CONFIG_DIR'] = str(ROOT / '.ultralytics')
os.environ['YOLO_AUTOINSTALL'] = 'false'
(ROOT / '.ultralytics').mkdir(exist_ok=True)
import numpy as np
from PIL import Image
from mask_geometry import resize_nearest
import torch
from ultralytics import YOLO
from ultralytics.yolo.utils import set_settings
set_settings(dict(sync=False, datasets_dir=str(ROOT/'datasets'),
                  weights_dir=str(ROOT/'weights'), runs_dir=str(ROOT/'runs')))

def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()

def exclusive_labels(masks):
    """Smaller instances own overlaps, matching upstream display priority."""
    labels = np.zeros(masks.shape[1:], dtype=np.uint16)
    order = np.argsort(masks.sum(axis=(1, 2)), kind='stable')
    for index in order:
        labels[(masks[index] != 0) & (labels == 0)] = int(index) + 1
    return labels

class CaptureModel:
    def __init__(self, model):
        self.model = model
        self.result = None
    def __call__(self, image, **kwargs):
        self.result = self.model(image, device='cpu', verbose=True, **kwargs)
        return self.result

def run(case, image_path, out):
    if (out/'COMPLETE.json').exists():
        print('Already complete:', case['name'], flush=True)
        return
    out.mkdir(parents=True, exist_ok=True)
    image = Image.open(image_path).convert('RGB')
    np.random.seed(0)
    torch.manual_seed(0)
    torch.set_num_threads(4)
    started = time.perf_counter()
    model = CaptureModel(YOLO(str(ROOT/'weights/FastSAM-x.pt')))
    loaded = time.perf_counter()
    if case['pipeline'] == 'grainsight':
        source = ROOT/'upstream/grainsight/src/segmentation/segment.py'
        spec = importlib.util.spec_from_file_location('archived_grainsight_segment', source)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        # Skip ONLY rendering, not inference. The original tensor is returned unchanged.
        module.fast_process = lambda **kwargs: kwargs['image']
        native_image, annotations = module.segment_everything(
            image, model, torch.device('cpu'), input_size=case['imgsz'],
            iou_threshold=case['iou'], conf_threshold=case['conf'],
            better_quality=True, contour_thickness=1, max_det=case['max_det'])
    else:
        results = model(image, retina_masks=True, imgsz=case['imgsz'],
                        conf=case['conf'], iou=case['iou'], max_det=case['max_det'])
        annotations = results[0].masks.data if results[0].masks is not None else None
        native_image = image
    inference_done = time.perf_counter()
    if annotations is None:
        masks = np.zeros((0, native_image.height, native_image.width), dtype=np.uint8)
    else:
        masks = annotations.detach().cpu().numpy().astype(np.uint8)
    assert masks.shape[1:] == (native_image.height, native_image.width)
    assert np.all((masks == 0) | (masks == 1))
    native_image.save(out/'model_input.png')
    np.savez_compressed(out/'raw_masks_packed.npz',
                        packed=np.packbits(masks, axis=2), shape=np.array(masks.shape))
    result = model.result[0]
    np.savez_compressed(out/'detections.npz',
                        boxes_xyxy=result.boxes.xyxy.cpu().numpy(),
                        confidence=result.boxes.conf.cpu().numpy())
    native_labels = exclusive_labels(masks)
    Image.fromarray(native_labels).save(out/'labels_native.png')
    working_labels = resize_nearest(native_labels, image.size)
    Image.fromarray(working_labels).save(out/'labels_working.png')
    np.save(out/'labels_working.npy', working_labels)
    np.savetxt(out/'labels_working.csv', working_labels, fmt='%d', delimiter=',')
    info = dict(case=case, image_sha256=sha256(image_path), weights_sha256=sha256(ROOT/'weights/FastSAM-x.pt'),
                protocol_sha256=sha256(ROOT/'protocol.json'),
                model_input_wh=list(native_image.size), comparison_wh=list(image.size),
                native_to_working_xy=[image.width/native_image.width, image.height/native_image.height],
                mask_count=len(masks), overlap_pixels_native=int((masks.sum(axis=0)>1).sum()),
                load_seconds=loaded-started, inference_and_wrapper_seconds=inference_done-loaded,
                total_seconds=time.perf_counter()-started, device='cpu', torch_threads=4,
                versions={p:importlib.metadata.version(p) for p in ['torch','torchvision','ultralytics','numpy','Pillow','opencv-python']},
                output_note='Independent native masks preserved; exclusive labels are an explicit adapter, not upstream GrainSight output.')
    (out/'COMPLETE.json').write_text(json.dumps(info, indent=2), encoding='utf-8')
    print(json.dumps(info), flush=True)

if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('--case', default='all')
    args = p.parse_args()
    protocol = json.loads((ROOT/'protocol.json').read_text())
    for case in protocol['cases']:
        if args.case in ('all', case['name']):
            run(case, (ROOT/protocol['source_image']).resolve(), ROOT/'results'/case['name'])
