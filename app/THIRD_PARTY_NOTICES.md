# Image Fragmentator V7.7.0 — third-party notices

Integration prepared 2026-09-29/30. Compact release packaging prepared 2026-10-14. Runtime libraries and model weights are downloaded separately during optional AI setup. The release includes the application, adapter source, runtime patch and upstream attribution files.

## Image Fragmentator / Make Thin Section

Original project: Sergei Abramov, https://github.com/westabra/My_Shiny_App

The public project declares GPL-3.0-or-later. The existing R code retains that license; its GPL text is in LICENSE-GPL-3.0.txt. The new version is based on the user's V7.6.4 source, SHA256 `26d0161ce09cef108c9f6af33d577e97cba4d0da3d0c4f8876288785242e18cc`.

Changes: an optional method selector and controls, a headless Python bridge, separate result state and provenance, and existing calibration/morphometry applied to FastSAM object masks. Existing segmentation and measurement algorithms are preserved. New R integration code follows GPL-3.0-or-later; Python integration code is marked AGPL-3.0-only.

## GrainSight

Authors: F. Azzam and collaborators. Repository: https://github.com/fazzam12345/grainsight

Archived commit: `a3b5b587c24229363fc4c9c81176d7526c4de3fc`. License: Apache-2.0, preserved in `../upstream/grainsight/LICENSE` and LICENSE-GrainSight-Apache-2.0.txt. Source files remain unchanged under `../upstream/grainsight`.

Publication: Azzam, Blaise and Brigaud (2024), Automated petrographic image analysis by supervised and unsupervised machine learning methods, https://doi.org/10.57035/journals/sdk.2024.e22.1594

The adapter calls GrainSight's original segment_everything function. It bypasses only visualization and provides an empty-result guard, native-mask retention, exclusive overlap resolution, coordinate mapping and working-pixel area filtering. It does not copy GrainSight's Streamlit UI or morphometry calculations. This is an adaptation, not the official GrainSight desktop product.

## FastSAM and Ultralytics

FastSAM: https://github.com/CASIA-LMC-Lab/FastSAM

Ultralytics: https://github.com/ultralytics/ultralytics

Runtime: ultralytics 8.0.120, torch 2.2.2+cpu, torchvision 0.17.2+cpu. Weights: FastSAM-x.pt, official Ultralytics assets v8.3.0; source URL and SHA256 are in `../weights_manifest.json`.

The stack is handled under AGPL-3.0. Copies are preserved in LICENSE-FastSAM-AGPL-3.0.txt and LICENSE-Ultralytics-AGPL-3.0.txt. FastSAM's README labels the model Apache-2.0 while its linked LICENSE is AGPL-3.0; this distribution does not claim the whole stack is Apache-only.

The installed Ultralytics package has a documented one-line local configuration-path compatibility patch (`../patch_runtime.py`, `../runtime_patch.json`). It does not alter inference, weights, or mask generation. Complete installed package versions are in `../requirements-lock.txt`; the original installed package source and its notices are in the local environment. Before redistributing a complete binary/environment bundle, provide corresponding sources as required by its licenses.

## Combined distribution

GPLv3 section 13 expressly permits combination with AGPLv3 code. Preserve each component's notices; AGPL network-source provisions apply to a combined work as specified by those licenses. Making the modified combined program available to remote users requires offering the corresponding source. This repository arrangement does not automatically change the license of an independent Make Thin Section application merely because it launches a separate tool.

Python/backend installation is optional. Existing segmentation methods work without running Python. Other R/Python dependencies retain their own licenses. This source integration is not a license audit of every possible future bundled environment or deployment.
