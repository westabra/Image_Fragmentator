# Image Fragmentator 7.7.0 — compact Windows distribution

- Standalone R Shiny application with optional GrainSight/FastSAM CPU segmentation.
- Small ZIP: source code, license notices and installation scripts only.
- Separate base and AI setup; no bundled R library, Python runtime or model.
- Python and model downloads are checked against SHA-256 digests. CPU inference is verified after AI installation.
- Compatible with Make Thin Section V70 Lite external launching.
- English setup guide and launch files; RStudio is not required.

Requirements: Windows 10/11 x64 Intel/AMD, compatible 64-bit R (tested 4.5.3), and Internet access for setup. The full installed environment is much larger than the release ZIP. Read START_HERE.md before installation.

Download `Image_Fragmentator_7.7.0_Lite.zip` and extract it. Run `1_Install_dependencies.cmd`, then `2_Start.cmd`. Install the optional AI backend with `3_Install_AI.cmd`.
