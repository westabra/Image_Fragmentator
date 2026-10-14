# Image Fragmentator 7.7.0 Lite

**An interactive tool for exploring rock microtextures and obtaining quantitative measurements from thin-section photographs.**

Image Fragmentator is designed for petrographers, geologists and researchers working with microscope images. It brings image segmentation, spatial calibration, object measurements and export into one local application, helping users move from visual observations to documented, measurable results.

## What it helps you do

- Select a representative area of a photograph or process the complete image.
- Separate visually distinct regions using grayscale thresholds, RGB colour clustering, experimental watershed methods or optional GrainSight/FastSAM segmentation.
- Explore subdivisions within larger domains and aggregates using hierarchical watershed analysis.
- Calibrate image dimensions in millimetres or micrometres and reuse saved calibration profiles.
- Examine two-dimensional apparent grain/object size distributions, review individual measurements and exclude border-touching objects from size statistics.
- Export segmented images with legends, statistics, object measurements and text reports for further analysis and petrographic documentation.

The original image, segmentation and overlay can be compared visually. Segmentation detects image regions and object candidates; mineral identification and geological interpretation remain the user's responsibility. Reported sizes describe apparent two-dimensional sections, not a reconstructed three-dimensional grain-size distribution.

## Standalone application or companion to Make Thin Section

The program runs locally through an R Shiny interface in a web browser; RStudio is optional. It can be used independently or launched from Make Thin Section V70 Lite. Optional AI segmentation runs on a local CPU after installation.

## Compact distribution

Download **Image_Fragmentator_7.7.0_Lite.zip — approximately 165 KB**. The archive includes source code, installers, English instructions and license notices. R packages, the Python runtime and model weights are installed separately. The installed environment is substantially larger than the ZIP.

Extract the archive, read START_HERE.md, then run:
1. **1_Install_dependencies.cmd** — install the base dependencies.
2. **2_Start.cmd** — open the application.
3. **3_Install_AI.cmd** — optionally install GrainSight/FastSAM.
4. **4_Check_installation.cmd** — verify the installation.

Requirements: Windows 10/11 x64 Intel/AMD, compatible 64-bit R (tested 4.5.3), and Internet access for setup. Installation, local application startup, CPU model inference and the R–Python segmentation bridge were checked during release preparation.

See the [project README](https://github.com/westabra/Image_Fragmentator#readme) for a fuller overview and the included third-party notices for licenses and attribution.
