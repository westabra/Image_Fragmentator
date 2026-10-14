# Image Fragmentator 7.7.0

**From thin-section photographs to image segmentation, object measurements and documented petrographic observations.**

Image Fragmentator is an open-source application for geologists, petrographers and researchers who want to extract quantitative information from microscope photographs of rocks. It helps users examine microtextures, separate visually distinct image regions, investigate grain and aggregate boundaries, and describe apparent size distributions.

A microphotograph records much more than an attractive texture. It can also provide a basis for comparing the proportions of image classes, measuring visible objects and documenting how a rock's fabric changes between selected areas. Image Fragmentator brings these tasks into a single interactive workspace, with the original photograph available alongside the results for visual assessment.

## What can you do with it?

- **Work with a whole photograph or a selected area.** Load an image or paste one from the clipboard, then draw a rectangular region of interest directly on the photograph.
- **Separate image regions using different methods.** Apply grayscale thresholding, RGB colour clustering, experimental watershed segmentation or the optional GrainSight/FastSAM backend. Adjust parameters and compare the result with the original image and its overlay.
- **Explore grains within larger domains.** Experimental hierarchical watershed analysis first identifies domains or aggregates, then tests subdivisions within larger objects. Separate views show the initial domains and the final grain candidates.
- **Calibrate measurements.** Enter the known full-image width in millimetres or micrometres, and save calibration profiles for later use. Without calibration, measurements remain in pixels.
- **Examine apparent grain and object sizes.** View object measurements, size histograms and summary statistics from a completed hierarchical watershed or GrainSight analysis. Objects touching the image border can be excluded from size statistics.
- **Keep results for further analysis and reporting.** Export a segmented PNG with a legend, statistics and object measurements as CSV, and selected size reports as CSV or plain text.

For example, a petrographer can select a representative part of a thin-section photograph, set its spatial calibration, compare segmentation methods, inspect the detected boundaries and export the resulting measurements together with an illustrated segmentation result.

## The petrographer remains part of the interpretation

Segmentation identifies image regions and object candidates; it does not automatically identify minerals. A colour class can contain more than one mineral, while a single mineral grain can contain several colours or brightness levels. Detected boundaries therefore need to be checked against the photograph and the geological context.

The reported sizes describe **two-dimensional apparent sections of detected objects**. They are not a direct measurement of the rock's three-dimensional grain-size distribution. Watershed and GrainSight/FastSAM modes are experimental tools for supporting interpretation.

## Standalone use and Make Thin Section

Image Fragmentator works independently and can also be launched from [Make Thin Section V70 Lite](https://github.com/westabra/My_Shiny_App). This allows image analysis to accompany structured petrographic descriptions while the two applications remain separate installations.

The application is written in R with a Shiny interface and opens in a web browser on your own computer. RStudio is not required. The optional GrainSight/FastSAM backend uses a local Python CPU runtime. Once dependencies and the model have been installed, normal image processing runs locally.

## A small download, with optional AI installation

The Lite release ZIP is approximately **165 KB**. It contains the application source, installation scripts, instructions and license notices. R packages, Python libraries and model weights are installed separately; the complete installed environment is much larger than the ZIP.

1. Read [START_HERE.md](START_HERE.md) and install compatible 64-bit R.
2. Run **1_Install_dependencies.cmd** for the base application.
3. Start the program with **2_Start.cmd**.
4. If needed, add GrainSight/FastSAM with **3_Install_AI.cmd**.
5. Use **4_Check_installation.cmd** to check the installation.

Windows 10/11 x64 Intel/AMD; tested with R 4.5.3. Internet access is required during setup.

## Open source and attribution

Users can inspect the processing code, adapt the workflow and contribute improvements. The existing R application is licensed under GPL-3.0-or-later; the Python integration is marked AGPL-3.0-only. Third-party components retain their respective licenses.

The optional AI integration builds on GrainSight by F. Azzam and collaborators and FastSAM from CASIA/Ultralytics. See [third-party notices](app/THIRD_PARTY_NOTICES.md) for attribution, source references and component licenses.
