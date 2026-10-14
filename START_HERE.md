# Image Fragmentator 7.7.0 Lite

Windows 10/11, 64-bit Intel/AMD. Install 64-bit R (tested with R 4.5.3) first.
RStudio is optional. Extract the ZIP into a writable folder before running it.

1. Run **1_Install_dependencies.cmd** once with an Internet connection. It installs R packages in this folder's `R-library`.
2. Run **2_Start.cmd**. The application opens in your browser. Keep the console open while working; close it to stop the application.
3. Optional: run **3_Install_AI.cmd** to enable GrainSight/FastSAM segmentation. It downloads an isolated Python interpreter, CPU libraries and the FastSAM-x model, verifies the model and runs an inference check.
4. **4_Check_installation.cmd** checks installed dependencies and, when present, the AI runtime.

The ZIP contains source code, installers and licenses. It intentionally contains no R library, Python runtime, model, personal images or results. The installed program will occupy substantially more disk space than the downloaded ZIP. Allow several GB for full AI setup and temporary installation files. Base processing does not require the AI download; AI segmentation remains unavailable until step 3 completes. After setup, normal processing runs locally without an Internet connection.

## Connect Make Thin Section V70 Lite

Complete both base and AI setup. In the V70 Lite folder, create or edit `fragmentator_path.txt` with one line containing the full path to this extracted Image Fragmentator folder. Example:

```text
C:\Tools\Image_Fragmentator_7.7.0_Lite
```

V70 Lite can then launch this independent installation. Its external-launch check requires the AI runtime and model, even when you intend to use only the base tools. Standalone use through `2_Start.cmd` supports base-only installation.

## Troubleshooting and portability

- If R is not found, create `R_PATH.txt` beside `2_Start.cmd` containing the full path to `Rscript.exe`, without quotes.
- Run base setup again after changing the R major/minor version. R packages are kept in a version-specific folder.
- After an interrupted download, rerun the same installer. Installation errors are displayed in the console. AI validation is saved in `logs/python-check.json`.
- A missing Microsoft Visual C++ runtime may prevent Python libraries loading. Install the official Microsoft Visual C++ Redistributable x64 if Windows reports missing runtime DLLs.
- Calibration profiles are stored in `data`; image processing output is in `app/fragmentator_output`. Back up these folders.
- Once configured, the folder includes its local dependencies. Moving it to another Windows x64 computer still requires a compatible R installation and Windows runtime components. This small release is an online bootstrap distribution, not a ready-to-run offline bundle.

## Sources and licenses

R application: GPL-3.0-or-later. Python adapter: AGPL-3.0-only. Third-party components retain their own licenses. See `LICENSE`, `app/THIRD_PARTY_NOTICES.md`, and the license files in `app` and `upstream`. The FastSAM model is downloaded from the official Ultralytics assets release; it is not a GrainSight-specific fine-tuned model.
