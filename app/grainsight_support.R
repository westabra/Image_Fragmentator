# GrainSight integration, 2026-09-29. Existing R code remains GPL-3.0-or-later;
# the combined FastSAM backend is distributed with AGPL-3.0 obligations.

grainsight_runtime <- function() {
  root <- getOption("fragmentator.grainsight_root", file.path(APP_DIR, ".."))
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  python <- getOption("fragmentator.grainsight_python", file.path(root, ".venv", "Scripts", "python.exe"))
  if (.Platform$OS.type != "windows" && !file.exists(python)) python <- file.path(root, ".venv", "bin", "python")
  list(root = root, python = python, script = file.path(root, "segment_cli.py"),
       weights = file.path(root, "weights", "FastSAM-x.pt"))
}

grainsight_preflight <- function() {
  rt <- grainsight_runtime()
  missing <- c(if (!file.exists(rt$python)) "Python environment",
               if (!file.exists(rt$script)) "GrainSight adapter",
               if (!file.exists(rt$weights)) "FastSAM-x weights")
  if (length(missing)) paste("Not installed:", paste(missing, collapse = ", "), "— see ../START_HERE.md.")
  else "Ready: local FastSAM-x model, CPU. Runs only when you click Run segmentation."
}

segment_grainsight <- function(arr, imgsz = 1024, conf = 0.5, iou = 0.7, min_area = 50, max_det = 500) {
  rt <- grainsight_runtime()
  if (!all(file.exists(c(rt$python, rt$script, rt$weights)))) stop(grainsight_preflight())
  if (min(dim(arr)[1:2]) < 16) stop("Select an ROI at least 16 pixels wide and high.")
  if (!requireNamespace("processx", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE))
    stop("Install R packages processx and jsonlite to use GrainSight.")
  folder <- tempfile("grainsight-", tmpdir = FRAGMENTATOR_OUTPUT_DIR)
  dir.create(folder, recursive = TRUE)
  input_path <- file.path(folder, "input.png")
  png::writePNG(arr, input_path)
  request <- list(input = normalizePath(input_path, winslash = "/"),
                  output = file.path(normalizePath(folder, winslash = "/"), "prediction"),
                  imgsz = as.integer(imgsz), conf = as.numeric(conf), iou = as.numeric(iou),
                  min_area = as.integer(min_area), max_det = as.integer(max_det))
  request_path <- file.path(folder, "request.json")
  jsonlite::write_json(request, request_path, auto_unbox = TRUE, pretty = TRUE)
  job <- processx::run(rt$python, c(rt$script, "--request", normalizePath(request_path, winslash = "/")),
                       error_on_status = FALSE, timeout = 300000, windows_hide_window = TRUE)
  writeLines(c(job$stdout, job$stderr), file.path(folder, "backend.log"), useBytes = TRUE)
  if (job$status != 0) stop("GrainSight could not finish. See ", file.path(folder, "backend.log"))
  object_map <- as.matrix(utils::read.csv(file.path(request$output, "labels.csv"), header = FALSE, check.names = FALSE))
  storage.mode(object_map) <- "integer"
  if (!identical(dim(object_map), dim(arr)[1:2]) || anyNA(object_map) || any(object_map < 0L))
    stop("GrainSight returned an invalid label map; previous result retained.")
  meta <- jsonlite::read_json(file.path(request$output, "metadata.json"), simplifyVector = TRUE)
  rendered <- render_object_map_result(arr, object_map)
  c(list(type = "grainsight", object_map = object_map, metadata = meta,
         artifact_dir = normalizePath(folder, winslash = "/")), rendered)
}

grainsight_size_report <- function(obj, exclude_border = TRUE, calibrated = FALSE, metadata = NULL) {
  used <- obj
  if (nrow(used) && isTRUE(exclude_border)) used <- used[!used$touches_border, , drop = FALSE]
  col <- if (isTRUE(calibrated)) "equivalent_diameter_um" else "equivalent_diameter_px"
  unit <- if (isTRUE(calibrated)) "µm" else "pixels"
  x <- if (nrow(used)) used[[col]] else numeric()
  x <- x[is.finite(x) & x > 0]
  summary <- summarize_grain_sizes(obj, exclude_border, calibrated)
  settings <- if (is.null(metadata)) "" else paste0("\nSettings: input size ", metadata$imgsz,
    "; confidence ", metadata$conf, "; detection IoU ", metadata$iou,
    "; minimum area ", metadata$min_area, " working px².")
  list(summary = summary, objects = used,
       dominant = if (length(x)) median(x) else NA_real_, upper_tail_threshold = NA_real_, main_p90 = NA_real_,
       text = paste0("GrainSight / FastSAM — detected object size distribution\n",
         "Objects used: ", length(x), "; unit: ", unit, settings,
         "\nECD and moment axes are measured by Image Fragmentator on the working pixel grid.",
         "\nThese are model-detected objects, not verified mineral grains. Level-2 aggregate classification is not applied.",
         if (!length(x)) "\nNo objects remain after filtering." else ""))
}
