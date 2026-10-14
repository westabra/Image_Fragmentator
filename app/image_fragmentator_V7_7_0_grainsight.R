# ============================================================
# image_fragmentator_V7_7_0_grainsight.R
# ------------------------------------------------------------
# Standalone / bridge Shiny app for petrographic image segmentation.
#
# Version: V7.7.0 — optional GrainSight / FastSAM
#
# Main methods:
#   1. Grayscale threshold segmentation
#   2. K-means RGB clustering
#   3. Marker-controlled watershed segmentation (experimental)
#   4. GrainSight / FastSAM-x (optional experimental backend)
# Original R code: GPL-3.0-or-later. See THIRD_PARTY_NOTICES.md for the combined AGPL backend.
#
# Main concept:
#   Fragmentator is intended for texture / structure analysis.
#   It is NOT an automatic mineral identification tool.
#
# Standalone concept:
#   The user uploads an image manually or pastes PNG/JPEG image content.
#   Fragmentator performs segmentation and exports:
#       1) PNG figure with segmentation + legend
#       2) CSV table with segmentation statistics
#
#   There is no bridge mode in this version.
#   The app does not write to Make_Thin_Section SQLite database folders.
#
# Author: adapted for Make_Thin_Section workflow
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

repos <- getOption("repos")
if (is.null(repos) || length(repos) == 0 || is.na(repos[["CRAN"]]) || repos[["CRAN"]] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

required_cran_packages <- c("shiny", "magick", "png", "tools", "base64enc")

for (pkg in required_cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!requireNamespace("EBImage", quietly = TRUE)) {
  message("EBImage is not installed. Installing from Bioconductor...")
  BiocManager::install("EBImage", ask = FALSE, update = FALSE)
}

library(shiny)
library(magick)
library(png)
library(tools)
library(EBImage)


# ============================================================
# 1. Global settings
# ============================================================

APP_NAME <- "Image Fragmentator"
APP_VERSION <- "V7.7.0 — optional GrainSight / FastSAM"

MAX_UPLOAD_MB <- 80
MAX_WORKING_DIM <- 2000
JPEG_QUALITY_DEFAULT <- 90
# Keep profile data outside the installation folder; override for portable setups.
CALIBRATION_PROFILE_FILE <- getOption(
  "fragmentator.calibration_profile_file",
  file.path(tools::R_user_dir("ImageFragmentator", "data"), "calibration_profiles.rds")
)
MAX_CLIPBOARD_BYTES <- 40 * 1024^2

options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)


# ============================================================
# 2. Path helpers and standalone export settings
# ============================================================

safe_normalize_path <- function(path, mustWork = FALSE) {
  if (is.null(path) || !nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = mustWork)
}

APP_DIR <- safe_normalize_path(getOption("fragmentator.app_dir", getwd()), mustWork = FALSE)
source(file.path(APP_DIR, "grainsight_support.R"), local = TRUE, encoding = "UTF-8")

# Standalone output folder.
# Fragmentator does not write to Make_Thin_Section database folders.
# Results are exported manually by the user.
FRAGMENTATOR_OUTPUT_DIR <- file.path(APP_DIR, "fragmentator_output")

if (!dir.exists(FRAGMENTATOR_OUTPUT_DIR)) {
  dir.create(FRAGMENTATOR_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}


# ============================================================
# 3. General helper functions
# ============================================================

safe_file_stub <- function(x) {
  x <- ifelse(is.null(x) || !nzchar(x), "unknown", x)
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "unknown" else x
}

timestamp_stub <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

safe_range01 <- function(x) {
  x[is.na(x)] <- 0
  x[x < 0] <- 0
  x[x > 1] <- 1
  x
}

rgb_to_gray <- function(arr) {
  # arr: height x width x 3, values 0..1
  0.299 * arr[, , 1] + 0.587 * arr[, , 2] + 0.114 * arr[, , 3]
}

array_to_rgb_matrix <- function(arr) {
  # Convert height x width x 3 array to n_pixels x 3 RGB matrix.
  # Each row is one pixel: R, G, B.
  arr <- safe_range01(arr)

  if (length(dim(arr)) != 3 || dim(arr)[3] < 3) {
    stop("array_to_rgb_matrix expects height x width x 3 RGB array.")
  }

  cbind(
    as.vector(arr[, , 1]),
    as.vector(arr[, , 2]),
    as.vector(arr[, , 3])
  )
}

rgb_matrix_to_array <- function(mat, h, w) {
  arr <- array(0, dim = c(h, w, 3))
  arr[, , 1] <- matrix(mat[, 1], nrow = h, ncol = w)
  arr[, , 2] <- matrix(mat[, 2], nrow = h, ncol = w)
  arr[, , 3] <- matrix(mat[, 3], nrow = h, ncol = w)
  safe_range01(arr)
}

read_image_as_array <- function(path, max_dim = MAX_WORKING_DIM) {
  img <- magick::image_read(path)
  info_original <- magick::image_info(img)

  original_width <- info_original$width[1]
  original_height <- info_original$height[1]

  scale_factor <- min(1, max_dim / max(original_width, original_height))
  was_reduced <- scale_factor < 1

  if (was_reduced) {
    new_w <- max(1, round(original_width * scale_factor))
    new_h <- max(1, round(original_height * scale_factor))
    geometry <- paste0(new_w, "x", new_h, "!")
    img <- magick::image_resize(img, geometry)
  }

  # Very robust image import route:
  # 1. magick reads and optionally resizes the original file.
  # 2. The working image is written to a temporary PNG.
  # 3. png::readPNG() reads it back as a standard array:
  #      height x width x channels
  #
  # This avoids channel/width/height permutation problems that can appear
  # with magick::image_data() on some systems and lead to three stretched noisy bands.
  img <- magick::image_convert(img, colorspace = "sRGB", format = "png")

  tmp_png <- tempfile(fileext = ".png")
  on.exit({
    if (file.exists(tmp_png)) unlink(tmp_png)
  }, add = TRUE)

  magick::image_write(img, path = tmp_png, format = "png")

  arr <- png::readPNG(tmp_png, native = FALSE)

  # png::readPNG may return:
  #   height x width          for grayscale
  #   height x width x 3      for RGB
  #   height x width x 4      for RGBA
  if (length(dim(arr)) == 2) {
    arr <- array(
      rep(arr, 3),
      dim = c(dim(arr)[1], dim(arr)[2], 3)
    )
  } else if (length(dim(arr)) == 3) {
    if (dim(arr)[3] == 4) {
      # Drop alpha channel
      arr <- arr[, , 1:3, drop = FALSE]
    } else if (dim(arr)[3] == 3) {
      arr <- arr[, , 1:3, drop = FALSE]
    } else {
      stop("Unsupported PNG channel number: ", dim(arr)[3])
    }
  } else {
    stop("Unsupported image array dimensions after PNG conversion.")
  }

  arr <- safe_range01(arr)

  info_working <- list(
    width = dim(arr)[2],
    height = dim(arr)[1]
  )

  list(
    arr = arr,
    original_width = original_width,
    original_height = original_height,
    working_width = info_working$width,
    working_height = info_working$height,
    was_reduced = was_reduced,
    scale_factor = scale_factor,
    path = path
  )
}

rgb_array_to_raster <- function(arr) {
  # Convert height x width x 3 numeric RGB array into a raster matrix of HEX colors.
  # This is more robust than passing a numeric 3D array directly to as.raster(),
  # because some R graphics devices may display numeric arrays incorrectly.
  arr <- safe_range01(arr)

  if (length(dim(arr)) != 3 || dim(arr)[3] < 3) {
    stop("rgb_array_to_raster expects an array with dimensions height x width x 3.")
  }

  h <- dim(arr)[1]
  w <- dim(arr)[2]

  r <- as.vector(arr[, , 1])
  g <- as.vector(arr[, , 2])
  b <- as.vector(arr[, , 3])

  hex <- grDevices::rgb(r, g, b)

  matrix(hex, nrow = h, ncol = w)
}

plot_rgb_array <- function(arr, title = NULL) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  par(mar = c(1, 1, ifelse(is.null(title), 1, 3), 1))
  plot.new()
  plot.window(
    xlim = c(0, w),
    ylim = c(0, h),
    asp = 1,
    xaxs = "i",
    yaxs = "i"
  )
  rasterImage(
    as.raster(rgb_array_to_raster(arr)),
    xleft = 0,
    ybottom = 0,
    xright = w,
    ytop = h,
    interpolate = FALSE
  )
  if (!is.null(title)) title(main = title)
}

cluster_boundary_overlay <- function(arr, cluster_map, alpha = 0.65, boundary_color = c(1, 0, 0)) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  boundary <- matrix(FALSE, nrow = h, ncol = w)

  boundary[-1, ] <- boundary[-1, ] | (cluster_map[-1, ] != cluster_map[-h, ])
  boundary[, -1] <- boundary[, -1] | (cluster_map[, -1] != cluster_map[, -w])

  overlay <- arr

  for (ch in 1:3) {
    tmp <- overlay[, , ch]
    tmp[boundary] <- (1 - alpha) * tmp[boundary] + alpha * boundary_color[ch]
    overlay[, , ch] <- tmp
  }

  safe_range01(overlay)
}

crop_array_by_roi <- function(arr, roi) {
  if (is.null(roi)) return(list(arr = arr, roi = NULL))

  h <- dim(arr)[1]
  w <- dim(arr)[2]

  xmin <- max(1, floor(min(roi$xmin, roi$xmax, na.rm = TRUE)))
  xmax <- min(w, ceiling(max(roi$xmin, roi$xmax, na.rm = TRUE)))
  ymin <- max(1, floor(min(roi$ymin, roi$ymax, na.rm = TRUE)))
  ymax <- min(h, ceiling(max(roi$ymin, roi$ymax, na.rm = TRUE)))

  if (xmax <= xmin || ymax <= ymin) {
    return(list(arr = arr, roi = NULL))
  }

  list(
    arr = arr[ymin:ymax, xmin:xmax, , drop = FALSE],
    roi = list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  )
}




# ============================================================
# 4. Segmentation methods
# ============================================================

segment_threshold <- function(arr, threshold = 0.5, invert = FALSE) {
  gray <- rgb_to_gray(arr)
  mask <- gray >= threshold
  if (isTRUE(invert)) mask <- !mask

  seg <- array(0, dim = dim(arr))
  seg[, , 1] <- mask
  seg[, , 2] <- mask
  seg[, , 3] <- mask

  h <- dim(arr)[1]
  w <- dim(arr)[2]
  total_px <- h * w
  selected_px <- sum(mask, na.rm = TRUE)

  legend <- data.frame(
    class = c("selected", "background"),
    pixels = c(selected_px, total_px - selected_px),
    percent = round(c(selected_px, total_px - selected_px) / total_px * 100, 3),
    hex_color = c("#FFFFFF", "#000000"),
    stringsAsFactors = FALSE
  )

  list(
    mask = mask,
    seg_arr = safe_range01(seg),
    overlay_arr = cluster_boundary_overlay(arr, mask * 1, alpha = 0.65),
    stats = legend,
    legend = legend
  )
}

plot_threshold_legend <- function(legend_df, title = "Threshold legend") {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 2))
  title(main = title)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    text(0.5, 1, "No threshold result")
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(legend_df))) {
    y <- 2 - i + 0.5
    rect(0.05, y - 0.25, 0.18, y + 0.25, col = legend_df$hex_color[i], border = "black")
    label <- paste0(legend_df$class[i], " — ", legend_df$percent[i], "%")
    text(0.22, y, label, adj = c(0, 0.5), cex = 0.85)
  }
}

segment_kmeans <- function(arr, centers = 4, nstart = 25, seed = 123) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  mat <- array_to_rgb_matrix(arr)

  set.seed(seed)
  km <- kmeans(mat, centers = centers, nstart = nstart)

  cluster_map <- matrix(km$cluster, nrow = h, ncol = w)

  centers_rgb <- km$centers
  seg_mat <- centers_rgb[km$cluster, , drop = FALSE]
  seg_arr <- rgb_matrix_to_array(seg_mat, h, w)

  total_px <- h * w
  tab <- table(km$cluster)

  legend <- data.frame(
    cluster = paste0("cluster_", as.integer(names(tab))),
    cluster_id = as.integer(names(tab)),
    pixels = as.integer(tab),
    percent = round(as.integer(tab) / total_px * 100, 3),
    mean_red = round(centers_rgb[as.integer(names(tab)), 1] * 255),
    mean_green = round(centers_rgb[as.integer(names(tab)), 2] * 255),
    mean_blue = round(centers_rgb[as.integer(names(tab)), 3] * 255),
    stringsAsFactors = FALSE
  )

  legend$hex_color <- grDevices::rgb(
    legend$mean_red,
    legend$mean_green,
    legend$mean_blue,
    maxColorValue = 255
  )

  legend <- legend[order(-legend$pixels), ]
  rownames(legend) <- NULL

  list(
    cluster_map = cluster_map,
    seg_arr = safe_range01(seg_arr),
    overlay_arr = cluster_boundary_overlay(arr, cluster_map, alpha = 0.65),
    stats = legend,
    legend = legend,
    kmeans = km
  )
}

plot_segmentation_legend <- function(legend_df, title = "K-means cluster legend", max_items = 30) {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    plot.new()
    title(main = title)
    text(0.5, 0.5, "No clusters detected")
    return(invisible(NULL))
  }

  legend_show <- head(legend_df, max_items)
  n <- nrow(legend_show)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, max(1, n)))
  title(main = title)

  for (i in seq_len(n)) {
    y <- n - i + 0.5

    rect(
      0.05, y - 0.25,
      0.18, y + 0.25,
      col = legend_show$hex_color[i],
      border = "black"
    )

    label <- paste0(
      legend_show$cluster[i],
      " — ",
      legend_show$percent[i],
      "%, RGB(",
      legend_show$mean_red[i], ", ",
      legend_show$mean_green[i], ", ",
      legend_show$mean_blue[i], ")"
    )

    text(0.22, y, label, adj = c(0, 0.5), cex = 0.75)
  }
}

make_watershed_palette <- function(n) {
  if (n <= 0) return(character(0))
  grDevices::hcl.colors(n, palette = "Dynamic")
}

segment_marker_watershed <- function(
    arr,
    threshold = 0.5,
    invert_mask = FALSE,
    blur_sigma = 1.0,
    seed_threshold = 0.35,
    min_object_px = 50
) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  gray <- rgb_to_gray(arr)

  if (!is.null(blur_sigma) && blur_sigma > 0) {
    gray_img <- EBImage::Image(gray, colormode = "Grayscale")
    gray <- drop(as.array(EBImage::gblur(gray_img, sigma = blur_sigma)))
  }

  mask <- gray >= threshold
  if (isTRUE(invert_mask)) mask <- !mask

  mask_img <- EBImage::Image(mask * 1, colormode = "Grayscale")

  # Distance transform inside binary mask.
  dist_img <- EBImage::distmap(mask_img)
  dist_mat <- drop(as.array(dist_img))

  max_dist <- max(dist_mat, na.rm = TRUE)

  if (!is.finite(max_dist) || max_dist <= 0) {
    empty_map <- matrix(0L, nrow = h, ncol = w)
    empty_arr <- array(0, dim = c(h, w, 3))

    return(list(
      object_map = empty_map,
      seg_arr = empty_arr,
      overlay_arr = arr,
      stats = data.frame(),
      legend = data.frame(),
      n_objects = 0
    ))
  }

  # Markers are high-distance internal cores.
  # Higher seed_threshold produces fewer markers.
  seeds_mask <- dist_mat >= seed_threshold * max_dist
  seeds_img <- EBImage::Image(seeds_mask * 1, colormode = "Grayscale")
  seeds <- EBImage::bwlabel(seeds_img)

  n_seeds <- max(drop(as.array(seeds)), na.rm = TRUE)

  # Fallback: if marker generation failed, use connected mask objects.
  if (!is.finite(n_seeds) || n_seeds < 1) {
    seeds <- EBImage::bwlabel(mask_img)
  }

  # Marker-controlled watershed-like propagation constrained by mask.
  labels_img <- EBImage::propagate(
    x = EBImage::Image(-dist_mat, colormode = "Grayscale"),
    seeds = seeds,
    mask = mask_img,
    lambda = 1e-4
  )

  object_map <- round(drop(as.array(labels_img)))
  object_map[!mask] <- 0L

  # Remove small objects
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]

  if (length(ids) > 0) {
    tab <- table(object_map[object_map > 0])
    small_ids <- as.integer(names(tab)[tab < min_object_px])
    if (length(small_ids) > 0) {
      object_map[object_map %in% small_ids] <- 0L
    }
  }

  # Recalculate IDs after cleanup
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]
  n_objects <- length(ids)

  seg_arr <- array(0, dim = c(h, w, 3))

  if (n_objects == 0) {
    return(list(
      object_map = object_map,
      seg_arr = seg_arr,
      overlay_arr = arr,
      stats = data.frame(),
      legend = data.frame(),
      n_objects = 0
    ))
  }

  palette <- make_watershed_palette(n_objects)
  stats_list <- vector("list", n_objects)

  total_px <- h * w

  for (i in seq_along(ids)) {
    id <- ids[i]
    idx <- object_map == id

    px <- sum(idx, na.rm = TRUE)

    mean_r <- mean(arr[, , 1][idx], na.rm = TRUE)
    mean_g <- mean(arr[, , 2][idx], na.rm = TRUE)
    mean_b <- mean(arr[, , 3][idx], na.rm = TRUE)

    col_rgb <- grDevices::col2rgb(palette[i]) / 255

    seg_arr[, , 1][idx] <- col_rgb[1]
    seg_arr[, , 2][idx] <- col_rgb[2]
    seg_arr[, , 3][idx] <- col_rgb[3]

    stats_list[[i]] <- data.frame(
      object = paste0("object_", id),
      object_id = id,
      pixels = px,
      percent = round(px / total_px * 100, 3),
      mean_red = round(mean_r * 255),
      mean_green = round(mean_g * 255),
      mean_blue = round(mean_b * 255),
      hex_color = palette[i],
      stringsAsFactors = FALSE
    )
  }

  legend <- do.call(rbind, stats_list)
  legend <- legend[order(-legend$pixels), ]
  rownames(legend) <- NULL

  overlay_arr <- cluster_boundary_overlay(
    arr = arr,
    cluster_map = object_map,
    alpha = 0.65,
    boundary_color = c(1, 0, 0)
  )

  list(
    object_map = object_map,
    seg_arr = safe_range01(seg_arr),
    overlay_arr = safe_range01(overlay_arr),
    stats = legend,
    legend = legend,
    n_objects = n_objects
  )
}

# 4A. Spatial calibration and object morphometry helpers (V7)
# ============================================================

value_to_um <- function(value, unit = "mm") {
  if (is.null(value) || !is.finite(value) || value <= 0) return(NA_real_)
  if (identical(unit, "mm")) value * 1000 else value
}

# Profiles always refer to the full original image, never the preview or ROI.
valid_calibration_profile <- function(p) {
  if (!is.list(p)) return(FALSE)
  scalar_positive <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  if (!all(vapply(p[c("original_width_px", "original_height_px", "image_width", "um_per_px")],
                  scalar_positive, logical(1)))) return(FALSE)
  if (!is.character(p$name) || length(p$name) != 1L || is.na(p$name) || !nzchar(trimws(p$name))) return(FALSE)
  if (!is.character(p$unit) || length(p$unit) != 1L || !p$unit %in% c("mm", "um")) return(FALSE)
  if (p$original_width_px != floor(p$original_width_px) ||
      p$original_height_px != floor(p$original_height_px)) return(FALSE)
  isTRUE(all.equal(p$um_per_px, value_to_um(p$image_width, p$unit) / p$original_width_px,
                   tolerance = 1e-10))
}

profile_matches_image <- function(p, info) {
  valid_calibration_profile(p) && !is.null(info) &&
    isTRUE(p$original_width_px == info$original_width) &&
    isTRUE(p$original_height_px == info$original_height)
}

safe_read_calibration_profiles <- function(path = CALIBRATION_PROFILE_FILE) {
  if (!file.exists(path)) return(list())
  x <- readRDS(path)
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyDuplicated(names(x)) ||
      !all(vapply(x, valid_calibration_profile, logical(1))) ||
      !identical(names(x), unname(vapply(x, function(p) p$name, character(1))))))) {
    stop("Invalid calibration profile file. It has not been overwritten.")
  }
  x
}

# Read-modify-write under a short lock prevents two app sessions losing profiles.
update_calibration_profiles <- function(name, profile = NULL, path = CALIBRATION_PROFILE_FILE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(path, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Profile store is busy or not writable. Try again.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  x <- safe_read_calibration_profiles(path)
  if (!is.null(profile) && (!valid_calibration_profile(profile) || !identical(name, profile$name)))
    stop("Invalid calibration profile.")
  x[[name]] <- profile
  tmp <- tempfile("profiles-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  if (file.exists(path) && !file.copy(path, paste0(path, ".bak"), overwrite = TRUE))
    stop("Cannot back up the profile store; no changes saved.")
  if (!file.copy(tmp, path, overwrite = TRUE)) stop("Cannot write calibration profiles.")
  x
}

# Accept only image bytes with the advertised signature, then normalize to PNG.
clipboard_image_to_png <- function(data_url, path, max_bytes = MAX_CLIPBOARD_BYTES) {
  if (!is.character(data_url) || length(data_url) != 1L || is.na(data_url) ||
      nchar(data_url, type = "bytes") > 4 * ceiling(max_bytes / 3) + 64)
    stop("Clipboard image is missing or too large (maximum 40 MB).")
  if (!grepl("^data:image/(png|jpeg);base64,", data_url)) stop("Clipboard must contain a PNG or JPEG image.")
  payload <- sub("^data:image/(png|jpeg);base64,", "", data_url)
  if (!nzchar(payload) || nchar(payload) %% 4 != 0 ||
      !grepl("^[A-Za-z0-9+/]+={0,2}$", payload)) stop("Invalid clipboard image encoding.")
  bytes <- base64enc::base64decode(payload)
  if (length(bytes) > max_bytes || length(bytes) < 8) stop("Invalid clipboard image size.")
  is_png <- identical(bytes[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  is_jpeg <- identical(bytes[1:3], as.raw(c(255, 216, 255)))
  if (!(startsWith(data_url, "data:image/png;") && is_png) &&
      !(startsWith(data_url, "data:image/jpeg;") && is_jpeg)) stop("Clipboard image type does not match its bytes.")
  img <- magick::image_read(bytes)
  if (length(img) != 1L) stop("Paste a single still image.")
  magick::image_write(img, path = path, format = "png")
  invisible(path)
}

moment_object_measurements <- function(object_map, calibration = NULL) {
  if (is.null(object_map) || length(object_map) == 0) return(data.frame())

  h <- nrow(object_map)
  w <- ncol(object_map)
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]
  if (length(ids) == 0) return(data.frame())

  um_per_px <- NA_real_
  if (!is.null(calibration) && isTRUE(calibration$valid) && is.finite(calibration$working_um_per_px)) {
    um_per_px <- calibration$working_um_per_px
  }

  out <- vector("list", length(ids))

  for (i in seq_along(ids)) {
    id <- ids[i]
    idx <- object_map == id
    rc <- which(idx, arr.ind = TRUE)
    area_px <- nrow(rc)

    rows <- rc[, 1]
    cols <- rc[, 2]
    centroid_x <- mean(cols)
    centroid_y <- mean(rows)

    touches_border <- any(rows == 1L | rows == h | cols == 1L | cols == w)

    # Moment-based ellipse approximation.
    if (area_px >= 2) {
      cov_xy <- stats::cov(cbind(cols, rows))
      eg <- eigen(cov_xy, symmetric = TRUE)
      vals <- pmax(eg$values, 0)
      ord <- order(vals, decreasing = TRUE)
      vals <- vals[ord]
      vec <- eg$vectors[, ord[1]]
      major_axis_px <- 4 * sqrt(vals[1])
      minor_axis_px <- 4 * sqrt(vals[2])
      orientation_deg <- (atan2(vec[2], vec[1]) * 180 / pi) %% 180
    } else {
      major_axis_px <- NA_real_
      minor_axis_px <- NA_real_
      orientation_deg <- NA_real_
    }

    aspect_ratio <- if (is.finite(major_axis_px) && is.finite(minor_axis_px) && minor_axis_px > 0) {
      major_axis_px / minor_axis_px
    } else NA_real_

    # Four-neighbour digital perimeter in working-image pixels.
    up <- down <- left <- right <- matrix(FALSE, nrow = h, ncol = w)
    if (h > 1) {
      up[2:h, ] <- idx[1:(h - 1), , drop = FALSE]
      down[1:(h - 1), ] <- idx[2:h, , drop = FALSE]
    }
    if (w > 1) {
      left[, 2:w] <- idx[, 1:(w - 1), drop = FALSE]
      right[, 1:(w - 1)] <- idx[, 2:w, drop = FALSE]
    }
    perimeter_px <- sum(idx & !up) + sum(idx & !down) + sum(idx & !left) + sum(idx & !right)

    circularity <- if (is.finite(perimeter_px) && perimeter_px > 0) {
      min(1, 4 * pi * area_px / (perimeter_px^2))
    } else NA_real_

    ecd_px <- 2 * sqrt(area_px / pi)

    out[[i]] <- data.frame(
      object_id = id,
      area_px = area_px,
      equivalent_diameter_px = ecd_px,
      major_axis_px = major_axis_px,
      minor_axis_px = minor_axis_px,
      aspect_ratio = aspect_ratio,
      circularity = circularity,
      orientation_deg = orientation_deg,
      centroid_x_px = centroid_x,
      centroid_y_px = centroid_y,
      touches_border = touches_border,
      area_um2 = if (is.finite(um_per_px)) area_px * um_per_px^2 else NA_real_,
      equivalent_diameter_um = if (is.finite(um_per_px)) ecd_px * um_per_px else NA_real_,
      major_axis_um = if (is.finite(um_per_px)) major_axis_px * um_per_px else NA_real_,
      minor_axis_um = if (is.finite(um_per_px)) minor_axis_px * um_per_px else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  ans <- do.call(rbind, out)
  ans$equivalent_diameter_px <- round(ans$equivalent_diameter_px, 3)
  ans$major_axis_px <- round(ans$major_axis_px, 3)
  ans$minor_axis_px <- round(ans$minor_axis_px, 3)
  ans$aspect_ratio <- round(ans$aspect_ratio, 3)
  ans$circularity <- round(ans$circularity, 3)
  ans$orientation_deg <- round(ans$orientation_deg, 2)
  ans$centroid_x_px <- round(ans$centroid_x_px, 2)
  ans$centroid_y_px <- round(ans$centroid_y_px, 2)
  ans$area_um2 <- round(ans$area_um2, 3)
  ans$equivalent_diameter_um <- round(ans$equivalent_diameter_um, 3)
  ans$major_axis_um <- round(ans$major_axis_um, 3)
  ans$minor_axis_um <- round(ans$minor_axis_um, 3)
  ans
}

summarize_grain_sizes <- function(object_df, exclude_border = TRUE, calibrated = FALSE) {
  if (is.null(object_df) || nrow(object_df) == 0) return(data.frame())

  used <- object_df
  border_excluded <- 0L
  if (isTRUE(exclude_border)) {
    border_excluded <- sum(used$touches_border %in% TRUE, na.rm = TRUE)
    used <- used[!used$touches_border, , drop = FALSE]
  }

  value_col <- if (isTRUE(calibrated)) "equivalent_diameter_um" else "equivalent_diameter_px"
  unit <- if (isTRUE(calibrated)) "um" else "px"
  x <- used[[value_col]]
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(data.frame(
      parameter = c("N total objects", "N used", "Border objects excluded", "Measurement unit"),
      value = c(nrow(object_df), 0, border_excluded, unit),
      stringsAsFactors = FALSE
    ))
  }

  q <- stats::quantile(x, probs = c(0.10, 0.50, 0.90), na.rm = TRUE, names = FALSE, type = 7)
  m <- mean(x)
  sdx <- if (length(x) > 1) stats::sd(x) else NA_real_
  cv <- if (is.finite(m) && m != 0 && is.finite(sdx)) sdx / m else NA_real_

  data.frame(
    parameter = c(
      "N total objects", "N used", "Border objects excluded", "Measurement unit",
      "Mean ECD", "Median ECD (P50)", "SD", "CV", "P10", "P90", "Minimum", "Maximum"
    ),
    value = c(
      nrow(object_df), length(x), border_excluded, unit,
      round(m, 3), round(stats::median(x), 3), round(sdx, 3), round(cv, 3),
      round(q[1], 3), round(q[3], 3), round(min(x), 3), round(max(x), 3)
    ),
    stringsAsFactors = FALSE
  )
}


# V7.6.2: petrographic size-population report.
# The report does not automatically call large objects "grains" or "aggregates".
# A candidate unresolved aggregate is defined conservatively as an object that:
#   1) remains a Level-1 terminal object,
#   2) was actually sent to Level 2 but was not split, and
#   3) lies in the upper size tail (>= P90) of the current filtered population.
# This preserves the main grain/object population while flagging large unresolved parents.
petrographic_size_report <- function(object_df, exclude_border = TRUE, calibrated = FALSE) {
  if (is.null(object_df) || nrow(object_df) == 0) {
    return(list(summary = data.frame(), objects = data.frame(), text = "No hierarchical objects available."))
  }

  used <- object_df
  if (isTRUE(exclude_border) && "touches_border" %in% names(used)) {
    used <- used[!(used$touches_border %in% TRUE), , drop = FALSE]
  }

  value_col <- if (isTRUE(calibrated)) "equivalent_diameter_um" else "equivalent_diameter_px"
  unit <- if (isTRUE(calibrated)) "µm" else "pixels"
  if (!value_col %in% names(used)) {
    return(list(summary = data.frame(), objects = used, text = "Equivalent-diameter column is unavailable."))
  }

  x <- suppressWarnings(as.numeric(used[[value_col]]))
  valid <- is.finite(x) & x > 0
  used <- used[valid, , drop = FALSE]
  x <- x[valid]

  if (length(x) == 0) {
    return(list(summary = data.frame(), objects = used, text = "No finite positive object sizes are available."))
  }

  qs <- stats::quantile(x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE, names = FALSE, type = 7)
  names(qs) <- c("P10", "Q1", "P50", "Q3", "P90")
  iqr_x <- qs["Q3"] - qs["Q1"]
  extreme_fence <- qs["Q3"] + 1.5 * iqr_x
  upper_tail_threshold <- qs["P90"]

  level1 <- if ("level" %in% names(used)) suppressWarnings(as.integer(used$level)) == 1L else rep(FALSE, nrow(used))
  attempted <- if ("refinement_attempted" %in% names(used)) used$refinement_attempted %in% TRUE else rep(FALSE, nrow(used))
  accepted <- if ("refinement_split_accepted" %in% names(used)) used$refinement_split_accepted %in% TRUE else rep(FALSE, nrow(used))

  # Accept an explicit error flag and recognise legacy saved error reasons.
  refinement_error <- if ("refinement_error" %in% names(used)) used$refinement_error %in% TRUE else rep(FALSE, nrow(used))
  if ("refinement_reason" %in% names(used)) {
    refinement_error <- refinement_error | grepl("^Level-2 parent .* error:", used$refinement_reason)
  }
  level1[is.na(level1)] <- FALSE
  unresolved_parent <- level1 & attempted & !accepted & !refinement_error
  upper_tail <- x >= upper_tail_threshold
  candidate_aggregate <- unresolved_parent & upper_tail
  extreme_upper_tail <- unresolved_parent & x > extreme_fence

  # Main population excludes large successfully tested unsplit parents and processing failures.
  # This is intentionally conservative: small/medium unsplit Level-1 objects remain in the main population.
  main_idx <- !candidate_aggregate & !refinement_error
  main_x <- x[main_idx]
  main_q <- if (length(main_x)) stats::quantile(main_x, probs = c(0.10, 0.50, 0.90), na.rm = TRUE, names = FALSE, type = 7) else rep(NA_real_, 3)
  dominant <- if (length(main_x)) stats::median(main_x) else NA_real_

  used$size_population <- ifelse(
    candidate_aggregate,
    "large unresolved Level-1 parent / candidate aggregate",
    "main apparent grain-object population"
  )
  used$size_population[refinement_error] <- "Level-2 processing error / excluded from probable main population"
  used$refinement_error <- refinement_error
  used$upper_tail_P90 <- upper_tail
  used$unresolved_level1_parent <- unresolved_parent
  used$candidate_aggregate <- candidate_aggregate
  used$extreme_upper_tail <- extreme_upper_tail

  agg_x <- x[candidate_aggregate]
  unresolved_x <- x[unresolved_parent]

  summary <- data.frame(
    parameter = c(
      "Measurement unit",
      "N objects used",
      "Objects with Level-2 processing errors (excluded from main population)",
      "Dominant apparent grain–aggregate size (median of main population)",
      "Main population P10",
      "P90 of probable main grain population",
      "Initial upper-tail cutoff (P90 of all terminal objects)",
      "Large unresolved Level-1 parents / candidate aggregates",
      "Candidate aggregate minimum size",
      "Candidate aggregate maximum size",
      "Extreme upper-tail fence (Q3 + 1.5 IQR)",
      "Extreme unresolved Level-1 parents"
    ),
    value = c(
      unit,
      length(x),
      sum(refinement_error),
      round(dominant, 3),
      round(main_q[1], 3),
      round(main_q[3], 3),
      round(upper_tail_threshold, 3),
      sum(candidate_aggregate, na.rm = TRUE),
      if (length(agg_x) > 0) round(min(agg_x), 3) else NA_real_,
      if (length(agg_x) > 0) round(max(agg_x), 3) else NA_real_,
      round(extreme_fence, 3),
      sum(extreme_upper_tail, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )

  agg_text <- if (length(agg_x) > 0) {
    paste0(
      length(agg_x), " large unresolved Level-1 object(s), ",
      round(min(agg_x), 1), "–", round(max(agg_x), 1), " ", unit,
      ". These are candidates for aggregates/domains requiring further refinement, not automatically individual grains."
    )
  } else {
    "No large unresolved Level-1 parent objects fall in the current upper size tail."
  }

  txt <- paste0(
    "Estimated apparent size distribution of mineral grains and unresolved aggregates\nDominant apparent grain–aggregate size: ", round(dominant, 1), " ", unit,
    "\nProbable main grain population (P10–P90): ", round(main_q[1], 1), "–", round(main_q[3], 1), " ", unit,
    "\nInitial upper-tail cutoff (P90 of all terminal objects): ", round(upper_tail_threshold, 1), " ", unit,
    "\nP90 of probable main grain population: ", round(main_q[3], 1), " ", unit,
    "\nHow the two P90 values are calculated: the initial cutoff is the 90th percentile of all terminal objects with finite positive ECD, after the selected border filter. Objects at or above this cutoff are flagged only if they remain at Level 1 after a successfully completed Level-2 analysis that did not accept a split. The main-population P90 is then recalculated after excluding these flagged objects and objects with Level-2 processing errors. The initial P90 still includes all terminal objects after the border filter, including those with processing errors. The values can differ because they describe different populations; other large objects remain in the main population.",
    if (!any(main_idx)) " No probable main population remains; its statistics are unavailable (NA)." else if (!any(candidate_aggregate | refinement_error)) " No objects were excluded from the main population, so both P90 values are equal." else " The main-population P90 can differ after these exclusions; equality is possible with tied sizes.",
    "\nLevel-2 processing errors among used objects: ", sum(refinement_error),
    if (any(refinement_error)) " WARNING: these objects were not successfully evaluated, are not classified as candidate aggregates, and are excluded from the probable main population. Inspect parent diagnostics and rerun after resolving the error." else " (none).",
    "\nLarge unresolved objects: ", agg_text,
    "\nInterpretation: the main population is reported separately from large Level-1 parents that were tested at Level 2 but remained unsplit. This is a heuristic petrographic flag, not mineral identification."
  )

  list(
    summary = summary,
    objects = used,
    text = txt,
    dominant = dominant,
    main_p10 = main_q[1],
    main_p90 = main_q[3],
    upper_tail_threshold = upper_tail_threshold,
    unit = unit
  )
}


# 7a. Auto-fit Watershed helpers (V7.1 pilot)
# ============================================================

watershed_otsu_threshold <- function(arr, blur_sigma = 1.0) {
  gray <- rgb_to_gray(arr)
  if (!is.null(blur_sigma) && is.finite(blur_sigma) && blur_sigma > 0) {
    gray_img <- EBImage::Image(gray, colormode = "Grayscale")
    gray <- drop(as.array(EBImage::gblur(gray_img, sigma = blur_sigma)))
  }

  th <- tryCatch(
    as.numeric(EBImage::otsu(EBImage::Image(gray, colormode = "Grayscale"))),
    error = function(e) NA_real_
  )

  if (!is.finite(th) || th <= 0 || th >= 1) {
    th <- stats::median(gray[is.finite(gray)], na.rm = TRUE)
  }
  if (!is.finite(th)) th <- 0.5
  max(0.01, min(0.99, th))
}

safe_log_change <- function(a, b, eps = 1e-6) {
  if (!is.finite(a) || !is.finite(b)) return(NA_real_)
  abs(log((b + eps) / (a + eps)))
}

downsample_rgb_for_preview <- function(arr, max_dim = 900) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  md <- max(h, w)

  if (!is.finite(max_dim) || max_dim <= 0 || md <= max_dim) {
    return(list(
      arr = arr,
      scale = 1,
      original_height = h,
      original_width = w,
      preview_height = h,
      preview_width = w
    ))
  }

  scale <- max_dim / md
  new_h <- max(1L, as.integer(round(h * scale)))
  new_w <- max(1L, as.integer(round(w * scale)))

  # For parameter selection we only need a fast, aspect-preserving preview.
  # Direct row/column sampling avoids an additional expensive interpolation pass.
  rr <- as.integer(round(seq(1, h, length.out = new_h)))
  cc <- as.integer(round(seq(1, w, length.out = new_w)))
  out <- arr[rr, cc, , drop = FALSE]

  list(
    arr = safe_range01(out),
    scale = scale,
    original_height = h,
    original_width = w,
    preview_height = length(rr),
    preview_width = length(cc)
  )
}

auto_fit_watershed <- function(
    arr,
    threshold,
    invert_mask = FALSE,
    blur_sigma = 1.0,
    min_object_px = 25,
    seed_values = seq(0.10, 0.82, length.out = 19)
) {
  seed_values <- sort(unique(pmax(0.03, pmin(0.95, as.numeric(seed_values)))))
  rows <- vector("list", length(seed_values))

  for (i in seq_along(seed_values)) {
    seed <- seed_values[i]
    ws <- segment_marker_watershed(
      arr = arr,
      threshold = threshold,
      invert_mask = invert_mask,
      blur_sigma = blur_sigma,
      seed_threshold = seed,
      min_object_px = min_object_px
    )

    obj <- moment_object_measurements(ws$object_map, calibration = NULL)
    complete <- if (nrow(obj) > 0) obj[!obj$touches_border, , drop = FALSE] else obj
    ecd <- if (nrow(complete) > 0) complete$equivalent_diameter_px else numeric(0)
    ecd <- ecd[is.finite(ecd) & ecd > 0]

    near_floor_fraction <- if (nrow(complete) > 0) {
      mean(complete$area_px <= max(min_object_px * 1.5, min_object_px + 3), na.rm = TRUE)
    } else {
      1
    }

    rows[[i]] <- data.frame(
      seed_threshold = seed,
      n_objects = nrow(obj),
      n_complete = nrow(complete),
      median_ecd_px = if (length(ecd) > 0) stats::median(ecd) else NA_real_,
      p10_ecd_px = if (length(ecd) > 0) as.numeric(stats::quantile(ecd, 0.10, names = FALSE, na.rm = TRUE)) else NA_real_,
      near_floor_fraction = near_floor_fraction,
      border_fraction = if (nrow(obj) > 0) mean(obj$touches_border %in% TRUE) else 1,
      stringsAsFactors = FALSE
    )
  }

  d <- do.call(rbind, rows)
  n <- nrow(d)

  d$neighbor_change_n <- NA_real_
  d$neighbor_change_median <- NA_real_

  for (i in seq_len(n)) {
    idx <- intersect(c(i - 1L, i + 1L), seq_len(n))
    if (length(idx) > 0) {
      d$neighbor_change_n[i] <- mean(
        vapply(idx, function(j) safe_log_change(d$n_complete[i], d$n_complete[j]), numeric(1)),
        na.rm = TRUE
      )
      d$neighbor_change_median[i] <- mean(
        vapply(idx, function(j) safe_log_change(d$median_ecd_px[i], d$median_ecd_px[j]), numeric(1)),
        na.rm = TRUE
      )
    }
  }

  d$neighbor_change_n[!is.finite(d$neighbor_change_n)] <- 10
  d$neighbor_change_median[!is.finite(d$neighbor_change_median)] <- 10

  # Stability approaches 1 when object count and median ECD change little
  # between adjacent marker-threshold trials.
  d$stability <- exp(-(d$neighbor_change_n + d$neighbor_change_median))

  valid_n <- d$n_complete[d$n_complete > 0 & is.finite(d$n_complete)]
  if (length(valid_n) > 0) {
    max_n <- max(valid_n)
    d$detail <- log1p(d$n_complete) / log1p(max_n)
  } else {
    d$detail <- 0
  }
  d$detail[!is.finite(d$detail)] <- 0

  # Penalize solutions dominated by objects sitting immediately above the
  # minimum-size filter: this is a common sign of over-segmentation/noise.
  d$score <- d$stability * (0.70 + 0.30 * d$detail) *
    pmax(0.05, 1 - 0.80 * d$near_floor_fraction)

  valid <- which(
    d$n_complete >= 3 &
      is.finite(d$median_ecd_px) &
      is.finite(d$score)
  )

  if (length(valid) == 0) {
    suggested_idx <- which.max(d$n_objects)
  } else {
    stable_cut <- max(0.45, 0.80 * max(d$stability[valid], na.rm = TRUE))
    stable <- valid[d$stability[valid] >= stable_cut & d$near_floor_fraction[valid] <= 0.45]
    if (length(stable) == 0) stable <- valid

    # "Smallest stable scale": among sufficiently stable trials, prefer the
    # more detailed solution while retaining the anti-fragmentation penalty.
    stable_score <- d$score[stable] + 0.20 * d$detail[stable]
    suggested_idx <- stable[which.max(stable_score)]
  }

  d$suggested <- FALSE
  d$suggested[suggested_idx] <- TRUE

  list(
    diagnostics = d,
    suggested_idx = suggested_idx,
    suggested_seed = d$seed_threshold[suggested_idx],
    threshold = threshold
  )
}

# Choose a deliberately COARSE stable Level-1 solution for the hierarchy.
# The ordinary auto-fit above searches for the smallest stable object scale;
# that is useful for grain-scale work but is too detailed for Level 1.
# Here Level 1 is explicitly treated as a domain/aggregate detector.
select_level1_domain_scale <- function(auto, mode = c("coarse", "very_coarse", "balanced")) {
  mode <- match.arg(mode)
  d <- auto$diagnostics
  if (is.null(d) || nrow(d) == 0) return(auto)

  valid <- which(
    d$n_complete >= 3 &
      is.finite(d$n_complete) &
      is.finite(d$median_ecd_px) &
      is.finite(d$stability)
  )
  if (length(valid) == 0) return(auto)

  max_n <- max(d$n_complete[valid], na.rm = TRUE)
  # Do not allow the coarse selector to collapse the whole image into only
  # one or two giant blobs. The floor is intentionally weak and scales with
  # the finest object count seen during the preview scan.
  collapse_floor <- max(3L, as.integer(ceiling(0.04 * max_n)))
  usable <- valid[d$n_complete[valid] >= collapse_floor]
  if (length(usable) == 0) usable <- valid

  max_stab <- max(d$stability[usable], na.rm = TRUE)
  stable_cut <- max(0.30, 0.65 * max_stab)
  stable <- usable[
    d$stability[usable] >= stable_cut &
      d$near_floor_fraction[usable] <= 0.60
  ]
  if (length(stable) == 0) stable <- usable

  # Rank stable trials by object count: low n = coarse domain scale.
  counts <- d$n_complete[stable]
  qprob <- switch(mode, very_coarse = 0.20, coarse = 0.35, balanced = 0.55)
  qn <- as.numeric(stats::quantile(counts, probs = qprob, names = FALSE, na.rm = TRUE))
  coarse_pool <- stable[counts <= qn]
  if (length(coarse_pool) == 0) coarse_pool <- stable[which.min(counts)]

  # Within the coarse part of the stable family, prefer the most stable point
  # and mildly penalize solutions sitting on the minimum-object-size floor.
  coarse_score <- d$stability[coarse_pool] *
    pmax(0.05, 1 - 0.60 * d$near_floor_fraction[coarse_pool])
  chosen <- coarse_pool[which.max(coarse_score)]

  d$suggested <- FALSE
  d$suggested[chosen] <- TRUE
  d$level1_domain_choice <- FALSE
  d$level1_domain_choice[chosen] <- TRUE

  auto$diagnostics <- d
  auto$suggested_idx <- chosen
  auto$suggested_seed <- d$seed_threshold[chosen]
  auto$selection_mode <- mode
  auto$collapse_floor_n <- collapse_floor
  auto$selected_n_complete <- d$n_complete[chosen]
  auto$selected_median_ecd_px <- d$median_ecd_px[chosen]
  auto
}

# ============================================================
# 7b. Hierarchical Watershed helpers (V7.4 coarse-to-fine pilot)
# ============================================================

matrix_gradient_magnitude <- function(arr, blur_sigma = 0.8) {
  gray <- rgb_to_gray(arr)

  if (!is.null(blur_sigma) && is.finite(blur_sigma) && blur_sigma > 0) {
    gray <- drop(as.array(EBImage::gblur(
      EBImage::Image(gray, colormode = "Grayscale"),
      sigma = blur_sigma
    )))
  }

  h <- nrow(gray)
  w <- ncol(gray)
  gx <- matrix(0, nrow = h, ncol = w)
  gy <- matrix(0, nrow = h, ncol = w)

  if (w > 2) gx[, 2:(w - 1)] <- (gray[, 3:w, drop = FALSE] - gray[, 1:(w - 2), drop = FALSE]) / 2
  if (w > 1) {
    gx[, 1] <- gray[, 2] - gray[, 1]
    gx[, w] <- gray[, w] - gray[, w - 1]
  }

  if (h > 2) gy[2:(h - 1), ] <- (gray[3:h, , drop = FALSE] - gray[1:(h - 2), , drop = FALSE]) / 2
  if (h > 1) {
    gy[1, ] <- gray[2, ] - gray[1, ]
    gy[h, ] <- gray[h, ] - gray[h - 1, ]
  }

  sqrt(gx^2 + gy^2)
}

# Display coordinates for new interfaces between children of the SAME parent.
# Compare neighbouring label pairs directly so existing parent boundaries and
# background edges are excluded, including at multi-object junctions.
hierarchical_split_boundaries <- function(parent_map, terminal_map) {
  stopifnot(is.matrix(parent_map), is.matrix(terminal_map),
            identical(dim(parent_map), dim(terminal_map)))
  h <- nrow(parent_map)
  w <- ncol(parent_map)
  edges <- list(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0), y1 = numeric(0))
  if (w > 1L) {
    p <- parent_map[, seq_len(w - 1L), drop = FALSE]
    q <- parent_map[, 2:w, drop = FALSE]
    a <- terminal_map[, seq_len(w - 1L), drop = FALSE]
    b <- terminal_map[, 2:w, drop = FALSE]
    ij <- which(p > 0 & p == q & a > 0 & b > 0 & a != b, arr.ind = TRUE)
    edges <- list(x0 = ij[, 2], y0 = h - ij[, 1],
                  x1 = ij[, 2], y1 = h - ij[, 1] + 1)
  }
  if (h > 1L) {
    p <- parent_map[seq_len(h - 1L), , drop = FALSE]
    q <- parent_map[2:h, , drop = FALSE]
    a <- terminal_map[seq_len(h - 1L), , drop = FALSE]
    b <- terminal_map[2:h, , drop = FALSE]
    ij <- which(p > 0 & p == q & a > 0 & b > 0 & a != b, arr.ind = TRUE)
    edges$x0 <- c(edges$x0, ij[, 2] - 1)
    edges$y0 <- c(edges$y0, h - ij[, 1])
    edges$x1 <- c(edges$x1, ij[, 2])
    edges$y1 <- c(edges$y1, h - ij[, 1])
  }
  edges
}

draw_hierarchical_split_boundaries <- function(edges) {
  if (!length(edges$x0)) return(invisible(NULL))
  # Device-space widths remain visible when a large image is scaled down.
  # White over black contrasts with both light and dark object fills.
  do.call(graphics::segments, c(edges, list(col = "black", lwd = 3, lend = "square")))
  do.call(graphics::segments, c(edges, list(col = "white", lwd = 1.2, lend = "square")))
  invisible(NULL)
}

internal_label_boundary <- function(label_map, mask = NULL) {
  h <- nrow(label_map)
  w <- ncol(label_map)
  b <- matrix(FALSE, nrow = h, ncol = w)

  if (w > 1) {
    a <- label_map[, 1:(w - 1), drop = FALSE]
    c <- label_map[, 2:w, drop = FALSE]
    hit <- a > 0 & c > 0 & a != c
    b[, 1:(w - 1)] <- b[, 1:(w - 1), drop = FALSE] | hit
    b[, 2:w] <- b[, 2:w, drop = FALSE] | hit
  }

  if (h > 1) {
    a <- label_map[1:(h - 1), , drop = FALSE]
    c <- label_map[2:h, , drop = FALSE]
    hit <- a > 0 & c > 0 & a != c
    b[1:(h - 1), ] <- b[1:(h - 1), , drop = FALSE] | hit
    b[2:h, ] <- b[2:h, , drop = FALSE] | hit
  }

  if (!is.null(mask)) b <- b & mask
  b
}

expand_binary_mask_1px <- function(mask) {
  h <- nrow(mask)
  w <- ncol(mask)
  out <- mask

  if (h > 1) {
    out[2:h, ] <- out[2:h, , drop = FALSE] | mask[1:(h - 1), , drop = FALSE]
    out[1:(h - 1), ] <- out[1:(h - 1), , drop = FALSE] | mask[2:h, , drop = FALSE]
  }
  if (w > 1) {
    out[, 2:w] <- out[, 2:w, drop = FALSE] | mask[, 1:(w - 1), drop = FALSE]
    out[, 1:(w - 1)] <- out[, 1:(w - 1), drop = FALSE] | mask[, 2:w, drop = FALSE]
  }
  out
}

make_boundary_support_cache <- function(arr, parent_mask, blur_sigma = 0.8) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  grad <- matrix_rgb_gradient_magnitude(arr, blur_sigma = blur_sigma)

  hdiff <- if (w > 1) {
    sqrt(
      (arr[, 1:(w - 1), 1, drop = FALSE][,,1] - arr[, 2:w, 1, drop = FALSE][,,1])^2 +
      (arr[, 1:(w - 1), 2, drop = FALSE][,,1] - arr[, 2:w, 2, drop = FALSE][,,1])^2 +
      (arr[, 1:(w - 1), 3, drop = FALSE][,,1] - arr[, 2:w, 3, drop = FALSE][,,1])^2
    )
  } else {
    matrix(numeric(0), nrow = h, ncol = 0)
  }

  vdiff <- if (h > 1) {
    sqrt(
      (arr[1:(h - 1), , 1, drop = FALSE][,,1] - arr[2:h, , 1, drop = FALSE][,,1])^2 +
      (arr[1:(h - 1), , 2, drop = FALSE][,,1] - arr[2:h, , 2, drop = FALSE][,,1])^2 +
      (arr[1:(h - 1), , 3, drop = FALSE][,,1] - arr[2:h, , 3, drop = FALSE][,,1])^2
    )
  } else {
    matrix(numeric(0), nrow = 0, ncol = w)
  }

  list(
    grad = grad,
    hdiff = hdiff,
    vdiff = vdiff,
    parent_mask = parent_mask
  )
}

boundary_support_score <- function(
    arr,
    label_map,
    parent_mask,
    blur_sigma = 0.8,
    cache = NULL
) {
  b <- internal_label_boundary(label_map, parent_mask)
  n_boundary_px <- sum(b, na.rm = TRUE)

  if (!is.finite(n_boundary_px) || n_boundary_px < 8) {
    return(list(
      score = 0,
      gradient_ratio = 0,
      color_jump_ratio = 0,
      boundary_pixels = n_boundary_px
    ))
  }

  if (is.null(cache)) {
    cache <- make_boundary_support_cache(arr, parent_mask, blur_sigma = blur_sigma)
  }

  grad <- cache$grad
  near_b <- expand_binary_mask_1px(b)
  interior <- parent_mask & !near_b

  g_boundary <- grad[b]
  g_interior <- grad[interior]
  g_boundary <- g_boundary[is.finite(g_boundary)]
  g_interior <- g_interior[is.finite(g_interior)]

  if (length(g_boundary) == 0 || length(g_interior) == 0) {
    grad_ratio <- 0
  } else {
    gb <- as.numeric(stats::quantile(g_boundary, 0.65, na.rm = TRUE, names = FALSE))
    gi <- stats::median(g_interior, na.rm = TRUE)
    grad_ratio <- (gb + 1e-6) / (gi + 1e-6)
  }

  h <- dim(arr)[1]
  w <- dim(arr)[2]
  cross_diff <- numeric(0)
  within_diff <- numeric(0)

  if (w > 1) {
    l1 <- label_map[, 1:(w - 1), drop = FALSE]
    l2 <- label_map[, 2:w, drop = FALSE]
    valid <- l1 > 0 & l2 > 0
    cross <- valid & l1 != l2
    within <- valid & l1 == l2
    d <- cache$hdiff

    cross_diff <- c(cross_diff, d[cross])
    within_diff <- c(within_diff, d[within])
  }

  if (h > 1) {
    l1 <- label_map[1:(h - 1), , drop = FALSE]
    l2 <- label_map[2:h, , drop = FALSE]
    valid <- l1 > 0 & l2 > 0
    cross <- valid & l1 != l2
    within <- valid & l1 == l2
    d <- cache$vdiff

    cross_diff <- c(cross_diff, d[cross])
    within_diff <- c(within_diff, d[within])
  }

  cross_diff <- cross_diff[is.finite(cross_diff)]
  within_diff <- within_diff[is.finite(within_diff)]

  if (length(cross_diff) == 0 || length(within_diff) == 0) {
    color_ratio <- 0
  } else {
    cd <- as.numeric(stats::quantile(cross_diff, 0.65, na.rm = TRUE, names = FALSE))
    wd <- stats::median(within_diff, na.rm = TRUE)
    color_ratio <- (cd + 1e-6) / (wd + 1e-6)
  }

  score <- 0.60 * grad_ratio + 0.40 * color_ratio

  list(
    score = score,
    gradient_ratio = grad_ratio,
    color_jump_ratio = color_ratio,
    boundary_pixels = n_boundary_px
  )
}


crop_parent_for_local_analysis <- function(arr, parent_mask, margin = 2L) {
  rc <- which(parent_mask, arr.ind = TRUE)
  if (nrow(rc) == 0) return(NULL)

  h <- dim(arr)[1]
  w <- dim(arr)[2]
  r1 <- max(1L, min(rc[, 1]) - margin)
  r2 <- min(h, max(rc[, 1]) + margin)
  c1 <- max(1L, min(rc[, 2]) - margin)
  c2 <- min(w, max(rc[, 2]) + margin)

  list(
    arr = arr[r1:r2, c1:c2, , drop = FALSE],
    mask = parent_mask[r1:r2, c1:c2, drop = FALSE],
    r1 = r1,
    r2 = r2,
    c1 = c1,
    c2 = c2
  )
}

local_parent_split_scan_distance <- function(
    arr,
    parent_mask,
    min_child_px = 12,
    boundary_min = 1.08,
    max_children = 12,
    seed_values = seq(0.10, 0.74, length.out = 17)
) {
  cp <- crop_parent_for_local_analysis(arr, parent_mask, margin = 2L)
  if (is.null(cp)) return(list(accepted = FALSE, reason = "empty parent"))

  local_arr <- cp$arr
  local_mask <- cp$mask
  mask_img <- EBImage::Image(local_mask * 1, colormode = "Grayscale")
  dist_mat <- drop(as.array(EBImage::distmap(mask_img)))
  max_dist <- max(dist_mat, na.rm = TRUE)

  if (!is.finite(max_dist) || max_dist <= 1) {
    return(list(accepted = FALSE, reason = "insufficient internal distance structure"))
  }

  # Cache expensive image-derived quantities once per parent rather than once per trial.
  boundary_cache <- make_boundary_support_cache(local_arr, local_mask, blur_sigma = 0.8)
  local_gray <- rgb_to_gray(local_arr)
  local_gray <- drop(as.array(EBImage::normalize(
    EBImage::Image(local_gray, colormode = "Grayscale")
  )))

  seed_values <- sort(unique(pmax(0.03, pmin(0.95, as.numeric(seed_values)))))
  trial_rows <- vector("list", length(seed_values))
  trial_maps <- vector("list", length(seed_values))

  for (i in seq_along(seed_values)) {
    seed <- seed_values[i]
    seeds_mask <- local_mask & dist_mat >= seed * max_dist
    seeds <- EBImage::bwlabel(EBImage::Image(seeds_mask * 1, colormode = "Grayscale"))
    n_seeds <- max(drop(as.array(seeds)), na.rm = TRUE)

    if (!is.finite(n_seeds) || n_seeds < 2 || n_seeds > max_children * 3) {
      trial_rows[[i]] <- data.frame(
        seed_threshold = seed,
        n_children = ifelse(is.finite(n_seeds), n_seeds, 0),
        min_child_area_px = NA_real_,
        median_child_area_px = NA_real_,
        tiny_fraction = 1,
        boundary_support = 0,
        gradient_ratio = 0,
        color_jump_ratio = 0,
        stringsAsFactors = FALSE
      )
      next
    }

    labels_img <- EBImage::propagate(
      x = EBImage::Image(local_gray, colormode = "Grayscale"),
      seeds = seeds,
      mask = mask_img,
      lambda = 1e-4
    )

    lab <- round(drop(as.array(labels_img)))
    lab[!local_mask] <- 0L
    ids <- sort(unique(as.integer(lab)))
    ids <- ids[ids > 0]
    sizes <- if (length(ids) > 0) vapply(ids, function(id) sum(lab == id), numeric(1)) else numeric(0)
    n_children <- length(ids)

    support <- if (n_children >= 2) {
      boundary_support_score(
        local_arr, lab, local_mask, blur_sigma = 0.8, cache = boundary_cache
      )
    } else {
      list(score = 0, gradient_ratio = 0, color_jump_ratio = 0, boundary_pixels = 0)
    }

    tiny_fraction <- if (length(sizes) > 0) mean(sizes < min_child_px) else 1

    trial_rows[[i]] <- data.frame(
      seed_threshold = seed,
      n_children = n_children,
      min_child_area_px = if (length(sizes) > 0) min(sizes) else NA_real_,
      median_child_area_px = if (length(sizes) > 0) stats::median(sizes) else NA_real_,
      tiny_fraction = tiny_fraction,
      boundary_support = support$score,
      gradient_ratio = support$gradient_ratio,
      color_jump_ratio = support$color_jump_ratio,
      stringsAsFactors = FALSE
    )
    trial_maps[[i]] <- lab
  }

  d <- do.call(rbind, trial_rows)
  if (is.null(d) || nrow(d) == 0) return(list(accepted = FALSE, reason = "no split trials"))

  plateau_len <- rep(1L, nrow(d))
  code <- ifelse(is.finite(d$n_children), d$n_children, -999L)
  rr <- rle(code)
  pos <- 1L
  for (j in seq_along(rr$lengths)) {
    ix <- pos:(pos + rr$lengths[j] - 1L)
    plateau_len[ix] <- rr$lengths[j]
    pos <- pos + rr$lengths[j]
  }
  d$plateau_len <- plateau_len

  valid <- which(
    d$n_children >= 2 &
      d$n_children <= max_children &
      d$plateau_len >= 2 &
      d$tiny_fraction <= 0.25 &
      d$boundary_support >= boundary_min &
      is.finite(d$median_child_area_px)
  )

  if (length(valid) == 0) {
    best_support <- if (any(is.finite(d$boundary_support))) max(d$boundary_support, na.rm = TRUE) else 0
    return(list(
      accepted = FALSE,
      reason = "no stable split with sufficient optical boundary support",
      diagnostics = d,
      best_support = best_support,
      crop = cp
    ))
  }

  # Prefer the most detailed split that remains stable and image-supported.
  best_n <- max(d$n_children[valid])
  candidates <- valid[d$n_children[valid] == best_n]
  score <- d$boundary_support[candidates] * pmin(d$plateau_len[candidates], 4) * (1 - d$tiny_fraction[candidates])
  selected <- candidates[which.max(score)]

  list(
    accepted = TRUE,
    reason = "stable image-supported local split",
    selected_idx = selected,
    selected_seed = d$seed_threshold[selected],
    selected_map = trial_maps[[selected]],
    n_children = d$n_children[selected],
    boundary_support = d$boundary_support[selected],
    gradient_ratio = d$gradient_ratio[selected],
    color_jump_ratio = d$color_jump_ratio[selected],
    plateau_len = d$plateau_len[selected],
    diagnostics = d,
    crop = cp
  )
}


matrix_rgb_gradient_magnitude <- function(arr, blur_sigma = 0.8) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  out_sq <- matrix(0, nrow = h, ncol = w)

  for (k in 1:3) {
    ch <- arr[, , k]
    if (!is.null(blur_sigma) && is.finite(blur_sigma) && blur_sigma > 0) {
      ch <- drop(as.array(EBImage::gblur(
        EBImage::Image(ch, colormode = "Grayscale"),
        sigma = blur_sigma
      )))
    }

    gx <- matrix(0, nrow = h, ncol = w)
    gy <- matrix(0, nrow = h, ncol = w)

    if (w > 2) gx[, 2:(w - 1)] <- (ch[, 3:w, drop = FALSE] - ch[, 1:(w - 2), drop = FALSE]) / 2
    if (w > 1) {
      gx[, 1] <- ch[, 2] - ch[, 1]
      gx[, w] <- ch[, w] - ch[, w - 1]
    }
    if (h > 2) gy[2:(h - 1), ] <- (ch[3:h, , drop = FALSE] - ch[1:(h - 2), , drop = FALSE]) / 2
    if (h > 1) {
      gy[1, ] <- ch[2, ] - ch[1, ]
      gy[h, ] <- ch[h, ] - ch[h - 1, ]
    }

    out_sq <- out_sq + gx^2 + gy^2
  }

  sqrt(out_sq)
}

local_parent_split_scan_optical <- function(
    arr,
    parent_mask,
    min_child_px = 12,
    boundary_min = 1.08,
    max_children = 12,
    tolerance_values = seq(0.025, 0.30, length.out = 17),
    ext = 2
) {
  cp <- crop_parent_for_local_analysis(arr, parent_mask, margin = 2L)
  if (is.null(cp)) return(list(accepted = FALSE, reason = "empty parent", source = "optical RGB-gradient"))

  local_arr <- cp$arr
  local_mask <- cp$mask
  boundary_cache <- make_boundary_support_cache(local_arr, local_mask, blur_sigma = 0.8)
  grad <- boundary_cache$grad
  local_gray <- rgb_to_gray(local_arr)
  local_gray <- drop(as.array(EBImage::normalize(
    EBImage::Image(local_gray, colormode = "Grayscale")
  )))
  gv <- grad[local_mask]
  gv <- gv[is.finite(gv)]

  if (length(gv) < 20) {
    return(list(accepted = FALSE, reason = "too few pixels for optical-gradient analysis", source = "optical RGB-gradient"))
  }

  lo <- as.numeric(stats::quantile(gv, 0.05, na.rm = TRUE, names = FALSE))
  hi <- as.numeric(stats::quantile(gv, 0.95, na.rm = TRUE, names = FALSE))
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    return(list(accepted = FALSE, reason = "insufficient RGB-gradient range", source = "optical RGB-gradient"))
  }

  grad_norm <- (grad - lo) / (hi - lo)
  grad_norm <- pmax(pmin(grad_norm, 1), 0)

  # EBImage::watershed detects high regions. By using 1-gradient, relatively
  # uniform grain interiors become peaks while optical boundaries become valleys.
  interior_strength <- (1 - grad_norm) * local_mask
  x <- EBImage::Image(interior_strength, colormode = "Grayscale")

  tolerance_values <- sort(unique(pmax(0.005, pmin(0.80, as.numeric(tolerance_values)))))
  trial_rows <- vector("list", length(tolerance_values))
  trial_maps <- vector("list", length(tolerance_values))

  for (i in seq_along(tolerance_values)) {
    tol <- tolerance_values[i]
    lab_img <- tryCatch(
      EBImage::watershed(x, tolerance = tol, ext = ext),
      error = function(e) NULL
    )

    if (is.null(lab_img)) {
      trial_rows[[i]] <- data.frame(
        local_parameter = tol,
        n_children = 0,
        min_child_area_px = NA_real_,
        median_child_area_px = NA_real_,
        tiny_fraction = 1,
        boundary_support = 0,
        gradient_ratio = 0,
        color_jump_ratio = 0,
        stringsAsFactors = FALSE
      )
      next
    }

    seed_lab <- round(drop(as.array(lab_img)))
    seed_lab[!local_mask] <- 0L
    seed_ids <- sort(unique(as.integer(seed_lab)))
    seed_ids <- seed_ids[seed_ids > 0]

    if (length(seed_ids) >= 2 && length(seed_ids) <= max_children * 3) {
      lab_img <- EBImage::propagate(
        x = EBImage::Image(local_gray, colormode = "Grayscale"),
        seeds = EBImage::Image(seed_lab, colormode = "Grayscale"),
        mask = EBImage::Image(local_mask * 1, colormode = "Grayscale"),
        lambda = 1e-4
      )
      lab <- round(drop(as.array(lab_img)))
    } else {
      lab <- seed_lab
    }

    lab[!local_mask] <- 0L
    ids <- sort(unique(as.integer(lab)))
    ids <- ids[ids > 0]
    sizes <- if (length(ids) > 0) vapply(ids, function(id) sum(lab == id), numeric(1)) else numeric(0)
    n_children <- length(ids)

    support <- if (n_children >= 2 && n_children <= max_children * 3) {
      boundary_support_score(
        local_arr, lab, local_mask, blur_sigma = 0.8, cache = boundary_cache
      )
    } else {
      list(score = 0, gradient_ratio = 0, color_jump_ratio = 0, boundary_pixels = 0)
    }

    tiny_fraction <- if (length(sizes) > 0) mean(sizes < min_child_px) else 1

    trial_rows[[i]] <- data.frame(
      local_parameter = tol,
      n_children = n_children,
      min_child_area_px = if (length(sizes) > 0) min(sizes) else NA_real_,
      median_child_area_px = if (length(sizes) > 0) stats::median(sizes) else NA_real_,
      tiny_fraction = tiny_fraction,
      boundary_support = support$score,
      gradient_ratio = support$gradient_ratio,
      color_jump_ratio = support$color_jump_ratio,
      stringsAsFactors = FALSE
    )
    trial_maps[[i]] <- lab
  }

  d <- do.call(rbind, trial_rows)
  if (is.null(d) || nrow(d) == 0) {
    return(list(accepted = FALSE, reason = "no optical watershed trials", source = "optical RGB-gradient"))
  }

  plateau_len <- rep(1L, nrow(d))
  code <- ifelse(is.finite(d$n_children), d$n_children, -999L)
  rr <- rle(code)
  pos <- 1L
  for (j in seq_along(rr$lengths)) {
    ix <- pos:(pos + rr$lengths[j] - 1L)
    plateau_len[ix] <- rr$lengths[j]
    pos <- pos + rr$lengths[j]
  }
  d$plateau_len <- plateau_len

  valid <- which(
    d$n_children >= 2 &
      d$n_children <= max_children &
      d$plateau_len >= 2 &
      d$tiny_fraction <= 0.25 &
      d$boundary_support >= boundary_min &
      is.finite(d$median_child_area_px)
  )

  if (length(valid) == 0) {
    best_support <- if (any(is.finite(d$boundary_support))) max(d$boundary_support, na.rm = TRUE) else 0
    return(list(
      accepted = FALSE,
      reason = "no stable RGB-gradient split with sufficient boundary support",
      source = "optical RGB-gradient",
      diagnostics = d,
      best_support = best_support,
      crop = cp
    ))
  }

  # Prefer detailed stable solutions, but only after passing the optical-boundary test.
  best_n <- max(d$n_children[valid])
  candidates <- valid[d$n_children[valid] == best_n]
  score <- d$boundary_support[candidates] * pmin(d$plateau_len[candidates], 4) * (1 - d$tiny_fraction[candidates])
  selected <- candidates[which.max(score)]

  list(
    accepted = TRUE,
    reason = "stable RGB-gradient-supported local split",
    source = "optical RGB-gradient",
    selected_idx = selected,
    selected_parameter = d$local_parameter[selected],
    selected_seed = d$local_parameter[selected],
    selected_map = trial_maps[[selected]],
    n_children = d$n_children[selected],
    boundary_support = d$boundary_support[selected],
    gradient_ratio = d$gradient_ratio[selected],
    color_jump_ratio = d$color_jump_ratio[selected],
    plateau_len = d$plateau_len[selected],
    diagnostics = d,
    crop = cp
  )
}

local_parent_split_scan_legacy <- function(
    arr,
    parent_mask,
    min_child_px = 12,
    boundary_min = 1.08,
    max_children = 12,
    seed_values = seq(0.12, 0.72, length.out = 7),
    fast_mode = TRUE
) {
  distance_result <- local_parent_split_scan_distance(
    arr = arr,
    parent_mask = parent_mask,
    min_child_px = min_child_px,
    boundary_min = boundary_min,
    max_children = max_children,
    seed_values = seed_values
  )
  if (is.null(distance_result$source)) distance_result$source <- "distance geometry"

  # Fast mode: if geometry already gives a clearly image-supported stable split,
  # do not run the second expensive optical scan.
  strong_distance <- isTRUE(distance_result$accepted) &&
    !is.null(distance_result$boundary_support) &&
    length(distance_result$boundary_support) == 1 &&
    is.finite(distance_result$boundary_support) &&
    distance_result$boundary_support >= boundary_min * 1.12 &&
    !is.null(distance_result$plateau_len) &&
    length(distance_result$plateau_len) == 1 &&
    is.finite(distance_result$plateau_len) &&
    distance_result$plateau_len >= 2

  if (isTRUE(fast_mode) && strong_distance) {
    distance_result$reason <- paste0(distance_result$reason, " [fast-path accepted]")
    return(distance_result)
  }

  optical_result <- local_parent_split_scan_optical(
    arr = arr,
    parent_mask = parent_mask,
    min_child_px = min_child_px,
    boundary_min = boundary_min,
    max_children = max_children,
    tolerance_values = seq(0.035, 0.28, length.out = length(seed_values)),
    ext = 2
  )

  accepted <- list()
  if (isTRUE(distance_result$accepted)) accepted[[length(accepted) + 1L]] <- distance_result
  if (isTRUE(optical_result$accepted)) accepted[[length(accepted) + 1L]] <- optical_result

  if (length(accepted) == 0) {
    ds <- if (!is.null(distance_result$best_support)) distance_result$best_support else 0
    os <- if (!is.null(optical_result$best_support)) optical_result$best_support else 0
    best <- if (os >= ds) optical_result else distance_result
    best$reason <- paste0(
      "No accepted local split. Distance: ", distance_result$reason,
      "; optical: ", optical_result$reason
    )
    best$best_support <- max(ds, os, na.rm = TRUE)
    return(best)
  }

  # If both mechanisms succeed, favor stronger boundary evidence first and
  # greater stable detail second. This prevents geometry alone from dominating.
  scores <- vapply(accepted, function(z) {
    support <- if (!is.null(z$boundary_support) && is.finite(z$boundary_support)) z$boundary_support else 0
    nchild <- if (!is.null(z$n_children) && is.finite(z$n_children)) z$n_children else 1
    plateau <- if (!is.null(z$plateau_len) && is.finite(z$plateau_len)) z$plateau_len else 1
    support * (1 + 0.08 * log1p(nchild)) * (1 + 0.04 * pmin(plateau, 4))
  }, numeric(1))

  accepted[[which.max(scores)]]
}


# ============================================================
# V7.6.1 numerical safety helpers
# ============================================================
safe_scalar_num <- function(x, default = 0) {
  if (is.null(x) || length(x) < 1) return(default)
  x <- suppressWarnings(as.numeric(x[1]))
  if (!is.finite(x)) default else x
}

safe_scalar_int <- function(x, default = 0L) {
  as.integer(round(safe_scalar_num(x, default)))
}

safe_scalar_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) < 1 || is.na(x[1])) return(default)
  isTRUE(x[1])
}

# ============================================================
# V7.6 Level-2 boundary-first splitter
# ------------------------------------------------------------
# Petrographic rationale:
#   For weak quartz-quartz or amphibole-amphibole contacts it is often easier
#   to detect a persistent internal boundary than to guess the correct grain
#   centres (markers) in advance.  V7.6 therefore detects long RGB-gradient
#   ridges first, tests whether they separate the parent into plausible regions,
#   and performs only ONE final propagate() call for the selected candidate.
# ============================================================

# Keep spatial dimensions while converting NA/non-TRUE values to FALSE.
# %in% returns a vector even when its input is a matrix.
as_binary_mask_matrix <- function(mask) {
  dims <- dim(mask)
  if (length(dims) != 2L) stop("Binary mask must be a two-dimensional matrix.")
  matrix(mask %in% TRUE, nrow = dims[1], ncol = dims[2], dimnames = dimnames(mask))
}

erode_binary_mask_1px <- function(mask) {
  mask <- as_binary_mask_matrix(mask)
  h <- nrow(mask)
  w <- ncol(mask)
  if (h < 3 || w < 3) return(matrix(FALSE, nrow = h, ncol = w))

  up <- matrix(FALSE, nrow = h, ncol = w)
  down <- matrix(FALSE, nrow = h, ncol = w)
  left <- matrix(FALSE, nrow = h, ncol = w)
  right <- matrix(FALSE, nrow = h, ncol = w)

  up[2:h, ] <- mask[1:(h - 1), , drop = FALSE]
  down[1:(h - 1), ] <- mask[2:h, , drop = FALSE]
  left[, 2:w] <- mask[, 1:(w - 1), drop = FALSE]
  right[, 1:(w - 1)] <- mask[, 2:w, drop = FALSE]

  mask & up & down & left & right
}

erode_binary_mask_n <- function(mask, n = 1L) {
  out <- as_binary_mask_matrix(mask)
  n <- max(0L, as.integer(n))
  if (n == 0L) return(out)
  for (i in seq_len(n)) out <- erode_binary_mask_1px(out)
  out
}

close_binary_mask_1px <- function(mask) {
  # Four-neighbour closing: bridge one-pixel gaps without aggressively
  # thickening all ridges.
  erode_binary_mask_1px(expand_binary_mask_1px(as_binary_mask_matrix(mask)))
}

filter_binary_components_by_size <- function(mask, min_px = 8L) {
  mask <- as_binary_mask_matrix(mask)
  if (!any(mask)) return(mask)

  lab <- round(drop(as.array(EBImage::bwlabel(
    EBImage::Image(mask * 1, colormode = "Grayscale")
  ))))
  ids <- sort(unique(as.integer(lab)))
  ids <- ids[ids > 0]
  if (length(ids) == 0) return(matrix(FALSE, nrow = nrow(mask), ncol = ncol(mask)))

  sizes <- vapply(ids, function(id) sum(lab == id), numeric(1))
  keep <- ids[sizes >= max(1L, as.integer(min_px))]
  matrix(lab %in% keep, nrow = nrow(mask), ncol = ncol(mask))
}

edge_mask_overlap_score <- function(a, b) {
  a <- as_binary_mask_matrix(a)
  b <- as_binary_mask_matrix(b)
  if (!identical(dim(a), dim(b))) stop("Boundary masks must have matching dimensions.")
  if (!any(a) || !any(b)) return(0)

  # Allow one-pixel positional tolerance because the same optical boundary can
  # move slightly when the gradient threshold changes.
  ae <- expand_binary_mask_1px(a)
  be <- expand_binary_mask_1px(b)
  cov_a <- sum(a & be) / max(1, sum(a))
  cov_b <- sum(b & ae) / max(1, sum(b))
  0.5 * (cov_a + cov_b)
}

boundary_first_quantiles <- function(sensitivity = c("balanced", "conservative", "sensitive"), fast_mode = TRUE) {
  sensitivity <- match.arg(sensitivity)
  if (identical(sensitivity, "conservative")) {
    q <- c(0.90, 0.86, 0.82, 0.78)
  } else if (identical(sensitivity, "sensitive")) {
    q <- c(0.82, 0.78, 0.74, 0.70)
  } else {
    q <- c(0.86, 0.82, 0.78, 0.74)
  }

  if (!isTRUE(fast_mode)) {
    q <- sort(unique(c(q, q - 0.02, q + 0.02)), decreasing = TRUE)
    q <- q[q >= 0.64 & q <= 0.94]
  }
  q
}

local_parent_split_boundary_first <- function(
    arr,
    parent_mask,
    min_child_px = 12,
    boundary_min = 1.08,
    max_children = 12,
    sensitivity = c("balanced", "conservative", "sensitive"),
    fast_mode = TRUE
) {
  sensitivity <- match.arg(sensitivity)
  min_child_px <- max(1L, safe_scalar_int(min_child_px, 12L))
  boundary_min <- safe_scalar_num(boundary_min, 1.08)
  max_children <- max(2L, safe_scalar_int(max_children, 12L))
  cp <- crop_parent_for_local_analysis(arr, parent_mask, margin = 3L)
  if (is.null(cp)) {
    return(list(
      accepted = FALSE,
      reason = "empty parent",
      source = "boundary-first RGB ridges"
    ))
  }

  local_arr <- cp$arr
  local_mask <- as_binary_mask_matrix(cp$mask)
  parent_area <- sum(local_mask)
  if (!is.finite(parent_area) || parent_area < max(30, min_child_px * 2)) {
    return(list(
      accepted = FALSE,
      reason = "parent too small for boundary-first analysis",
      source = "boundary-first RGB ridges",
      crop = cp
    ))
  }

  # Cache the expensive RGB gradient once for this parent.
  boundary_cache <- make_boundary_support_cache(local_arr, local_mask, blur_sigma = 0.8)
  grad <- boundary_cache$grad

  # Do not let the strong parent/background edge masquerade as an internal
  # grain boundary.  Only ridges at least two pixels inside the parent compete.
  inner_mask <- erode_binary_mask_n(local_mask, n = 2L)
  gv <- grad[inner_mask]
  gv <- gv[is.finite(gv)]
  if (length(gv) < 30) {
    return(list(
      accepted = FALSE,
      reason = "too little internal area for ridge analysis",
      source = "boundary-first RGB ridges",
      crop = cp
    ))
  }

  # Long connected ridge requirement suppresses isolated texture/noise pixels.
  min_ridge_px <- max(8L, as.integer(round(sqrt(parent_area) * 0.05)))
  quantiles <- boundary_first_quantiles(sensitivity, fast_mode = fast_mode)
  candidate_rows <- vector("list", length(quantiles))
  edge_masks <- vector("list", length(quantiles))
  seed_maps <- vector("list", length(quantiles))

  for (i in seq_along(quantiles)) {
    q <- quantiles[i]
    thr <- as.numeric(stats::quantile(gv, q, na.rm = TRUE, names = FALSE))
    if (!is.finite(thr)) {
      candidate_rows[[i]] <- data.frame(
        ridge_quantile = q, n_regions = 1L, ridge_pixels = 0L,
        ridge_fraction = 0, ridge_support = 0, persistence = 0,
        stringsAsFactors = FALSE
      )
      edge_masks[[i]] <- matrix(FALSE, nrow = nrow(local_mask), ncol = ncol(local_mask))
      seed_maps[[i]] <- matrix(0L, nrow = nrow(local_mask), ncol = ncol(local_mask))
      next
    }
    edge <- inner_mask & is.finite(grad) & grad >= thr

    # Bridge only very small gaps, then discard short isolated ridge fragments.
    edge <- close_binary_mask_1px(edge)
    edge <- filter_binary_components_by_size(edge, min_px = min_ridge_px)
    edge_masks[[i]] <- edge

    core_mask <- local_mask & !edge
    core_lab <- round(drop(as.array(EBImage::bwlabel(
      EBImage::Image(core_mask * 1, colormode = "Grayscale")
    ))))
    ids <- sort(unique(as.integer(core_lab)))
    ids <- ids[ids > 0]
    sizes <- if (length(ids) > 0) {
      vapply(ids, function(id) sum(core_lab == id), numeric(1))
    } else {
      numeric(0)
    }

    # Only substantial separated interiors become seeds.  Tiny slivers created
    # by cracks/ridge thickness are ignored rather than treated as grains.
    keep_ids <- if (length(ids) > 0) ids[sizes >= min_child_px] else integer(0)
    seed_lab <- matrix(0L, nrow = nrow(local_mask), ncol = ncol(local_mask))
    if (length(keep_ids) > 0) {
      for (j in seq_along(keep_ids)) seed_lab[core_lab == keep_ids[j]] <- j
    }
    seed_maps[[i]] <- seed_lab

    n_regions <- length(keep_ids)
    edge_vals <- grad[edge]
    interior_vals <- grad[inner_mask & !edge]
    edge_vals <- edge_vals[is.finite(edge_vals)]
    interior_vals <- interior_vals[is.finite(interior_vals)]
    ridge_support <- if (length(edge_vals) > 0 && length(interior_vals) > 0) {
      (as.numeric(stats::quantile(edge_vals, 0.60, na.rm = TRUE, names = FALSE)) + 1e-6) /
        (stats::median(interior_vals, na.rm = TRUE) + 1e-6)
    } else {
      0
    }

    ridge_support <- safe_scalar_num(ridge_support, 0)
    candidate_rows[[i]] <- data.frame(
      ridge_quantile = q,
      n_regions = safe_scalar_int(n_regions, 0L),
      ridge_pixels = safe_scalar_int(sum(edge, na.rm = TRUE), 0L),
      ridge_fraction = safe_scalar_num(sum(edge, na.rm = TRUE) / max(1, parent_area), 0),
      ridge_support = ridge_support,
      persistence = 0,
      stringsAsFactors = FALSE
    )
  }

  d <- do.call(rbind, candidate_rows)
  if (is.null(d) || nrow(d) == 0) {
    return(list(
      accepted = FALSE,
      reason = "no boundary-first ridge candidates",
      source = "boundary-first RGB ridges",
      crop = cp
    ))
  }

  # Spatial persistence, not repeated child COUNT: a useful boundary should
  # reappear in nearly the same place at a neighbouring gradient threshold.
  for (i in seq_len(nrow(d))) {
    scores <- numeric(0)
    if (i > 1) scores <- c(scores, edge_mask_overlap_score(edge_masks[[i]], edge_masks[[i - 1L]]))
    if (i < nrow(d)) scores <- c(scores, edge_mask_overlap_score(edge_masks[[i]], edge_masks[[i + 1L]]))
    d$persistence[i] <- if (length(scores) > 0) safe_scalar_num(max(scores, na.rm = TRUE), 0) else 0
  }

  # Never allow NA/Inf values into logical selection below.
  d$n_regions[!is.finite(d$n_regions)] <- 0
  d$ridge_pixels[!is.finite(d$ridge_pixels)] <- 0
  d$ridge_fraction[!is.finite(d$ridge_fraction)] <- 1
  d$ridge_support[!is.finite(d$ridge_support)] <- 0
  d$persistence[!is.finite(d$persistence)] <- 0

  valid <- which(
    d$n_regions >= 2 &
      d$n_regions <= max_children &
      d$ridge_pixels >= min_ridge_px &
      d$ridge_fraction <= 0.22 &
      d$ridge_support >= boundary_min &
      d$persistence >= 0.35
  )

  if (length(valid) == 0) {
    best_score <- d$ridge_support * (0.5 + d$persistence)
    best_score[!is.finite(best_score)] <- -Inf
    best_i <- if (any(is.finite(best_score))) which.max(best_score) else integer(0)
    return(list(
      accepted = FALSE,
      reason = "no persistent internal ridge network produced 2+ plausible regions",
      source = "boundary-first RGB ridges",
      best_support = if (length(best_i) == 1) d$ridge_support[best_i] else 0,
      boundary_persistence = if (length(best_i) == 1) d$persistence[best_i] else 0,
      ridge_quantile = if (length(best_i) == 1) d$ridge_quantile[best_i] else NA_real_,
      ridge_pixels = if (length(best_i) == 1) d$ridge_pixels[best_i] else 0,
      candidate_regions = if (length(best_i) == 1) d$n_regions[best_i] else 1L,
      diagnostics = d,
      crop = cp
    ))
  }

  # Prefer persistent boundaries first.  Within similarly persistent solutions,
  # allow somewhat greater detail but avoid choosing a dense noisy edge network.
  score <- d$ridge_support[valid] *
    (0.70 + 0.60 * d$persistence[valid]) *
    (1 + 0.06 * log1p(d$n_regions[valid])) *
    pmax(0.45, 1 - 2.0 * d$ridge_fraction[valid])
  score[!is.finite(score)] <- -Inf
  if (!any(is.finite(score))) {
    return(list(
      accepted = FALSE,
      reason = "boundary-first candidate scores were non-finite",
      source = "boundary-first RGB ridges",
      diagnostics = d, crop = cp
    ))
  }
  selected <- valid[which.max(score)]
  if (length(selected) != 1L || !is.finite(selected)) {
    return(list(
      accepted = FALSE,
      reason = "boundary-first candidate selection failed",
      source = "boundary-first RGB ridges",
      diagnostics = d, crop = cp
    ))
  }

  seeds <- seed_maps[[selected]]
  n_seed <- max(seeds, na.rm = TRUE)
  if (!is.finite(n_seed) || n_seed < 2) {
    return(list(
      accepted = FALSE,
      reason = "persistent ridges found but fewer than two viable interior seeds remained",
      source = "boundary-first RGB ridges",
      best_support = d$ridge_support[selected],
      boundary_persistence = d$persistence[selected],
      ridge_quantile = d$ridge_quantile[selected],
      ridge_pixels = d$ridge_pixels[selected],
      candidate_regions = d$n_regions[selected],
      diagnostics = d,
      crop = cp
    ))
  }

  # Only ONE expensive propagate call per parent.  Use normalized RGB-gradient
  # as the manifold so the final division prefers the already detected ridges.
  gvals <- grad[local_mask]
  gvals <- gvals[is.finite(gvals)]
  if (length(gvals) < 2) {
    return(list(
      accepted = FALSE,
      reason = "insufficient finite gradient values for final propagation",
      source = "boundary-first RGB ridges",
      best_support = safe_scalar_num(d$ridge_support[selected], 0),
      boundary_persistence = safe_scalar_num(d$persistence[selected], 0),
      ridge_quantile = safe_scalar_num(d$ridge_quantile[selected], NA_real_),
      ridge_pixels = safe_scalar_int(d$ridge_pixels[selected], 0L),
      candidate_regions = safe_scalar_int(d$n_regions[selected], 1L),
      diagnostics = d, crop = cp
    ))
  }
  lo <- as.numeric(stats::quantile(gvals, 0.05, na.rm = TRUE, names = FALSE))
  hi <- as.numeric(stats::quantile(gvals, 0.95, na.rm = TRUE, names = FALSE))
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    grad_norm <- grad
  } else {
    grad_norm <- pmax(pmin((grad - lo) / (hi - lo), 1), 0)
  }

  labels_img <- EBImage::propagate(
    x = EBImage::Image(grad_norm, colormode = "Grayscale"),
    seeds = EBImage::Image(seeds, colormode = "Grayscale"),
    mask = EBImage::Image(local_mask * 1, colormode = "Grayscale"),
    lambda = 1e-4
  )
  lab <- round(drop(as.array(labels_img)))
  lab[!local_mask] <- 0L

  ids <- sort(unique(as.integer(lab)))
  ids <- ids[ids > 0]
  sizes <- if (length(ids) > 0) vapply(ids, function(id) sum(lab == id), numeric(1)) else numeric(0)
  n_children <- length(ids)
  tiny_fraction <- if (length(sizes) > 0) mean(sizes < min_child_px) else 1

  support <- if (n_children >= 2) {
    boundary_support_score(
      local_arr, lab, local_mask, blur_sigma = 0.8, cache = boundary_cache
    )
  } else {
    list(score = 0, gradient_ratio = 0, color_jump_ratio = 0, boundary_pixels = 0)
  }

  support$score <- safe_scalar_num(support$score, 0)
  support$gradient_ratio <- safe_scalar_num(support$gradient_ratio, 0)
  support$color_jump_ratio <- safe_scalar_num(support$color_jump_ratio, 0)
  tiny_fraction <- safe_scalar_num(tiny_fraction, 1)
  n_children <- safe_scalar_int(n_children, 0L)

  accepted <- isTRUE(
    n_children >= 2 &&
      n_children <= max_children &&
      tiny_fraction <= 0.25 &&
      support$score >= safe_scalar_num(boundary_min, 1.08) * 0.95
  )

  if (!isTRUE(accepted)) {
    return(list(
      accepted = FALSE,
      reason = "persistent ridges found, but final image-guided split failed plausibility/support checks",
      source = "boundary-first RGB ridges",
      n_children = n_children,
      selected_seed = NA_real_,
      plateau_len = NA_integer_,
      boundary_support = support$score,
      best_support = max(safe_scalar_num(d$ridge_support[selected], 0), support$score),
      gradient_ratio = support$gradient_ratio,
      color_jump_ratio = support$color_jump_ratio,
      boundary_persistence = d$persistence[selected],
      ridge_quantile = d$ridge_quantile[selected],
      ridge_pixels = d$ridge_pixels[selected],
      candidate_regions = d$n_regions[selected],
      diagnostics = d,
      crop = cp
    ))
  }

  list(
    accepted = TRUE,
    reason = "persistent internal RGB-gradient ridges support local split",
    source = "boundary-first RGB ridges",
    selected_idx = selected,
    selected_seed = NA_real_,
    plateau_len = NA_integer_,
    selected_map = lab,
    n_children = n_children,
    boundary_support = support$score,
    gradient_ratio = support$gradient_ratio,
    color_jump_ratio = support$color_jump_ratio,
    boundary_persistence = d$persistence[selected],
    ridge_quantile = d$ridge_quantile[selected],
    ridge_pixels = d$ridge_pixels[selected],
    candidate_regions = d$n_regions[selected],
    diagnostics = d,
    crop = cp
  )
}

# V7.6 wrapper. Fast mode is boundary-first only. Thorough mode uses the same
# boundary-first result first, then falls back to the V7.4 marker-first logic
# only if the new method cannot split the parent.
local_parent_split_scan <- function(
    arr,
    parent_mask,
    min_child_px = 12,
    boundary_min = 1.08,
    max_children = 12,
    seed_values = seq(0.12, 0.72, length.out = 7),
    fast_mode = TRUE,
    boundary_sensitivity = c("balanced", "conservative", "sensitive")
) {
  boundary_sensitivity <- match.arg(boundary_sensitivity)

  ridge_result <- local_parent_split_boundary_first(
    arr = arr,
    parent_mask = parent_mask,
    min_child_px = min_child_px,
    boundary_min = boundary_min,
    max_children = max_children,
    sensitivity = boundary_sensitivity,
    fast_mode = fast_mode
  )

  if (isTRUE(ridge_result$accepted) || isTRUE(fast_mode)) return(ridge_result)

  legacy <- local_parent_split_scan_legacy(
    arr = arr,
    parent_mask = parent_mask,
    min_child_px = min_child_px,
    boundary_min = boundary_min,
    max_children = max_children,
    seed_values = seed_values,
    fast_mode = FALSE
  )

  if (isTRUE(legacy$accepted)) {
    legacy$reason <- paste0(legacy$reason, " [V7.4 fallback after boundary-first rejection]")
    return(legacy)
  }

  ridge_score <- if (!is.null(ridge_result$best_support) && is.finite(ridge_result$best_support)) ridge_result$best_support else 0
  legacy_score <- if (!is.null(legacy$best_support) && is.finite(legacy$best_support)) legacy$best_support else 0
  if (ridge_score >= legacy_score) ridge_result else legacy
}

render_object_map_result <- function(arr, object_map) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]
  n_objects <- length(ids)

  seg_arr <- array(0, dim = c(h, w, 3))
  if (n_objects == 0) {
    return(list(
      seg_arr = seg_arr,
      overlay_arr = arr,
      stats = data.frame(),
      legend = data.frame(),
      n_objects = 0
    ))
  }

  palette <- make_watershed_palette(n_objects)
  stats_list <- vector("list", n_objects)
  total_px <- h * w

  for (i in seq_along(ids)) {
    id <- ids[i]
    idx <- object_map == id
    px <- sum(idx, na.rm = TRUE)
    col_rgb <- grDevices::col2rgb(palette[i]) / 255

    seg_arr[, , 1][idx] <- col_rgb[1]
    seg_arr[, , 2][idx] <- col_rgb[2]
    seg_arr[, , 3][idx] <- col_rgb[3]

    stats_list[[i]] <- data.frame(
      object = paste0("object_", id),
      object_id = id,
      pixels = px,
      percent = round(px / total_px * 100, 3),
      mean_red = round(mean(arr[, , 1][idx], na.rm = TRUE) * 255),
      mean_green = round(mean(arr[, , 2][idx], na.rm = TRUE) * 255),
      mean_blue = round(mean(arr[, , 3][idx], na.rm = TRUE) * 255),
      hex_color = palette[i],
      stringsAsFactors = FALSE
    )
  }

  legend <- do.call(rbind, stats_list)
  legend <- legend[order(-legend$pixels), , drop = FALSE]
  rownames(legend) <- NULL

  list(
    seg_arr = safe_range01(seg_arr),
    overlay_arr = safe_range01(cluster_boundary_overlay(
      arr = arr,
      cluster_map = object_map,
      alpha = 0.65,
      boundary_color = c(1, 0, 0)
    )),
    stats = legend,
    legend = legend,
    n_objects = n_objects
  )
}

hierarchical_watershed_two_level <- function(
    arr,
    threshold,
    invert_mask = FALSE,
    blur_sigma = 1.0,
    min_object_px = 25,
    level1_seed_threshold = 0.35,
    parent_size_factor = 0.90,
    boundary_min = 1.08,
    boundary_sensitivity = "balanced",
    max_children = 12,
    child_seed_values = NULL,
    max_parent_trials = 18,
    analysis_mode = c("fast", "thorough"),
    progress_callback = NULL
) {
  analysis_mode <- match.arg(analysis_mode)
  fast_mode <- identical(analysis_mode, "fast")

  threshold <- safe_scalar_num(threshold, 0.5)
  blur_sigma <- max(0, safe_scalar_num(blur_sigma, 1.0))
  min_object_px <- max(1L, safe_scalar_int(min_object_px, 25L))
  level1_seed_threshold <- safe_scalar_num(level1_seed_threshold, 0.35)
  parent_size_factor <- max(0, safe_scalar_num(parent_size_factor, 0.90))
  boundary_min <- max(0, safe_scalar_num(boundary_min, 1.08))
  max_children <- max(2L, safe_scalar_int(max_children, 12L))
  max_parent_trials <- max(1L, safe_scalar_int(max_parent_trials, 18L))

  if (is.null(child_seed_values)) {
    child_seed_values <- if (fast_mode) {
      seq(0.12, 0.72, length.out = 7)
    } else {
      seq(0.10, 0.74, length.out = 17)
    }
  }

  if (!is.null(progress_callback)) {
    progress_callback(stage = "level1_full", done = 0, total = 1, parent_id = NA_integer_)
  }

  # The global parameter search is done on a preview before entering this
  # function. Here Level 1 is therefore computed only ONCE at full resolution.
  level1 <- segment_marker_watershed(
    arr = arr,
    threshold = threshold,
    invert_mask = invert_mask,
    blur_sigma = blur_sigma,
    seed_threshold = level1_seed_threshold,
    min_object_px = min_object_px
  )

  if (!is.null(progress_callback)) {
    progress_callback(stage = "level1_full", done = 1, total = 1, parent_id = NA_integer_)
  }

  parent_map <- level1$object_map
  parent_meas <- moment_object_measurements(parent_map, calibration = NULL)

  if (nrow(parent_meas) == 0) {
    rendered <- render_object_map_result(arr, parent_map)
    return(list(
      level1 = level1,
      terminal_map = parent_map,
      terminal = rendered,
      parent_diagnostics = data.frame(),
      terminal_metadata = data.frame(),
      n_level1 = 0,
      n_terminal = 0,
      n_candidate_parents = 0,
      n_attempted_parents = 0,
      n_error_parents = 0L,
      n_split_parents = 0,
      analysis_mode = analysis_mode
    ))
  }

  median_area <- stats::median(parent_meas$area_px, na.rm = TRUE)
  min_child_px <- max(8L, as.integer(round(min_object_px * 0.5)))

  # V7.4: Level 1 is deliberately coarse, so its objects are parent domains
  # by design. Screening is therefore more permissive than in V7.3 and includes a fallback queue.
  parent_area_floor <- max(
    min_object_px * 3,
    median_area * parent_size_factor,
    min_child_px * 6
  )

  # Sort every Level-1 parent by size once. This also provides a deterministic
  # fallback if the size threshold is too strict.
  size_order <- order(parent_meas$area_px, decreasing = TRUE)
  all_ids_by_size <- parent_meas$object_id[size_order]

  candidate_ids <- parent_meas$object_id[parent_meas$area_px >= parent_area_floor]
  if (length(candidate_ids) > 0) {
    candidate_ids <- all_ids_by_size[all_ids_by_size %in% candidate_ids]
  }

  # Guarantee that the hierarchy actually proceeds beyond Level 1 whenever
  # there are plausible parent objects. If threshold screening selects too few,
  # add the largest domains to reach a small minimum refinement queue.
  min_refine_target <- min(
    max_parent_trials,
    max(1L, min(6L, as.integer(ceiling(nrow(parent_meas) * 0.25))))
  )
  fallback_added <- 0L
  if (length(candidate_ids) < min_refine_target && length(all_ids_by_size) > 0) {
    add_ids <- setdiff(all_ids_by_size, candidate_ids)
    need_n <- min(length(add_ids), min_refine_target - length(candidate_ids))
    if (need_n > 0) {
      candidate_ids <- c(candidate_ids, add_ids[seq_len(need_n)])
      fallback_added <- need_n
    }
  }

  candidate_ids <- unique(candidate_ids)
  candidate_ids <- all_ids_by_size[all_ids_by_size %in% candidate_ids]

  if (length(candidate_ids) > max_parent_trials) {
    candidate_ids <- candidate_ids[seq_len(max_parent_trials)]
  }

  candidate_total <- length(candidate_ids)
  if (!is.null(progress_callback)) {
    progress_callback(stage = "screening", done = 0, total = candidate_total, parent_id = NA_integer_)
  }

  terminal_map <- matrix(0L, nrow = nrow(parent_map), ncol = ncol(parent_map))
  next_terminal_id <- 1L
  diag_rows <- list()
  meta_rows <- list()
  d_i <- 0L
  m_i <- 0L
  attempted_n <- 0L

  parent_ids <- sort(unique(as.integer(parent_map)))
  parent_ids <- parent_ids[parent_ids > 0]

  for (parent_id in parent_ids) {
    pmask <- parent_map == parent_id
    parea <- sum(pmask)
    try_split <- parent_id %in% candidate_ids

    if (try_split) {
      attempted_n <- attempted_n + 1L
      if (!is.null(progress_callback)) {
        progress_callback(
          stage = "level2",
          done = attempted_n - 1L,
          total = candidate_total,
          parent_id = parent_id
        )
      }

      split <- tryCatch(
        local_parent_split_scan(
          arr = arr,
          parent_mask = pmask,
          min_child_px = min_child_px,
          boundary_min = boundary_min,
          max_children = max_children,
          seed_values = child_seed_values,
          fast_mode = fast_mode,
          boundary_sensitivity = boundary_sensitivity
        ),
        error = function(e) {
          list(
            accepted = FALSE,
            n_children = 1L,
            error = TRUE,
            reason = paste0("Level-2 parent ", parent_id, " error: ", conditionMessage(e)),
            source = "boundary-first RGB ridges",
            best_support = 0,
            boundary_persistence = 0,
            ridge_quantile = NA_real_,
            ridge_pixels = 0L,
            candidate_regions = 1L,
            gradient_ratio = 0,
            color_jump_ratio = 0
          )
        }
      )

      if (!is.null(progress_callback)) {
        progress_callback(
          stage = "level2",
          done = attempted_n,
          total = candidate_total,
          parent_id = parent_id
        )
      }
    } else {
      split <- list(
        accepted = FALSE,
        reason = "below parent-size screening threshold"
      )
    }

    if (isTRUE(split$accepted)) {
      cp <- split$crop
      lab <- split$selected_map
      child_ids <- sort(unique(as.integer(lab)))
      child_ids <- child_ids[child_ids > 0]

      for (child_local_id in child_ids) {
        local_idx <- lab == child_local_id
        global_rows <- cp$r1:cp$r2
        global_cols <- cp$c1:cp$c2
        submap <- terminal_map[global_rows, global_cols, drop = FALSE]
        submap[local_idx] <- next_terminal_id
        terminal_map[global_rows, global_cols] <- submap

        m_i <- m_i + 1L
        meta_rows[[m_i]] <- data.frame(
          terminal_id = next_terminal_id,
          parent_id = parent_id,
          level = 2L,
          child_local_id = child_local_id,
          split_parent = TRUE,
          stringsAsFactors = FALSE
        )
        next_terminal_id <- next_terminal_id + 1L
      }

      d_i <- d_i + 1L
      diag_rows[[d_i]] <- data.frame(
        parent_id = parent_id,
        parent_area_px = parea,
        attempted = TRUE,
        split_accepted = TRUE,
        error = FALSE,
        n_children = split$n_children,
        selected_seed = split$selected_seed,
        plateau_len = split$plateau_len,
        boundary_persistence = if (!is.null(split$boundary_persistence)) round(split$boundary_persistence, 3) else NA_real_,
        ridge_quantile = if (!is.null(split$ridge_quantile)) round(split$ridge_quantile, 3) else NA_real_,
        ridge_pixels = if (!is.null(split$ridge_pixels)) split$ridge_pixels else NA_real_,
        candidate_regions = if (!is.null(split$candidate_regions)) split$candidate_regions else NA_integer_,
        boundary_support = round(safe_scalar_num(split$boundary_support, 0), 3),
        split_source = if (!is.null(split$source)) split$source else "distance geometry",
        gradient_ratio = round(safe_scalar_num(split$gradient_ratio, 0), 3),
        color_jump_ratio = round(safe_scalar_num(split$color_jump_ratio, 0), 3),
        reason = split$reason,
        stringsAsFactors = FALSE
      )
    } else {
      terminal_map[pmask] <- next_terminal_id
      m_i <- m_i + 1L
      meta_rows[[m_i]] <- data.frame(
        terminal_id = next_terminal_id,
        parent_id = parent_id,
        level = 1L,
        child_local_id = NA_integer_,
        split_parent = FALSE,
        stringsAsFactors = FALSE
      )
      next_terminal_id <- next_terminal_id + 1L

      d_i <- d_i + 1L
      diag_rows[[d_i]] <- data.frame(
        parent_id = parent_id,
        parent_area_px = parea,
        attempted = try_split,
        split_accepted = FALSE,
        error = isTRUE(split$error),
        n_children = if (!is.null(split$n_children)) split$n_children else 1L,
        selected_seed = if (!is.null(split$selected_seed)) split$selected_seed else NA_real_,
        plateau_len = if (!is.null(split$plateau_len)) split$plateau_len else NA_integer_,
        boundary_persistence = if (!is.null(split$boundary_persistence)) round(split$boundary_persistence, 3) else NA_real_,
        ridge_quantile = if (!is.null(split$ridge_quantile)) round(split$ridge_quantile, 3) else NA_real_,
        ridge_pixels = if (!is.null(split$ridge_pixels)) split$ridge_pixels else NA_real_,
        candidate_regions = if (!is.null(split$candidate_regions)) split$candidate_regions else NA_integer_,
        boundary_support = if (!is.null(split$best_support)) {
          round(safe_scalar_num(split$best_support, 0), 3)
        } else if (!is.null(split$boundary_support)) {
          round(safe_scalar_num(split$boundary_support, 0), 3)
        } else {
          NA_real_
        },
        split_source = if (!is.null(split$source)) split$source else NA_character_,
        gradient_ratio = if (!is.null(split$gradient_ratio)) round(safe_scalar_num(split$gradient_ratio, 0), 3) else NA_real_,
        color_jump_ratio = if (!is.null(split$color_jump_ratio)) round(safe_scalar_num(split$color_jump_ratio, 0), 3) else NA_real_,
        reason = split$reason,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!is.null(progress_callback)) {
    progress_callback(stage = "render", done = 0, total = 1, parent_id = NA_integer_)
  }

  # Safety: preserve any foreground pixels not assigned by a local split.
  missing_fg <- parent_map > 0 & terminal_map == 0
  if (any(missing_fg)) {
    orphan_parents <- sort(unique(parent_map[missing_fg]))
    for (parent_id in orphan_parents) {
      idx <- missing_fg & parent_map == parent_id
      terminal_map[idx] <- next_terminal_id
      m_i <- m_i + 1L
      meta_rows[[m_i]] <- data.frame(
        terminal_id = next_terminal_id,
        parent_id = parent_id,
        level = 1L,
        child_local_id = NA_integer_,
        split_parent = FALSE,
        stringsAsFactors = FALSE
      )
      next_terminal_id <- next_terminal_id + 1L
    }
  }

  terminal <- render_object_map_result(arr, terminal_map)
  diag_df <- if (length(diag_rows) > 0) do.call(rbind, diag_rows) else data.frame()
  meta_df <- if (length(meta_rows) > 0) do.call(rbind, meta_rows) else data.frame()

  if (!is.null(progress_callback)) {
    progress_callback(stage = "render", done = 1, total = 1, parent_id = NA_integer_)
  }

  list(
    level1 = level1,
    terminal_map = terminal_map,
    terminal = terminal,
    parent_diagnostics = diag_df,
    terminal_metadata = meta_df,
    n_level1 = level1$n_objects,
    n_terminal = terminal$n_objects,
    n_candidate_parents = candidate_total,
    n_fallback_parents_added = fallback_added,
    min_refine_target = min_refine_target,
    n_attempted_parents = attempted_n,
    n_error_parents = if (nrow(diag_df) > 0) sum(diag_df$error %in% TRUE) else 0L,
    n_split_parents = if (nrow(diag_df) > 0) sum(diag_df$split_accepted %in% TRUE) else 0,
    level1_seed_threshold = level1_seed_threshold,
    boundary_min = boundary_min,
    boundary_sensitivity = boundary_sensitivity,
    level2_method = "boundary-first persistent RGB-gradient ridges",
    parent_area_floor = parent_area_floor,
    min_child_px = min_child_px,
    analysis_mode = analysis_mode
  )
}


plot_watershed_legend <- function(legend_df, title = "Watershed object legend", max_items = 30) {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    plot.new()
    title(main = title)
    text(0.5, 0.5, "No watershed objects detected")
    return(invisible(NULL))
  }

  legend_show <- head(legend_df, max_items)
  n <- nrow(legend_show)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, max(1, n)))
  title(main = title)

  for (i in seq_len(n)) {
    y <- n - i + 0.5

    rect(
      0.05, y - 0.25,
      0.18, y + 0.25,
      col = legend_show$hex_color[i],
      border = "black"
    )

    label <- paste0(
      legend_show$object[i],
      " — ",
      legend_show$percent[i],
      "%, mean RGB(",
      legend_show$mean_red[i], ", ",
      legend_show$mean_green[i], ", ",
      legend_show$mean_blue[i], ")"
    )

    text(0.22, y, label, adj = c(0, 0.5), cex = 0.70)
  }

  if (nrow(legend_df) > max_items) {
    text(
      0.05, 0.15,
      paste0("Only largest ", max_items, " objects shown."),
      adj = c(0, 0.5),
      cex = 0.75
    )
  }
}


# ============================================================
# 5. Export helpers
# ============================================================

build_segmentation_caption <- function(
    req = NULL,
    method,
    use_roi = FALSE,
    k_centers = NA,
    threshold = NA,
    nstart = NA,
    seed = NA,
    ws_seed_threshold = NA,
    ws_min_object_px = NA
) {
  # Concise caption for exported PNG.
  # Keep only method and parameters used for this segmentation.
  # Do not add interpretative / limitation text here; that belongs to the About tab.

  method_label <- if (method == "kmeans") {
    paste0(
      "k-means RGB clustering",
      if (!is.na(k_centers)) paste0("; k = ", k_centers) else "",
      if (!is.na(nstart)) paste0("; nstart = ", nstart) else "",
      if (!is.na(seed)) paste0("; seed = ", seed) else ""
    )
  } else if (method == "watershed") {
    paste0(
      "marker-controlled watershed segmentation",
      if (!is.na(threshold)) paste0("; mask threshold = ", threshold) else "",
      if (!is.na(ws_seed_threshold)) paste0("; marker threshold = ", ws_seed_threshold) else "",
      if (!is.na(ws_min_object_px)) paste0("; min object size = ", ws_min_object_px, " px") else ""
    )
  } else {
    paste0(
      "grayscale threshold",
      if (!is.na(threshold)) paste0("; threshold = ", threshold) else ""
    )
  }

  roi_label <- if (isTRUE(use_roi)) "ROI used" else "full image"

  paste0(
    "Segmentation result with legend. Method: ",
    method_label,
    "; ",
    roi_label,
    "."
  )
}

export_segmentation_with_legend_png <- function(res, filepath, caption = NULL) {
  png(filename = filepath, width = 1800, height = 1100, res = 150)

  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(5, 1.2), widths = c(2.2, 1))

  if (res$type == "threshold") {
    plot_rgb_array(res$seg_arr, title = "Threshold segmentation mask")
    plot_threshold_legend(res$legend, title = "Threshold legend")
  } else if (res$type == "kmeans") {
    plot_rgb_array(res$seg_arr, title = "K-means segmented image")
    plot_segmentation_legend(res$legend, title = "K-means cluster legend")
  } else if (res$type == "grainsight") {
    plot_rgb_array(res$seg_arr, title = "GrainSight / FastSAM objects")
    plot_watershed_legend(res$legend, title = "Detected object legend")
  } else if (res$type == "watershed") {
    plot_rgb_array(res$seg_arr, title = "Marker-controlled watershed")
    plot_watershed_legend(res$legend, title = "Watershed object legend")
  } else {
    plot_rgb_array(res$seg_arr, title = "Segmentation")
    plot.new()
    text(0.5, 0.5, "No legend")
  }

  par(mar = c(1, 1, 1, 1))
  plot.new()
  if (!is.null(caption) && nzchar(caption)) {
    text(0.02, 0.55, caption, adj = c(0, 0.5), cex = 0.85)
  } else {
    text(0.02, 0.55, "Segmentation result with legend.", adj = c(0, 0.5), cex = 0.85)
  }

  dev.off()

  filepath
}

write_standalone_result_metadata <- function(output_png, output_csv, caption, method = NULL) {
  # Optional local metadata beside exported PNG/CSV.
  # This is not a bridge file and is not used by Make_Thin_Section.
  output_png_abs <- safe_normalize_path(output_png, mustWork = FALSE)
  output_csv_abs <- safe_normalize_path(output_csv, mustWork = FALSE)

  result <- list(
    created_at = Sys.time(),
    output_png = output_png_abs,
    output_csv = output_csv_abs,
    caption = caption,
    method = method,
    app_name = APP_NAME,
    app_version = APP_VERSION,
    standalone_mode = TRUE
  )

  meta_rds <- sub("\\.png$", "_metadata.rds", output_png_abs, ignore.case = TRUE)
  saveRDS(result, meta_rds)

  result
}


# ============================================================
# 6. UI
# ============================================================

ui <- fluidPage(
  titlePanel(paste(APP_NAME, APP_VERSION)),
  tags$style(HTML("#petro_size_report_text, #calibration_profile_details { white-space: pre-wrap; overflow-wrap: anywhere; }")),
  tags$script(HTML("(function () {\n  const limit = 40 * 1024 * 1024;\n  let sending = false;\n  function message(text) {\n    const el = document.getElementById('clipboard_status');\n    if (el) el.textContent = text;\n  }\n  function send(blob) {\n    if (sending) { message('An image is already being transferred. Please wait.'); return; }\n    if (!blob || !['image/png', 'image/jpeg'].includes(blob.type)) {\n      message('No PNG/JPEG image found. Copy the image itself, then press Ctrl+V here, or use Upload image.'); return;\n    }\n    if (blob.size > limit) { message('Clipboard image exceeds 40 MB. Use Upload image.'); return; }\n    if (!window.Shiny || !Shiny.setInputValue) { message('Wait for the app to connect, then paste again.'); return; }\n    sending = true;\n    message('Transferring clipboard image...');\n    const reader = new FileReader();\n    reader.onerror = function () { sending = false; message('Cannot read image. Try Ctrl+V or Upload image.'); };\n    reader.onload = function () {\n      Shiny.setInputValue('clipboard_image', {data_url: reader.result, nonce: Date.now() + '-' + Math.random()}, {priority: 'event'});\n    };\n    reader.readAsDataURL(blob);\n  }\n  document.addEventListener('click', async function (event) {\n    if (!event.target.closest('#paste_image_clipboard')) return;\n    if (!window.isSecureContext || !navigator.clipboard || !navigator.clipboard.read) {\n      message('Clipboard button unavailable here. Open the app in a browser on localhost/127.0.0.1 or HTTPS, or focus the Ctrl+V area below and paste. Upload image also works.'); return;\n    }\n    try {\n      const items = await navigator.clipboard.read();\n      for (const item of items) {\n        const type = item.types.find(t => t === 'image/png' || t === 'image/jpeg');\n        if (type) { send(await item.getType(type)); return; }\n      }\n      message('Clipboard has no PNG/JPEG image. Copy image content (for example a screenshot), or use Upload image.');\n    } catch (err) {\n      message('Clipboard access was denied or is unavailable. Allow browser access, or click the Ctrl+V area and paste; otherwise use Upload image.');\n    }\n  });\n  document.addEventListener('paste', function (event) {\n    if (!event.target.closest('#clipboard_paste_area')) return;\n    event.preventDefault();\n    const items = Array.from((event.clipboardData && event.clipboardData.items) || []);\n    const item = items.find(i => i.kind === 'file' && ['image/png', 'image/jpeg'].includes(i.type));\n    send(item ? item.getAsFile() : null);\n  });\n  $(document).on('shiny:connected', function () {\n    sending = false;\n    Shiny.addCustomMessageHandler('fragmentator_clipboard_status', function (text) {\n      sending = false; message(text);\n    });\n  });\n  $(document).on('shiny:disconnected', function () {\n    sending = false; message('Connection lost. Reconnect before pasting again.');\n  });\n})();")),

  tags$div(
    id = "busy_indicator",
    tags$span(class = "fragmentator-spinner"),
    tags$span("Fragmentation / processing is running...")
  ),

  tags$head(
    tags$style(HTML("
      .small-note {
        font-size: 12px;
        color: #666666;
        line-height: 1.3;
        display: block;
      }
      .compact-sidebar .control-section {
        background: rgba(255, 255, 255, 0.72);
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 8px 10px;
        margin-bottom: 8px;
      }
      .compact-sidebar .section-title {
        font-size: 15px;
        font-weight: 600;
        margin: 0 0 7px 0;
      }
      .compact-sidebar .form-group {
        margin-bottom: 7px;
      }
      .compact-sidebar .control-label {
        margin-bottom: 3px;
        font-size: 12px;
      }
      .compact-sidebar .checkbox {
        margin-top: 4px;
        margin-bottom: 4px;
      }
      .compact-sidebar .row {
        margin-left: -5px;
        margin-right: -5px;
      }
      .compact-sidebar .row > div {
        padding-left: 5px;
        padding-right: 5px;
      }
      .compact-sidebar #run_segmentation,
      .compact-sidebar #run_hierarchical,
      .compact-sidebar #save_to_output_folder,
      .compact-sidebar .shiny-download-link {
        width: 100%;
      }
      .compact-sidebar .shiny-download-link {
        display: block;
        margin-top: 5px;
        white-space: normal;
      }
      .compact-sidebar .run-note {
        margin-top: 6px;
      }
      .compact-sidebar .status-details {
        margin-top: 2px;
      }
      .compact-sidebar .status-details summary {
        cursor: pointer;
        font-weight: 600;
        padding: 6px 2px;
      }
      .compact-sidebar .status-details pre {
        margin-top: 5px;
        font-size: 11px;
        white-space: pre-wrap;
      }
      .warning-box {
        background: #fff3cd;
        border: 1px solid #ffeeba;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .info-box {
        background: #e9f5ff;
        border: 1px solid #b6dcff;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .danger-box {
        background: #f8d7da;
        border: 1px solid #f5c6cb;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      #busy_indicator {
        display: none;
        position: fixed;
        top: 18px;
        right: 18px;
        z-index: 99999;
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid #cccccc;
        border-radius: 10px;
        padding: 12px 16px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.18);
        font-weight: 600;
      }
      .fragmentator-spinner {
        display: inline-block;
        width: 18px;
        height: 18px;
        border: 3px solid #dddddd;
        border-top: 3px solid #337ab7;
        border-radius: 50%;
        animation: fragmentator_spin 0.8s linear infinite;
        vertical-align: middle;
        margin-right: 8px;
      }
      @keyframes fragmentator_spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:busy', function() {
        $('#busy_indicator').show();
      });
      $(document).on('shiny:idle', function() {
        $('#busy_indicator').hide();
      });
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "compact-sidebar",

        div(
          class = "control-section",
          h4(class = "section-title", "1. Image and ROI"),
          fileInput(
            "upload_image",
            "Upload image",
            accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff")
          ),
          tags$button(id = "paste_image_clipboard", type = "button",
                      class = "btn btn-default btn-sm", "Paste image from clipboard"),
          tags$textarea(id = "clipboard_paste_area", rows = 2,
                        style = "width:100%; margin-top:6px;",
                        placeholder = "Click here, then Ctrl+V to paste an image",
                        `aria-label` = "Paste PNG or JPEG image with Ctrl+V"),
          tags$div(id = "clipboard_status", role = "status", `aria-live` = "polite",
                   class = "small-note", "PNG/JPEG clipboard images up to 40 MB. Manual upload remains available."),
          actionButton(
            "clear_roi",
            "Clear ROI / use full image",
            icon = icon("eraser"),
            class = "btn btn-default btn-sm"
          ),
          tags$span(
            class = "small-note",
            "Draw a rectangle on Original Image; it becomes active immediately."
          )
        ),

        div(
          class = "control-section",
          h4(class = "section-title", "2. Spatial calibration"),
          radioButtons(
            "calibration_mode",
            "Calibration",
            choices = c(
              "Pixels only" = "none",
              "Known image width" = "width"
            ),
            selected = "none"
          ),
          conditionalPanel(
            condition = "input.calibration_mode == 'width'",
            fluidRow(
              column(
                7,
                numericInput(
                  "calibration_width", "Image width",
                  value = 2.0, min = 0.0001, step = 0.1, width = "100%"
                )
              ),
              column(
                5,
                selectInput(
                  "calibration_unit", "Unit",
                  choices = c("mm" = "mm", "µm" = "um"),
                  selected = "mm", width = "100%"
                )
              )
            ),
            checkboxInput(
              "inherit_calibration",
              "Keep for next image if pixel dimensions are identical",
              value = TRUE
            )
          ),
          textInput("calibration_profile_name", "Profile name", placeholder = "e.g. Microscope 10x"),
          selectInput("calibration_profile", "Saved calibration profiles", choices = character(0)),
          actionButton("save_calibration_profile", "Save profile", class = "btn btn-default btn-sm"),
          actionButton("load_calibration_profile", "Load profile", class = "btn btn-default btn-sm"),
          actionButton("delete_calibration_profile", "Delete profile", class = "btn btn-default btn-sm"),
          verbatimTextOutput("calibration_profile_details"),
          verbatimTextOutput("calibration_info"),
          tags$span(
            class = "small-note",
            "Calibration is based on the full original image width. Image height and µm/pixel are calculated automatically."
          )
        ),

        div(
          class = "control-section",
          h4(class = "section-title", "3. Method and parameters"),
          selectInput(
            "method",
            "Segmentation method",
            choices = c(
              "Grayscale threshold" = "threshold",
              "K-means RGB clustering" = "kmeans",
              "Watershed (experimental)" = "watershed",
              "GrainSight / FastSAM (experimental)" = "grainsight"
            ),
            selected = "threshold",
            width = "100%"
          ),

          conditionalPanel(
            condition = "input.method == 'grainsight'",
            selectInput("gs_imgsz", "Model input size", choices = c("512" = 512, "1024 (default)" = 1024, "2048" = 2048), selected = 1024),
            sliderInput("gs_conf", "Confidence threshold", min = 0.01, max = 0.99, value = 0.5, step = 0.01),
            sliderInput("gs_iou", "Detection overlap threshold", min = 0.1, max = 0.95, value = 0.7, step = 0.05),
            numericInput("gs_min_area", "Minimum object area (working px²)", value = 50, min = 1, max = 1000000, step = 10),
            numericInput("gs_max_det", "Maximum detections", value = 500, min = 1, max = 1000, step = 50),
            textOutput("grainsight_ready"),
            tags$span(class = "small-note", "Detects image objects; review contours before interpreting them as grains. Increasing resolution does not always improve detection.")
          ),

          conditionalPanel(
            condition = "input.method == 'threshold'",
            sliderInput(
              "threshold", "Threshold",
              min = 0, max = 1, value = 0.5, step = 0.01
            ),
            checkboxInput("threshold_invert", "Invert mask", value = FALSE)
          ),

          conditionalPanel(
            condition = "input.method == 'kmeans'",
            fluidRow(
              column(
                6,
                numericInput(
                  "k_centers", "Clusters, k",
                  value = 4, min = 2, max = 20, step = 1,
                  width = "100%"
                )
              ),
              column(
                6,
                numericInput(
                  "k_nstart", "Random starts",
                  value = 25, min = 1, max = 200, step = 1,
                  width = "100%"
                )
              )
            ),
            numericInput(
              "k_seed", "Random seed",
              value = 123, min = 1, max = 999999, step = 1,
              width = "100%"
            ),
            tags$span(
              class = "small-note",
              "More starts improve stability; seed makes runs reproducible."
            )
          ),

          conditionalPanel(
            condition = "input.method == 'watershed'",
            sliderInput(
              "ws_threshold", "Mask threshold",
              min = 0, max = 1, value = 0.5, step = 0.01
            ),
            checkboxInput("ws_invert_mask", "Invert mask", value = FALSE),
            fluidRow(
              column(
                6,
                numericInput(
                  "ws_blur_sigma", "Smoothing",
                  value = 1.0, min = 0, max = 10, step = 0.5,
                  width = "100%"
                )
              ),
              column(
                6,
                numericInput(
                  "ws_min_object_px", "Min. object, px",
                  value = 50, min = 1, max = 100000, step = 10,
                  width = "100%"
                )
              )
            ),
            sliderInput(
              "ws_seed_threshold", "Marker threshold",
              min = 0.05, max = 0.95, value = 0.35, step = 0.05
            ),
            tags$span(
              class = "small-note",
              "Higher marker threshold usually produces fewer objects."
            ),
            tags$details(
              class = "status-details",
              tags$summary("Hierarchical V7.6 settings"),
              checkboxInput(
                "ws_hier_use_otsu",
                "Use automatic Otsu mask for hierarchical detection",
                value = TRUE
              ),
              selectInput(
                "ws_hier_speed_mode",
                "Hierarchical processing mode",
                choices = c(
                  "Fast coarse-to-fine (recommended)" = "fast",
                  "Thorough (slower)" = "thorough"
                ),
                selected = "fast",
                width = "100%"
              ),
              numericInput(
                "ws_hier_preview_dim",
                "Level-1 preview maximum dimension (px)",
                value = 900, min = 400, max = 1800, step = 100, width = "100%"
              ),
              selectInput(
                "ws_hier_level1_coarseness",
                "Level-1 hierarchy scale",
                choices = c(
                  "Coarse domains / aggregates (recommended)" = "coarse",
                  "Very coarse domains" = "very_coarse",
                  "Balanced" = "balanced"
                ),
                selected = "coarse",
                width = "100%"
              ),
              numericInput(
                "ws_hier_boundary_min",
                "Minimum optical boundary support",
                value = 1.08, min = 0.8, max = 3.0, step = 0.02, width = "100%"
              ),
              selectInput(
                "ws_hier_boundary_sensitivity",
                "Level-2 boundary sensitivity",
                choices = c(
                  "Balanced (recommended)" = "balanced",
                  "Conservative — strongest ridges only" = "conservative",
                  "Sensitive — include weaker ridges" = "sensitive"
                ),
                selected = "balanced",
                width = "100%"
              ),
              numericInput(
                "ws_hier_parent_factor",
                "Parent-size screening factor",
                value = 0.90, min = 0.5, max = 8.0, step = 0.1, width = "100%"
              ),
              numericInput(
                "ws_hier_max_parents",
                "Maximum parent domains to refine",
                value = 18, min = 1, max = 100, step = 1, width = "100%"
              ),
              numericInput(
                "ws_hier_max_children",
                "Maximum children per parent",
                value = 12, min = 2, max = 40, step = 1, width = "100%"
              ),
              tags$span(
                class = "small-note",
                "V7.6 keeps the coarse Level-1 hierarchy from V7.4. Level 2 is boundary-first: it looks for long, spatially persistent RGB-gradient ridges inside each parent and only then performs one final image-guided split. This avoids repeated marker searches."
              )
            )
          )
        ),

        div(
          class = "control-section",
          h4(class = "section-title", "4. Process"),
          actionButton(
            "run_segmentation",
            "Run segmentation",
            icon = icon("play"),
            class = "btn btn-success"
          ),
          tags$span(
            class = "small-note run-note",
            "Runs only on click, using the settings above."
          ),
          conditionalPanel(
            condition = "input.method == 'watershed'",
            tags$hr(style = "margin: 8px 0;"),
            actionButton(
              "run_hierarchical",
              "Run Hierarchical Grain Detection",
              icon = icon("sitemap"),
              class = "btn btn-primary"
            ),
            tags$span(
              class = "small-note run-note",
              "Independent V7.6 analysis. It runs only when this button is clicked: coarse Level 1 first, then boundary-first Level 2 refinement inside selected parent domains."
            )
          )
        ),

        div(
          class = "control-section",
          h4(class = "section-title", "5. Export"),
          actionButton(
            "save_to_output_folder",
            "Save PNG + CSV to output folder",
            icon = icon("save"),
            class = "btn btn-primary btn-sm"
          ),
          downloadButton("download_png", "Download PNG + legend"),
          downloadButton("download_csv", "Download statistics CSV"),
          conditionalPanel(
            condition = "input.method == 'watershed' || input.method == 'grainsight'",
            downloadButton("download_hier_objects_csv", "Download size-source objects CSV"),
            downloadButton("download_hier_summary_csv", "Download grain-size summary CSV"),
            downloadButton("download_petro_size_report_csv", "Download selected size report CSV"),
            downloadButton("download_size_report_txt", "Download selected size report TXT")
          )
        ),

        tags$details(
          class = "status-details",
          tags$summary("Status and image details"),
          verbatimTextOutput("status_text")
        )
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        tabPanel(
          "Original Image",
          br(),
          div(
            class = "info-box",
            strong("ROI selection"),
            br(),
            "Drag a rectangle directly on the original image. The selected area immediately becomes the active ROI. ",
            "To return to the full image, click ",
            strong("Clear ROI / use full image"),
            " in the left panel."
          ),
          plotOutput(
            "plot_original_image",
            height = "650px",
            brush = brushOpts(
              id = "input_brush",
              resetOnNew = TRUE
            )
          )
        ),

        tabPanel(
          "ROI preview",
          br(),
          div(
            class = "info-box",
            "This tab shows the currently selected ROI that will be passed to the segmentation method. ",
            "If no ROI is selected, the full image is shown."
          ),
          verbatimTextOutput("roi_text"),
          plotOutput("plot_roi_preview", height = "650px")
        ),

        tabPanel(
          "Segmentation",
          br(),
          div(
            class = "info-box",
            "Results on this tab change only after you click ",
            strong("Run segmentation"),
            ". You can adjust the method and parameters without starting a calculation."
          ),
          fluidRow(
            column(
              width = 6,
              h4("Input image / selected ROI"),
              plotOutput("plot_input", height = "520px")
            ),
            column(
              width = 6,
              h4("Segmentation result"),
              plotOutput("plot_segmented", height = "520px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              h4("Overlay"),
              plotOutput("plot_overlay", height = "420px")
            ),
            column(
              width = 6,
              h4("Legend"),
              plotOutput("plot_legend", height = "420px")
            )
          )
        ),

        tabPanel(
          "Hierarchical Watershed",
          br(),
          div(
            class = "info-box",
            strong("V7.4 coarse-to-fine two-level pilot. "),
            "Level 1 preserves stable domains / aggregates. Large parent objects are then tested locally using distance geometry and RGB-gradient boundaries. ",
            "The analysis changes only after clicking Run Hierarchical Grain Detection."
          ),
          verbatimTextOutput("hierarchical_status"),
          checkboxInput("show_hierarchical_boundaries", "Highlight new Level-2 boundaries", value = TRUE),
          fluidRow(
            column(
              4,
              h4("Level 1 — domains / aggregates"),
              plotOutput("plot_hier_level1", height = "420px")
            ),
            column(
              4,
              h4("Terminal grain candidates"),
              plotOutput("plot_hier_terminal", height = "420px")
            ),
            column(
              4,
              h4("Original image / selected ROI"),
              plotOutput("plot_hier_input", height = "420px")
            )
          ),
          helpText("Object maps show separate objects in different colors without the photograph underneath. Black areas are outside the detected objects. The original image / ROI is from the same completed run."),
          helpText("On the terminal map, white lines with a black outline mark new divisions inside Level-1 domains. Toggle the highlight to compare; no recalculation is needed."),
          checkboxInput("show_hierarchical_parent_table", "Show parent-object split diagnostics", value = FALSE),
          conditionalPanel(
            condition = "input.show_hierarchical_parent_table",
            h4("Parent-object split diagnostics"),
            div(style = "overflow-x: auto;", tableOutput("hierarchical_parent_table"))
          )
        ),

        tabPanel(
          "Apparent Grain–Aggregate Size Distribution",
          br(),
          div(
            class = "info-box",
            strong("2-D apparent grain / object size distribution. "),
            "Choose a completed Hierarchical Watershed or GrainSight result below. Calibration and border filtering apply to that result."
          ),
          selectInput("size_source", "Size measurements from", choices = c("Hierarchical Watershed" = "hierarchical", "GrainSight / FastSAM" = "grainsight"), selected = "hierarchical"),
          checkboxInput(
            "exclude_border_objects",
            "Exclude border-touching objects from size statistics",
            value = TRUE
          ),
          verbatimTextOutput("grain_size_status"),
          plotOutput("grain_size_histogram", height = "430px"),
          h4("Apparent grain / object size report"),
          div(
            class = "info-box",
            "Hierarchical Watershed can flag large unresolved parents. GrainSight reports detected object sizes without applying the Level-2 aggregate flag."
          ),
          verbatimTextOutput("petro_size_report_text"),
          tableOutput("petro_size_report_table"),
          h4("Standard summary"),
          tableOutput("grain_size_summary_table"),
          h4("Object measurements"),
          tableOutput("grain_object_table")
        ),

        tabPanel(
          "Statistics",
          br(),
          tableOutput("stats_table")
        ),

        tabPanel(
          "Output info",
          br(),
          h4("Standalone output folder"),
          verbatimTextOutput("output_paths"),
          h4("Caption for last fragmentation"),
          verbatimTextOutput("last_fragmentation_caption_text"),
          h4("Last saved standalone result"),
          verbatimTextOutput("last_saved_result_text")
        ),

        tabPanel(
          "About",
          br(),
          h3("Image Fragmentator"),
          p(strong("Version:"), APP_VERSION),
          p("Optional GrainSight / FastSAM-x backend: GrainSight by F. Azzam and collaborators (Apache-2.0), FastSAM by CASIA and Ultralytics (AGPL-3.0 stack). Existing R code remains GPL-3.0-or-later; combined-use requirements and sources are listed in THIRD_PARTY_NOTICES.md."),
          p(tags$a(href = "https://github.com/fazzam12345/grainsight", "GrainSight source"), " | ",
            tags$a(href = "https://github.com/CASIA-LMC-Lab/FastSAM", "FastSAM source")),
          div(
            class = "info-box",
            strong("Purpose. "),
            "Image Fragmentator is a standalone Shiny application for exploratory segmentation and two-level hierarchical grain/domain analysis of petrographic and other scientific images. ",
            "It separates visually contrasting image domains and provides reproducible statistics for the selected method and settings."
          ),

          h4("Recommended workflow"),
          tags$ol(
            tags$li(strong("Upload an image."), " JPG, PNG and TIFF files are supported."),
            tags$li(
              strong("Choose the analysis area."),
              " Drag a rectangle on the Original Image tab to activate an ROI, or use the full image."
            ),
            tags$li(strong("Select a segmentation method."), " Only parameters relevant to that method are shown."),
            tags$li(strong("Adjust the parameters."), " Changing a control does not start processing."),
            tags$li(
              strong("Click Run segmentation."),
              " The result is calculated only on explicit request."
            ),
            tags$li(strong("Review and export."), " Compare the input, segmentation, overlay, legend and statistics before saving. For Watershed, Hierarchical Grain Detection is an independent explicit-run analysis."),
            tags$li(
              strong("Repeat if required."),
              " Change the ROI or settings and click Run segmentation again."
            )
          ),

          h4("ROI behaviour"),
          tags$ul(
            tags$li("The image is displayed at its correct aspect ratio; selecting an ROI does not stretch the source data."),
            tags$li("A brushed rectangle becomes active immediately and is outlined in red."),
            tags$li("Clear ROI / use full image removes the selection."),
            tags$li("ROI preview shows exactly which image area will be used on the next run."),
            tags$li("The Segmentation tab preserves the input used for the last completed run, even if the current ROI is changed later.")
          ),

          h4("Segmentation methods"),
          tags$table(
            class = "table table-bordered table-condensed",
            tags$thead(
              tags$tr(
                tags$th("Method"),
                tags$th("What it separates"),
                tags$th("Main controls")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(strong("Grayscale threshold")),
                tags$td("Pixels above or below a selected brightness level."),
                tags$td("Threshold and mask inversion.")
              ),
              tags$tr(
                tags$td(strong("K-means RGB")),
                tags$td("Color domains represented by a specified number of clusters."),
                tags$td("Number of clusters, random starts and reproducible seed.")
              ),
              tags$tr(
                tags$td(strong("Marker-controlled watershed")),
                tags$td("Adjacent objects or domains within a brightness-derived mask."),
                tags$td("Mask threshold, smoothing, marker threshold and minimum object size.")
              )
            )
          ),
          p(
            class = "small-note",
            "Watershed is experimental and may over-segment noisy or weakly separated objects."
          ),

          h4("Result tabs"),
          tags$dl(
            tags$dt("Original Image"),
            tags$dd("Source image and interactive rectangular ROI selection."),
            tags$dt("ROI preview"),
            tags$dd("The full image or active crop prepared for the next run."),
            tags$dt("Segmentation"),
            tags$dd("Input from the last run, segmented image, boundary overlay and legend."),
            tags$dt("Statistics"),
            tags$dd("Pixel counts, percentages and method-specific class or object information."),
            tags$dt("Output info"),
            tags$dd("Output folder, last caption and metadata for the last saved result.")
          ),

          h4("Export and file handling"),
          tags$ul(
            tags$li("Save PNG + CSV writes a presentation figure, statistics table and local RDS metadata to the standalone output folder."),
            tags$li("Download buttons save the PNG figure or CSV table through the browser."),
            tags$li("Exports always use the last completed segmentation, not unprocessed parameter changes."),
            tags$li("The uploaded source image is read only and is never overwritten."),
            tags$li("Fragmentator does not write to Make_Thin_Section SQLite databases.")
          ),

          h4("Image size and processing"),
          p(
            "Maximum upload size: ", strong(paste0(MAX_UPLOAD_MB, " MB")),
            ". Images larger than ", strong(paste0(MAX_WORKING_DIM, " pixels")),
            " on their longest side are proportionally reduced for interactive processing. ",
            "Original and working dimensions are shown under Status and image details."
          ),

          h4("Scientific interpretation"),
          div(
            class = "warning-box",
            strong("Important limitation. "),
            "Fragmentator is not an automatic mineral-identification or geological-interpretation system. ",
            "Its classes represent mathematical separation by brightness, color or object boundaries—not verified mineral species. ",
            "Use the overlay, legend and statistics as analytical aids and validate every geological interpretation against the original image and relevant observations."
          )
        )
      )
    )
  )
)


# ============================================================
# 7. Server
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    image_info = NULL,
    roi = NULL,
    segmentation_input = NULL,
    segmentation_result = NULL,
    segmentation_stale = FALSE,
    last_method = NULL,
    last_saved_result = NULL,
    last_fragmentation_caption = NULL,
    hierarchical_input = NULL,
    hierarchical_result = NULL,
    hierarchical_stale = FALSE,
    grainsight_result = NULL,
    grainsight_stale = FALSE,
    size_source = "hierarchical",
    status = "Ready."
  )

  current_calibration <- reactive({
    info <- rv$image_info
    if (is.null(info) || is.null(input$calibration_mode) || identical(input$calibration_mode, "none")) {
      return(list(valid = FALSE))
    }

    if (identical(input$calibration_mode, "width")) {
      width_value <- suppressWarnings(as.numeric(input$calibration_width))
      if (!is.finite(width_value) || width_value <= 0) return(list(valid = FALSE))
      width_um <- if (identical(input$calibration_unit, "mm")) width_value * 1000 else width_value
      original_um_per_px <- width_um / info$original_width
      working_um_per_px <- width_um / info$working_width
      height_um <- width_um * info$original_height / info$original_width

      return(list(
        valid = TRUE,
        source = "known image width",
        width_um = width_um,
        height_um = height_um,
        original_um_per_px = original_um_per_px,
        working_um_per_px = working_um_per_px,
        original_width_px = info$original_width,
        original_height_px = info$original_height,
        working_width_px = info$working_width,
        working_height_px = info$working_height
      ))
    }

    list(valid = FALSE)
  })

  load_image_from_path <- function(path, source_label = "manual") {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      rv$status <- paste("Image file not found:", path)
      showNotification(rv$status, type = "error")
      return(FALSE)
    }

    info <- tryCatch(
      read_image_as_array(path, max_dim = MAX_WORKING_DIM),
      error = function(e) {
        rv$status <- paste("Image loading error:", conditionMessage(e))
        showNotification(rv$status, type = "error")
        NULL
      }
    )

    if (is.null(info)) return(FALSE)

    old_info <- rv$image_info
    keep_cal <- FALSE
    if (!is.null(old_info) && isTRUE(isolate(input$inherit_calibration))) {
      keep_cal <- identical(old_info$original_width, info$original_width) &&
        identical(old_info$original_height, info$original_height)
    }

    rv$image_info <- info
    rv$roi <- NULL
    rv$segmentation_input <- NULL
    rv$segmentation_result <- NULL
    rv$segmentation_stale <- FALSE
    rv$last_method <- NULL
    rv$last_fragmentation_caption <- NULL
    rv$hierarchical_input <- NULL
    rv$hierarchical_result <- NULL
    rv$hierarchical_stale <- FALSE
    rv$grainsight_result <- NULL
    rv$grainsight_stale <- FALSE

    if (!is.null(old_info) && !isTRUE(keep_cal)) {
      updateRadioButtons(session, "calibration_mode", selected = "none")
      if (isTRUE(isolate(input$inherit_calibration))) {
        showNotification(
          "Image pixel dimensions changed. Spatial calibration was reset.",
          type = "warning"
        )
      }
    }

    rv$status <- paste0(
      "Loaded image from ", source_label, ": ",
      basename(path),
      " | original ",
      info$original_width, "x", info$original_height,
      " | working ",
      info$working_width, "x", info$working_height,
      if (isTRUE(info$was_reduced)) " | reduced for processing" else ""
    )

    showNotification("Image loaded.", type = "message")
    TRUE
  }

  profiles <- reactiveVal(list())
  profile_error <- function(e) showNotification(paste("Calibration profiles:", conditionMessage(e)), type = "error", duration = 10)
  refresh_profiles <- function(selected = isolate(input$calibration_profile)) {
    x <- safe_read_calibration_profiles()
    profiles(x)
    if (is.null(selected) || !selected %in% names(x)) selected <- if (length(x)) names(x)[1] else character(0)
    updateSelectInput(session, "calibration_profile", choices = names(x), selected = selected)
    invisible(x)
  }
  observeEvent(TRUE, { tryCatch(refresh_profiles(), error = profile_error) }, once = TRUE)

  output$calibration_profile_details <- renderText({
    name <- input$calibration_profile
    p <- if (is.null(name) || !nzchar(name)) NULL else profiles()[[name]]
    store <- paste0("Profile storage: ", CALIBRATION_PROFILE_FILE)
    if (is.null(p)) return(store)
    paste0(p$name, " | ", p$original_width_px, " × ", p$original_height_px, " original pixels",
           "\nImage width: ", p$image_width, " ", if (p$unit == "um") "µm" else "mm",
           " | ", signif(p$um_per_px, 8), " µm/px\n", store)
  })

  observeEvent(input$save_calibration_profile, {
    tryCatch({
      cal <- current_calibration()
      name <- trimws(input$calibration_profile_name)
      if (is.null(rv$image_info) || !isTRUE(cal$valid)) stop("Load an image and set a valid known image width first.")
      if (length(name) != 1L || !nzchar(name) || nchar(name) > 100) stop("Enter a profile name (1–100 characters).")
      p <- list(name = name, original_width_px = rv$image_info$original_width,
                original_height_px = rv$image_info$original_height,
                image_width = as.numeric(input$calibration_width), unit = input$calibration_unit,
                um_per_px = cal$original_um_per_px)
      update_calibration_profiles(name, p)
      refresh_profiles(name)
      showNotification(paste("Profile saved:", name), type = "message")
    }, error = profile_error)
  })

  observeEvent(input$load_calibration_profile, {
    tryCatch({
      name <- input$calibration_profile
      if (is.null(name) || !nzchar(name)) stop("Select a saved profile first.")
      x <- refresh_profiles(name)
      p <- x[[name]]
      if (is.null(p)) stop("This profile no longer exists.")
      if (is.null(rv$image_info)) stop("Load an image before applying a profile.")
      if (!profile_matches_image(p, rv$image_info)) {
        showNotification(paste0("Profile not applied: expected ", p$original_width_px, " × ", p$original_height_px,
                                " original pixels; current image is ", rv$image_info$original_width, " × ", rv$image_info$original_height,
                                ". Existing calibration was not changed."), type = "warning", duration = 12)
        return()
      }
      updateNumericInput(session, "calibration_width", value = p$image_width)
      updateSelectInput(session, "calibration_unit", selected = p$unit)
      updateRadioButtons(session, "calibration_mode", selected = "width")
      updateTextInput(session, "calibration_profile_name", value = p$name)
      showNotification(paste("Profile loaded:", p$name), type = "message")
    }, error = profile_error)
  })

  observeEvent(input$delete_calibration_profile, {
    tryCatch({
      name <- input$calibration_profile
      if (is.null(name) || !nzchar(name)) stop("Select a saved profile first.")
      update_calibration_profiles(name)
      refresh_profiles()
      showNotification(paste("Profile deleted:", name, "(current calibration retained)."), type = "message")
    }, error = profile_error)
  })

  observeEvent(input$clipboard_image, {
    tmp <- tempfile("fragmentator-clipboard-", fileext = ".png")
    tryCatch({
      clipboard_image_to_png(input$clipboard_image$data_url, tmp)
      ok <- load_image_from_path(tmp, source_label = "clipboard")
      session$sendCustomMessage("fragmentator_clipboard_status",
                               if (isTRUE(ok)) "Clipboard image loaded. Choose settings and click a Run button." else "Image loading failed. Try manual upload.")
    }, error = function(e) {
      msg <- paste("Clipboard image:", conditionMessage(e), "Try Ctrl+V or Upload image.")
      showNotification(msg, type = "error", duration = 10)
      session$sendCustomMessage("fragmentator_clipboard_status", msg)
    }, finally = unlink(tmp))
  })

  observeEvent(input$upload_image, {
    req(input$upload_image$datapath)
    load_image_from_path(input$upload_image$datapath, source_label = "manual upload")
  })

  observeEvent(input$input_brush, {
    req(rv$image_info)
    brush <- input$input_brush

    if (is.null(brush)) return()

    h <- dim(rv$image_info$arr)[1]
    w <- dim(rv$image_info$arr)[2]

    # The plot uses image-sized coordinates and a fixed 1:1 aspect ratio.
    # Convert brushed boundaries to inclusive pixel indices.
    xmin <- floor(min(brush$xmin, brush$xmax)) + 1
    xmax <- ceiling(max(brush$xmin, brush$xmax))

    # Plot y grows bottom-to-top; image row indices grow top-to-bottom.
    # Raster image uses row index top to bottom.
    ymin <- floor(h - max(brush$ymin, brush$ymax)) + 1
    ymax <- ceiling(h - min(brush$ymin, brush$ymax))

    rv$roi <- list(
      xmin = max(1, min(xmin, xmax)),
      xmax = min(w, max(xmin, xmax)),
      ymin = max(1, min(ymin, ymax)),
      ymax = min(h, max(ymin, ymax))
    )

    rv$segmentation_stale <- !is.null(rv$segmentation_result)
    rv$hierarchical_stale <- !is.null(rv$hierarchical_result)

    rv$status <- paste0(
      "ROI selected: x=", rv$roi$xmin, "-", rv$roi$xmax,
      ", y=", rv$roi$ymin, "-", rv$roi$ymax,
      ". Choose segmentation settings, then click Run segmentation."
    )
  })

  observeEvent(input$clear_roi, {
    rv$roi <- NULL
    rv$segmentation_stale <- !is.null(rv$segmentation_result)
    rv$hierarchical_stale <- !is.null(rv$hierarchical_result)
    rv$status <- "ROI cleared. The full image will be used on the next run."
  })

  working_array <- reactive({
    req(rv$image_info)

    arr <- rv$image_info$arr

    if (!is.null(rv$roi)) {
      crop_array_by_roi(arr, rv$roi)$arr
    } else {
      arr
    }
  })

  observeEvent(input$run_segmentation, {
    req(rv$image_info)

    arr <- working_array()
    method <- input$method

    result <- tryCatch({
      if (method == "threshold") {
      th <- withProgress(message = "Running threshold segmentation...", value = 0.3, {
        segment_threshold(
          arr,
          threshold = input$threshold,
          invert = input$threshold_invert
        )
      })

        list(
        type = "threshold",
        seg_arr = th$seg_arr,
        overlay_arr = th$overlay_arr,
        stats = th$stats,
        legend = th$legend,
        mask = th$mask
        )
      } else if (method == "kmeans") {

      km <- withProgress(message = "Running k-means RGB clustering...", value = 0.3, {
        segment_kmeans(
          arr,
          centers = input$k_centers,
          nstart = input$k_nstart,
          seed = input$k_seed
        )
      })

        list(
        type = "kmeans",
        seg_arr = km$seg_arr,
        overlay_arr = km$overlay_arr,
        stats = km$stats,
        legend = km$legend,
        cluster_map = km$cluster_map
        )
      } else if (method == "watershed") {

      ws <- withProgress(message = "Running marker-controlled watershed...", value = 0.3, {
        segment_marker_watershed(
          arr = arr,
          threshold = input$ws_threshold,
          invert_mask = input$ws_invert_mask,
          blur_sigma = input$ws_blur_sigma,
          seed_threshold = input$ws_seed_threshold,
          min_object_px = input$ws_min_object_px
        )
      })

        list(
        type = "watershed",
        seg_arr = ws$seg_arr,
        overlay_arr = ws$overlay_arr,
        stats = ws$stats,
        legend = ws$legend,
        object_map = ws$object_map,
        n_objects = ws$n_objects
        )
      } else if (method == "grainsight") {
        withProgress(message = "Running GrainSight / FastSAM...", value = 0.2, {
          segment_grainsight(arr, imgsz = input$gs_imgsz, conf = input$gs_conf,
                            iou = input$gs_iou, min_area = input$gs_min_area, max_det = input$gs_max_det)
        })
      } else {
        stop("Unknown segmentation method: ", method)
      }
    }, error = function(e) {
      rv$status <- paste("Segmentation error:", conditionMessage(e))
      showNotification(rv$status, type = "error")
      NULL
    })

    if (is.null(result)) {
      rv$segmentation_stale <- !is.null(rv$segmentation_result)
      if (identical(method, "grainsight")) rv$grainsight_stale <- !is.null(rv$grainsight_result)
      return()
    }

    rv$segmentation_result <- result
    if (identical(method, "grainsight")) {
      rv$grainsight_result <- result
      rv$grainsight_stale <- FALSE
      rv$size_source <- "grainsight"
      updateSelectInput(session, "size_source", selected = "grainsight")
    }
    rv$segmentation_input <- arr
    rv$last_method <- method
    rv$last_fragmentation_caption <- make_current_caption()
    rv$segmentation_stale <- FALSE
    rv$status <- paste0(
      "Segmentation completed: ", method,
      if (!is.null(rv$roi)) " | selected ROI" else " | full image",
      if (identical(method, "grainsight")) paste0(" | ", result$n_objects, " detected objects",
        if (result$n_objects == 0) "; no objects passed the selected thresholds" else "") else ""
    )
    showNotification("Segmentation completed.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$run_hierarchical, {
    req(rv$image_info)

    arr <- working_array()
    mode_used <- if (identical(input$ws_hier_speed_mode, "thorough")) "thorough" else "fast"
    t0 <- proc.time()[["elapsed"]]

    result <- tryCatch({
      withProgress(message = "Hierarchical grain detection", value = 0.02, {
        setProgress(value = 0.04, detail = "Building Level-1 preview...")

        preview <- downsample_rgb_for_preview(
          arr,
          max_dim = as.numeric(input$ws_hier_preview_dim)
        )
        preview_blur <- max(0.35, input$ws_blur_sigma * preview$scale)
        preview_min_object <- max(
          4L,
          as.integer(round(max(8, input$ws_min_object_px) * preview$scale^2))
        )

        threshold_used <- if (isTRUE(input$ws_hier_use_otsu)) {
          watershed_otsu_threshold(preview$arr, blur_sigma = preview_blur)
        } else {
          input$ws_threshold
        }

        seed_values <- if (identical(mode_used, "fast")) {
          seq(0.12, 0.80, length.out = 9)
        } else {
          seq(0.10, 0.82, length.out = 19)
        }

        setProgress(
          value = 0.10,
          detail = paste0(
            "Auto-fitting Level 1 on ",
            preview$preview_width, " x ", preview$preview_height,
            " preview..."
          )
        )

        auto <- auto_fit_watershed(
          arr = preview$arr,
          threshold = threshold_used,
          invert_mask = input$ws_invert_mask,
          blur_sigma = preview_blur,
          min_object_px = preview_min_object,
          seed_values = seed_values
        )
        auto <- select_level1_domain_scale(
          auto,
          mode = input$ws_hier_level1_coarseness
        )

        setProgress(
          value = 0.28,
          detail = paste0(
            "Coarse Level-1 domain scale selected (marker = ",
            round(auto$suggested_seed, 3),
            "; preview objects = ",
            if (!is.null(auto$selected_n_complete)) auto$selected_n_complete else "n/a",
            "). Computing full-resolution domains once..."
          )
        )

        progress_cb <- function(stage, done, total, parent_id) {
          if (identical(stage, "level1_full")) {
            if (done <= 0) {
              setProgress(
                value = 0.30,
                detail = "Level 1: computing full-resolution domains..."
              )
            } else {
              setProgress(
                value = 0.38,
                detail = "Level 1 complete. Screening parent domains for Level 2..."
              )
            }
          } else if (identical(stage, "screening")) {
            if (total <= 0) {
              setProgress(
                value = 0.88,
                detail = "No oversized parent domains require Level-2 refinement."
              )
            } else {
              setProgress(
                value = 0.40,
                detail = paste0(
                  "Level 1 complete -> Level 2: ",
                  total, " parent domain",
                  if (total == 1) "" else "s",
                  " selected for local refinement."
                )
              )
            }
          } else if (identical(stage, "level2")) {
            frac <- if (total > 0) min(1, max(0, done / total)) else 1
            pval <- 0.40 + 0.48 * frac
            setProgress(
              value = pval,
              detail = paste0(
                "Level 2 refinement: parent ",
                max(1, done + if (done < total) 1 else 0),
                " / ", max(1, total),
                if (is.finite(parent_id)) paste0(" (ID ", parent_id, ")") else ""
              )
            )
          } else if (identical(stage, "render")) {
            setProgress(
              value = if (done <= 0) 0.90 else 0.96,
              detail = if (done <= 0) {
                "Building terminal grain-candidate map..."
              } else {
                "Terminal grain-candidate map complete."
              }
            )
          }
        }

        hier <- hierarchical_watershed_two_level(
          arr = arr,
          threshold = threshold_used,
          invert_mask = input$ws_invert_mask,
          blur_sigma = input$ws_blur_sigma,
          min_object_px = max(8, input$ws_min_object_px),
          level1_seed_threshold = auto$suggested_seed,
          parent_size_factor = input$ws_hier_parent_factor,
          boundary_min = input$ws_hier_boundary_min,
          boundary_sensitivity = input$ws_hier_boundary_sensitivity,
          max_children = as.integer(input$ws_hier_max_children),
          max_parent_trials = as.integer(input$ws_hier_max_parents),
          analysis_mode = mode_used,
          progress_callback = progress_cb
        )

        hier$parent_autofit <- auto
        hier$level1_selection_mode <- input$ws_hier_level1_coarseness
        hier$boundary_sensitivity <- input$ws_hier_boundary_sensitivity
        hier$level2_method <- "boundary-first persistent RGB-gradient ridges"
        hier$threshold_used <- threshold_used
        hier$threshold_source <- if (isTRUE(input$ws_hier_use_otsu)) "Otsu on Level-1 preview" else "manual"
        hier$preview_scale <- preview$scale
        hier$preview_width <- preview$preview_width
        hier$preview_height <- preview$preview_height
        hier$elapsed_seconds <- proc.time()[["elapsed"]] - t0

        setProgress(value = 1.00, detail = "Hierarchical detection complete.")
        hier
      })
    }, error = function(e) {
      rv$status <- paste("Hierarchical watershed error:", conditionMessage(e))
      showNotification(rv$status, type = "error", duration = NULL)
      NULL
    })

    if (is.null(result)) return()

    rv$hierarchical_input <- arr
    rv$size_source <- "hierarchical"
    updateSelectInput(session, "size_source", selected = "hierarchical")
    rv$hierarchical_result <- result
    rv$hierarchical_stale <- FALSE
    rv$status <- paste0(
      "Hierarchical detection completed | Level 1: ", result$n_level1,
      " | Level-2 parents attempted: ", result$n_attempted_parents,
      " | split parents: ", result$n_split_parents,
      " | terminal: ", result$n_terminal,
      " | ", round(result$elapsed_seconds, 1), " s"
    )
    if (result$n_error_parents > 0) {
      rv$status <- paste0(rv$status, " | WARNING: ", result$n_error_parents, " Level-2 errors; inspect parent diagnostics")
      showNotification(paste(result$n_error_parents,
        "Level-2 parents failed with errors. These are not successful unsplit tests; inspect parent diagnostics."),
        type = "warning", duration = NULL)
    }
    showNotification(
      paste0(
        "Hierarchical grain detection completed in ",
        round(result$elapsed_seconds, 1), " s."
      ),
      type = "message"
    )
  }, ignoreInit = TRUE)

  segmentation_result <- reactive({
    req(rv$segmentation_result)
    rv$segmentation_result
  })

  observeEvent(list(
    input$method,
    input$gs_imgsz, input$gs_conf, input$gs_iou, input$gs_min_area, input$gs_max_det,
    input$threshold,
    input$threshold_invert,
    input$k_centers,
    input$k_nstart,
    input$k_seed,
    input$ws_threshold,
    input$ws_invert_mask,
    input$ws_blur_sigma,
    input$ws_seed_threshold,
    input$ws_min_object_px,
    input$ws_hier_use_otsu,
    input$ws_hier_speed_mode,
    input$ws_hier_preview_dim,
    input$ws_hier_level1_coarseness,
    input$ws_hier_boundary_min,
    input$ws_hier_boundary_sensitivity,
    input$ws_hier_parent_factor,
    input$ws_hier_max_parents,
    input$ws_hier_max_children
  ), {
    if (!is.null(rv$segmentation_result)) {
      rv$segmentation_stale <- TRUE
    }
    if (!is.null(rv$hierarchical_result)) {
      rv$hierarchical_stale <- TRUE
    }
  }, ignoreInit = TRUE)

  output$calibration_info <- renderText({
    if (is.null(rv$image_info)) return("Load an image to calculate calibration.")
    cal <- current_calibration()
    if (!isTRUE(cal$valid)) {
      return(paste0(
        "Original pixels: ", rv$image_info$original_width, " × ", rv$image_info$original_height,
        "\nWorking pixels: ", rv$image_info$working_width, " × ", rv$image_info$working_height,
        "\nPhysical calibration: not set"
      ))
    }

    paste0(
      "Image width: ", round(cal$width_um / 1000, 4), " mm",
      "\nImage height: ", round(cal$height_um / 1000, 4), " mm",
      "\nOriginal scale: ", round(cal$original_um_per_px, 4), " µm/px",
      "\nWorking scale: ", round(cal$working_um_per_px, 4), " µm/px"
    )
  })

  output$grainsight_ready <- renderText(grainsight_preflight())
  observeEvent(input$size_source, { rv$size_source <- input$size_source }, ignoreNULL = TRUE)
  observeEvent(list(input$gs_imgsz, input$gs_conf, input$gs_iou, input$gs_min_area, input$gs_max_det, rv$roi), {
    if (!is.null(rv$grainsight_result)) rv$grainsight_stale <- TRUE
  }, ignoreInit = TRUE)
  size_uses_grainsight <- reactive(identical(rv$size_source, "grainsight"))
  size_result_available <- reactive({
    if (size_uses_grainsight()) !is.null(rv$grainsight_result) else !is.null(rv$hierarchical_result)
  })

  hierarchical_objects <- reactive({
    if (size_uses_grainsight()) {
      if (is.null(rv$grainsight_result)) return(data.frame())
      obj <- moment_object_measurements(rv$grainsight_result$object_map, current_calibration())
      if (nrow(obj)) obj$object_origin <- "GrainSight / FastSAM detected object; no Level-2 aggregate classification"
      return(obj)
    }
    if (is.null(rv$hierarchical_result)) return(data.frame())
    cal <- current_calibration()
    obj <- moment_object_measurements(
      rv$hierarchical_result$terminal_map,
      calibration = if (isTRUE(cal$valid)) cal else NULL
    )
    meta <- rv$hierarchical_result$terminal_metadata
    if (nrow(obj) > 0 && !is.null(meta) && nrow(meta) > 0) {
      obj <- merge(
        obj, meta,
        by.x = "object_id", by.y = "terminal_id",
        all.x = TRUE, sort = FALSE
      )
      obj <- obj[order(obj$object_id), , drop = FALSE]
    }

    # Add parent-refinement provenance so grain-size reporting can distinguish
    # Level-2 children from unresolved Level-1 parents.
    pd <- rv$hierarchical_result$parent_diagnostics
    if (nrow(obj) > 0 && !is.null(pd) && nrow(pd) > 0 && "parent_id" %in% names(obj)) {
      keep <- intersect(c("parent_id", "attempted", "split_accepted", "error", "reason"), names(pd))
      pd2 <- unique(pd[, keep, drop = FALSE])
      names(pd2)[names(pd2) == "attempted"] <- "refinement_attempted"
      names(pd2)[names(pd2) == "split_accepted"] <- "refinement_split_accepted"
      names(pd2)[names(pd2) == "reason"] <- "refinement_reason"
      names(pd2)[names(pd2) == "error"] <- "refinement_error"
      obj <- merge(obj, pd2, by = "parent_id", all.x = TRUE, sort = FALSE)
      obj <- obj[order(obj$object_id), , drop = FALSE]
    }

    if (!"refinement_error" %in% names(obj)) obj$refinement_error <- FALSE
    obj$refinement_error[is.na(obj$refinement_error)] <- FALSE
    if (!"refinement_attempted" %in% names(obj)) obj$refinement_attempted <- FALSE
    if (!"refinement_split_accepted" %in% names(obj)) obj$refinement_split_accepted <- FALSE
    obj$refinement_attempted[is.na(obj$refinement_attempted)] <- FALSE
    obj$refinement_split_accepted[is.na(obj$refinement_split_accepted)] <- FALSE

    if (nrow(obj) > 0) {
      obj$object_origin <- ifelse(
        obj$level == 2L,
        "Level 2 child / refined grain candidate",
        ifelse(
          obj$refinement_attempted,
          "Level 1 unresolved parent after Level-2 test",
          "Level 1 terminal object / not refined"
        )
      )
    }
    obj$object_origin[obj$refinement_error %in% TRUE] <- "Level 1 parent / Level-2 processing error"
    obj
  })

  grain_size_summary <- reactive({
    obj <- hierarchical_objects()
    if (nrow(obj) == 0) return(data.frame())
    cal <- current_calibration()
    summarize_grain_sizes(
      obj,
      exclude_border = isTRUE(input$exclude_border_objects),
      calibrated = isTRUE(cal$valid)
    )
  })

  petro_size_report <- reactive({
    obj <- hierarchical_objects()
    if (nrow(obj) == 0 && !size_uses_grainsight()) return(list(summary = data.frame(), objects = data.frame(), text = "No hierarchical objects available."))
    cal <- current_calibration()
    if (size_uses_grainsight()) return(grainsight_size_report(obj,
      exclude_border = isTRUE(input$exclude_border_objects), calibrated = isTRUE(cal$valid),
      metadata = rv$grainsight_result$metadata))
    petrographic_size_report(
      obj,
      exclude_border = isTRUE(input$exclude_border_objects),
      calibrated = isTRUE(cal$valid)
    )
  })

  output$hierarchical_status <- renderText({
    if (is.null(rv$hierarchical_result)) {
      return("No hierarchical result yet. Choose Watershed and click Run Hierarchical Grain Detection.")
    }
    z <- rv$hierarchical_result
    paste0(
      "Status: ", if (isTRUE(rv$hierarchical_stale)) "previous result shown; ROI/settings changed — rerun hierarchical detection" else "up to date",
      "\nMask threshold: ", round(z$threshold_used, 4), " (", z$threshold_source, ")",
      "\nProcessing mode: ", if (!is.null(z$analysis_mode)) z$analysis_mode else "unknown",
      "\nLevel-1 preview: ", if (!is.null(z$preview_width)) paste0(z$preview_width, " x ", z$preview_height, " px") else "n/a",
      "\nLevel-1 hierarchy scale: ", if (!is.null(z$level1_selection_mode)) z$level1_selection_mode else "n/a",
      "\nLevel-1 marker threshold: ", round(z$level1_seed_threshold, 4),
      "\nLevel-1 domains: ", z$n_level1,
      "\nParents selected for Level 2: ", if (!is.null(z$n_candidate_parents)) z$n_candidate_parents else NA,
      "\nFallback parents added to keep hierarchy moving: ", if (!is.null(z$n_fallback_parents_added)) z$n_fallback_parents_added else 0,
      "\nParents attempted at Level 2: ", z$n_attempted_parents,
      "\nLevel-2 errors (not successful unsplit tests): ", z$n_error_parents,
      "\nParents successfully evaluated at Level 2: ", z$n_attempted_parents - z$n_error_parents,
      "\nParents split: ", z$n_split_parents,
      "\nTerminal grain candidates: ", z$n_terminal,
      "\nLevel-2 method: ", if (!is.null(z$level2_method)) z$level2_method else "boundary-first",
      "\nBoundary sensitivity: ", if (!is.null(z$boundary_sensitivity)) z$boundary_sensitivity else "balanced",
      "\nOptical boundary threshold: ", round(z$boundary_min, 3),
      "\nElapsed time: ", if (!is.null(z$elapsed_seconds)) paste0(round(z$elapsed_seconds, 1), " s") else "n/a"
    )
  })

  output$plot_hier_level1 <- renderPlot({
    validate(need(!is.null(rv$hierarchical_result), "Run Hierarchical Grain Detection first."))
    plot_rgb_array(
      rv$hierarchical_result$level1$seg_arr,
      title = paste0("Level 1: ", rv$hierarchical_result$n_level1, " objects")
    )
  })

  hierarchical_boundary_lines <- reactive({
    req(rv$hierarchical_result)
    hierarchical_split_boundaries(rv$hierarchical_result$level1$object_map,
                                 rv$hierarchical_result$terminal_map)
  })

  output$plot_hier_terminal <- renderPlot({
    validate(need(!is.null(rv$hierarchical_result), "Run Hierarchical Grain Detection first."))
    plot_rgb_array(
      rv$hierarchical_result$terminal$seg_arr,
      title = paste0("Terminal: ", rv$hierarchical_result$n_terminal, " objects")
    )
    if (isTRUE(input$show_hierarchical_boundaries)) {
      draw_hierarchical_split_boundaries(hierarchical_boundary_lines())
    }
  })

  output$plot_hier_input <- renderPlot({
    validate(need(!is.null(rv$hierarchical_result) && !is.null(rv$hierarchical_input),
                  "Run Hierarchical Grain Detection first."))
    plot_rgb_array(rv$hierarchical_input, title = "Original image / ROI used for this run")
  })

  output$hierarchical_parent_table <- renderTable({
    req(isTRUE(input$show_hierarchical_parent_table))
    validate(need(!is.null(rv$hierarchical_result), "Run Hierarchical Grain Detection first."))
    d <- rv$hierarchical_result$parent_diagnostics
    if (is.null(d) || nrow(d) == 0) return(data.frame(Message = "No parent diagnostics available."))
    d
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$grain_size_status <- renderText({
    if (!size_result_available()) return("Run the selected size source first: Hierarchical Watershed or GrainSight.")
    obj <- hierarchical_objects()
    cal <- current_calibration()
    paste0(
      "Source: ", if (size_uses_grainsight()) "GrainSight / FastSAM" else "Hierarchical Watershed",
      "\nObjects: ", nrow(obj),
      if (if (size_uses_grainsight()) isTRUE(rv$grainsight_stale) else isTRUE(rv$hierarchical_stale)) "\nPrevious result: ROI or settings changed; run again to update." else "",
      "\nMeasurement unit: ", if (isTRUE(cal$valid)) "µm" else "pixels",
      "\nBorder objects excluded from summary: ", isTRUE(input$exclude_border_objects)
    )
  })

  output$grain_size_histogram <- renderPlot({
    validate(need(size_result_available(), "Run the selected size source first."))
    obj <- hierarchical_objects()
    validate(need(nrow(obj) > 0, "No terminal objects detected."))

    if (isTRUE(input$exclude_border_objects)) {
      obj <- obj[!obj$touches_border, , drop = FALSE]
    }
    cal <- current_calibration()
    x <- if (isTRUE(cal$valid)) obj$equivalent_diameter_um else obj$equivalent_diameter_px
    x <- x[is.finite(x) & x > 0]
    validate(need(length(x) > 0, "No objects available after filtering."))

    hist(
      x, breaks = "FD",
      main = "Apparent Grain–Aggregate Size Distribution", cex.main = 1,
      xlab = if (isTRUE(cal$valid)) "ECD (µm)" else "ECD (pixels)",
      ylab = "Number of objects"
    )

    pr <- petro_size_report()
    if (size_uses_grainsight()) {
      if (is.finite(pr$dominant)) {
        abline(v = pr$dominant, lwd = 2, lty = 2)
        legend("topright", legend = "Median detected-object ECD", lty = 2, lwd = 2, bty = "n", cex = 0.8)
      }
      return(invisible(NULL))
    }
    if (!is.null(pr$dominant) && is.finite(pr$dominant)) {
      abline(v = pr$dominant, lwd = 2, lty = 2)
    } else {
      # No main-population line when all objects were excluded.
      invisible(NULL)
    }
    if (!is.null(pr$upper_tail_threshold) && is.finite(pr$upper_tail_threshold)) {
      abline(v = pr$upper_tail_threshold, lwd = 2, lty = 3)
      if (is.finite(pr$main_p90)) abline(v = pr$main_p90, lwd = 2, lty = 4, col = "#2166ac")
      legend(
        "topright",
        legend = c("Dominant apparent size (main-population median)",
                   "Initial upper-tail cutoff (P90 of all terminal objects)",
                   "P90 of probable main grain population")[c(is.finite(pr$dominant), TRUE, is.finite(pr$main_p90))],
        lty = c(2, 3, 4)[c(is.finite(pr$dominant), TRUE, is.finite(pr$main_p90))], lwd = 2,
        col = c("black", "black", "#2166ac")[c(is.finite(pr$dominant), TRUE, is.finite(pr$main_p90))],
        bty = "n", cex = 0.65
      )
    }
  })

  output$petro_size_report_text <- renderText({
    validate(need(size_result_available(), "Run the selected size source first."))
    petro_size_report()$text
  })

  output$petro_size_report_table <- renderTable({
    validate(need(size_result_available(), "Run the selected size source first."))
    petro_size_report()$summary
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$grain_size_summary_table <- renderTable({
    validate(need(size_result_available(), "Run the selected size source first."))
    grain_size_summary()
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$grain_object_table <- renderTable({
    validate(need(size_result_available(), "Run the selected size source first."))
    pr <- petro_size_report()
    obj <- pr$objects
    if (is.null(obj) || nrow(obj) == 0) return(data.frame(Message = "No terminal objects detected."))
    obj
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$plot_original_image <- renderPlot({
    req(rv$image_info)

    arr <- rv$image_info$arr

    plot_rgb_array(arr, title = "Original image — brush ROI here")

    if (!is.null(rv$roi)) {
      h <- dim(arr)[1]
      w <- dim(arr)[2]

      xleft <- rv$roi$xmin - 1
      xright <- rv$roi$xmax

      # Convert image row coordinates to plot coordinates.
      ytop <- h - (rv$roi$ymin - 1)
      ybottom <- h - rv$roi$ymax

      rect(xleft, ybottom, xright, ytop, border = "red", lwd = 3)
    }
  })

  output$plot_roi_preview <- renderPlot({
    req(rv$image_info)

    arr <- rv$image_info$arr

    if (!is.null(rv$roi)) {
      roi_arr <- crop_array_by_roi(arr, rv$roi)$arr
      plot_rgb_array(roi_arr, title = "ROI preview — image passed to segmentation")
    } else {
      plot_rgb_array(arr, title = "No ROI selected — full image will be used")
    }
  })

  output$roi_text <- renderText({
    if (is.null(rv$image_info)) {
      return("No image loaded.")
    }

    if (!is.null(rv$roi)) {
      paste0(
        "ROI active: yes\n",
        "x = ", rv$roi$xmin, " - ", rv$roi$xmax, "\n",
        "y = ", rv$roi$ymin, " - ", rv$roi$ymax, "\n",
        "Width: ", rv$roi$xmax - rv$roi$xmin + 1, " px\n",
        "Height: ", rv$roi$ymax - rv$roi$ymin + 1, " px"
      )
    } else {
      paste0(
        "ROI active: no\n",
        "The full image will be used for segmentation.\n",
        "Image size: ",
        rv$image_info$working_width,
        " x ",
        rv$image_info$working_height,
        " px"
      )
    }
  })

  output$plot_input <- renderPlot({
    req(rv$image_info)

    if (!is.null(rv$segmentation_input)) {
      plot_rgb_array(
        rv$segmentation_input,
        title = "Input used for the last segmentation run"
      )
    } else if (!is.null(rv$roi)) {
      arr <- crop_array_by_roi(rv$image_info$arr, rv$roi)$arr
      plot_rgb_array(arr, title = "Prepared input — selected ROI (not run yet)")
    } else {
      arr <- rv$image_info$arr
      plot_rgb_array(arr, title = "Prepared input — full image (not run yet)")
    }
  })

  output$plot_segmented <- renderPlot({
    validate(need(
      !is.null(rv$segmentation_result),
      "No result yet. Choose settings and click Run segmentation."
    ))
    res <- segmentation_result()

    title <- switch(
      res$type,
      threshold = "Threshold segmentation",
      kmeans = "K-means segmentation",
      watershed = "Marker-controlled watershed",
      grainsight = "GrainSight / FastSAM objects",
      "Segmentation"
    )

    plot_rgb_array(res$seg_arr, title = title)
  })

  output$plot_overlay <- renderPlot({
    validate(need(
      !is.null(rv$segmentation_result),
      "No result yet. Choose settings and click Run segmentation."
    ))
    res <- segmentation_result()
    plot_rgb_array(res$overlay_arr, title = "Boundary overlay")
  })

  output$plot_legend <- renderPlot({
    validate(need(
      !is.null(rv$segmentation_result),
      "No result yet. Choose settings and click Run segmentation."
    ))
    res <- segmentation_result()

    if (res$type == "threshold") {
      plot_threshold_legend(res$legend, title = "Threshold legend")
    } else if (res$type == "kmeans") {
      plot_segmentation_legend(res$legend, title = "K-means cluster legend")
    } else if (res$type %in% c("watershed", "grainsight")) {
      plot_watershed_legend(res$legend, title = "Watershed object legend")
    }
  })

  output$stats_table <- renderTable({
    validate(need(
      !is.null(rv$segmentation_result),
      "No statistics yet. Run segmentation first."
    ))
    segmentation_result()$stats
  })

  output$status_text <- renderText({
    paste(
      rv$status,
      "\n\nCurrent method:", input$method,
      "\nROI active:", !is.null(rv$roi),
      "\nHierarchical result:", if (is.null(rv$hierarchical_result)) {
        "not run yet"
      } else if (isTRUE(rv$hierarchical_stale)) {
        "previous result shown; settings or ROI changed — rerun"
      } else {
        "up to date"
      },
      "\nSegmentation result:", if (is.null(rv$segmentation_result)) {
        "not run yet"
      } else if (isTRUE(rv$segmentation_stale)) {
        "previous result shown; settings or ROI changed — click Run segmentation"
      } else {
        "up to date"
      },
      if (!is.null(rv$image_info)) {
        paste0(
          "\nImage: ", basename(rv$image_info$path),
          "\nOriginal size: ", rv$image_info$original_width, " x ", rv$image_info$original_height,
          "\nWorking size: ", rv$image_info$working_width, " x ", rv$image_info$working_height,
          "\nRGB range: ", round(min(rv$image_info$arr, na.rm = TRUE), 4), " - ",
          round(max(rv$image_info$arr, na.rm = TRUE), 4),
          "\nArray dim: ", paste(dim(rv$image_info$arr), collapse = " x ")
        )
      } else {
        "\nNo image loaded."
      }
    )
  })

  output$output_paths <- renderText({
    paste(
      "APP_DIR:", APP_DIR,
      "FRAGMENTATOR_OUTPUT_DIR:", FRAGMENTATOR_OUTPUT_DIR,
      sep = "\n"
    )
  })

  output$last_fragmentation_caption_text <- renderText({
    cap <- rv$last_fragmentation_caption

    if (is.null(cap) || !nzchar(cap)) {
      "No fragmentation caption yet. Run a segmentation or save/export a result first."
    } else {
      cap
    }
  })

  output$last_saved_result_text <- renderPrint({
    rv$last_saved_result
  })

  make_current_caption <- reactive({
    req(input$method)
    if (identical(input$method, "grainsight")) return(paste0(
      "GrainSight / FastSAM-x; input ", input$gs_imgsz, "; confidence ", input$gs_conf,
      "; detection IoU ", input$gs_iou, "; min area ", input$gs_min_area, " px²; max ", input$gs_max_det,
      if (!is.null(rv$roi)) "; ROI." else "; full image."))

    build_segmentation_caption(
      req = NULL,
      method = input$method,
      use_roi = !is.null(rv$roi),
      k_centers = if (input$method == "kmeans") input$k_centers else NA,
      threshold = if (input$method == "threshold") input$threshold else if (input$method == "watershed") input$ws_threshold else NA,
      nstart = if (input$method == "kmeans") input$k_nstart else NA,
      seed = if (input$method == "kmeans") input$k_seed else NA,
      ws_seed_threshold = if (input$method == "watershed") input$ws_seed_threshold else NA,
      ws_min_object_px = if (input$method == "watershed") input$ws_min_object_px else NA
    )
  })

  make_output_filename <- function(ext = "png") {
    base <- if (!is.null(rv$image_info) && !is.null(rv$image_info$path)) {
      safe_file_stub(tools::file_path_sans_ext(basename(rv$image_info$path)))
    } else {
      "image"
    }

    paste0(
      "fragmentator_",
      base, "_",
      if (!is.null(rv$last_method)) rv$last_method else input$method, "_",
      timestamp_stub(),
      ".",
      ext
    )
  }


  observeEvent(input$save_to_output_folder, {
    req(segmentation_result())

    res <- segmentation_result()
    caption <- rv$last_fragmentation_caption

    output_png <- file.path(FRAGMENTATOR_OUTPUT_DIR, make_output_filename("png"))
    output_csv <- sub("\\.png$", ".csv", output_png, ignore.case = TRUE)

    tryCatch({
      withProgress(message = "Saving segmentation PNG and CSV...", value = 0.2, {
        export_segmentation_with_legend_png(res, output_png, caption = caption)
        incProgress(0.4)

        write.csv(res$stats, output_csv, row.names = FALSE)
        incProgress(0.3)

        standalone_result <- write_standalone_result_metadata(
          output_png = output_png,
          output_csv = output_csv,
          caption = caption,
          method = rv$last_method
        )

        rv$last_saved_result <- standalone_result
      })

      rv$status <- paste("Standalone result saved:", output_png)
      showNotification("PNG + CSV saved to standalone output folder.", type = "message")
    }, error = function(e) {
      rv$status <- paste("Save error:", conditionMessage(e))
      showNotification(rv$status, type = "error")
    })
  })


  output$download_png <- downloadHandler(
    filename = function() {
      make_output_filename("png")
    },
    content = function(file) {
      req(segmentation_result())
      cap <- rv$last_fragmentation_caption

      export_segmentation_with_legend_png(
        segmentation_result(),
        filepath = file,
        caption = cap
      )
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      make_output_filename("csv")
    },
    content = function(file) {
      req(segmentation_result())

      stats <- segmentation_result()$stats
      write.csv(stats, file, row.names = FALSE)
    }
  )

  output$download_size_report_txt <- downloadHandler(
    filename = function() paste0("fragmentator_", rv$size_source, "_size_report.txt"),
    content = function(file) {
      req(size_result_available())
      writeLines(petro_size_report()$text, file, useBytes = TRUE)
    }
  )

  output$download_hier_objects_csv <- downloadHandler(
    filename = function() {
      paste0("fragmentator_", rv$size_source, "_objects_", timestamp_stub(), ".csv")
    },
    content = function(file) {
      req(size_result_available())
      write.csv(hierarchical_objects(), file, row.names = FALSE)
    }
  )

  output$download_hier_summary_csv <- downloadHandler(
    filename = function() {
      paste0("fragmentator_grain_size_summary_", timestamp_stub(), ".csv")
    },
    content = function(file) {
      req(size_result_available())
      write.csv(grain_size_summary(), file, row.names = FALSE)
    }
  )

  output$download_petro_size_report_csv <- downloadHandler(
    filename = function() {
      paste0("fragmentator_apparent_grain_aggregate_size_report_", timestamp_stub(), ".csv")
    },
    content = function(file) {
      req(size_result_available())
      write.csv(petro_size_report()$summary, file, row.names = FALSE)
    }
  )
}


# ============================================================
# 8. Run app
# ============================================================

shinyApp(ui = ui, server = server)
