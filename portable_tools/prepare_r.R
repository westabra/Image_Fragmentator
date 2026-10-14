invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- Sys.getenv("FRAGMENTATOR_PORTABLE_ROOT")
stopifnot(nzchar(root), dir.exists(root))
if (.Machine$sizeof.pointer != 8L || getRversion() < "4.4.0") stop("R 4.4 or newer, 64-bit, is required. Tested with R 4.5.3.")
version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
lib <- file.path(root, "R-library", version)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .Library))
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)
needed <- c("shiny", "magick", "png", "base64enc", "BiocManager", "processx", "jsonlite", "digest", "EBImage")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
args <- commandArgs(TRUE)
if (length(missing) && args[1] %in% c("install", "setup")) {
  cran <- setdiff(missing, "EBImage")
  if (length(cran)) install.packages(cran, lib = lib, type = "binary")
  if (!requireNamespace("EBImage", quietly = TRUE)) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) stop("BiocManager installation failed.")
    BiocManager::install("EBImage", lib = lib, ask = FALSE, update = FALSE, type = "binary")
  }
}
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "), ". Run dependency setup with Internet access.")
cat("R dependencies ready. Local library:", lib, "\n")
