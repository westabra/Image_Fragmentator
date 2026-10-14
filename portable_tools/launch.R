root <- normalizePath(Sys.getenv('FRAGMENTATOR_PORTABLE_ROOT'), winslash = '/', mustWork = TRUE)
version <- paste(R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][1], sep = '.')
.libPaths(c(file.path(root, 'R-library', version), .libPaths()))
for (folder in c('data', 'logs')) dir.create(file.path(root, folder), showWarnings = FALSE)
options(fragmentator.app_dir = file.path(root, 'app'),
        fragmentator.grainsight_root = root,
        fragmentator.grainsight_python = file.path(root, 'runtime', 'python', 'python.exe'),
        fragmentator.calibration_profile_file = file.path(root, 'data', 'calibration_profiles.rds'))
port <- Sys.getenv('MTS_FRAGMENTATOR_PORT', '')
port <- if (nzchar(port)) as.integer(port) else NULL
if (!is.null(port) && (is.na(port) || port < 1024 || port > 65535)) stop('Invalid port')
browser <- toupper(Sys.getenv('MTS_FRAGMENTATOR_BROWSER', 'TRUE')) == 'TRUE'
shiny::runApp(file.path(root, 'app', 'image_fragmentator_V7_7_0_grainsight.R'),
              host = '127.0.0.1', port = port, launch.browser = browser)
