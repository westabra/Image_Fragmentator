args <- commandArgs(trailingOnly = TRUE)
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE)[1])
root <- normalizePath(dirname(script), winslash = '/', mustWork = TRUE)
Sys.setenv(FRAGMENTATOR_PORTABLE_ROOT = root, PYTHONNOUSERSITE = '1', PYTHONDONTWRITEBYTECODE = '1')
setwd(root)
action <- if (length(args)) args[1] else 'run'
if (!action %in% c('run', 'setup', 'ai', 'check')) stop('Unknown action')
if (action == 'setup') {
  source(file.path(root, 'portable_tools', 'prepare_r.R'))
} else if (action == 'ai') {
  source(file.path(root, 'portable_tools', 'prepare_r.R'))
  source(file.path(root, 'portable_tools', 'install_ai.R'))
} else if (action == 'check') {
  source(file.path(root, 'portable_tools', 'prepare_r.R'))
  python <- file.path(root, 'runtime', 'python', 'python.exe')
  if (file.exists(python)) {
    status <- system2(python, shQuote(file.path(root, 'portable_tools', 'check_runtime.py')))
    if (status != 0) stop('AI runtime validation failed')
  } else message('Base installation OK. Optional AI runtime is not installed.')
} else {
  source(file.path(root, 'portable_tools', 'prepare_r.R'))
  source(file.path(root, 'portable_tools', 'launch.R'))
}
