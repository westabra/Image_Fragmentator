# Local, CPU-only runtime. No administrator privileges or system Python needed.
options(timeout = 1800)
dir.create(file.path(root, 'logs'), showWarnings = FALSE)
cache <- file.path(root, 'runtime', 'downloads')
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
download_checked <- function(url, filename, sha256) {
  target <- file.path(cache, filename)
  valid <- function() file.exists(target) && identical(digest::digest(file = target, algo = 'sha256'), sha256)
  if (!valid()) {
    part <- paste0(target, '.part')
    on.exit(unlink(part), add = TRUE)
    download.file(url, part, mode = 'wb', method = 'libcurl')
    if (!identical(digest::digest(file = part, algo = 'sha256'), sha256)) stop('Checksum mismatch: ', filename)
    if (!file.copy(part, target, overwrite = TRUE)) stop('Cannot save ', target)
  }
  target
}
pyroot <- file.path(root, 'runtime', 'python')
python <- file.path(pyroot, 'python.exe')
if (!file.exists(python)) {
  archive <- download_checked('https://www.python.org/ftp/python/3.12.10/python-3.12.10-embed-amd64.zip',
    'python.zip', '4acbed6dd1c744b0376e3b1cf57ce906f9dc9e95e68824584c8099a63025a3c3')
  dir.create(pyroot, recursive = TRUE, showWarnings = FALSE)
  unzip(archive, exdir = pyroot)
}
site <- file.path(pyroot, 'Lib', 'site-packages')
dir.create(site, recursive = TRUE, showWarnings = FALSE)
writeLines(c('python312.zip', '.', 'Lib\\site-packages', '..\\..', 'import site'), file.path(pyroot, 'python312._pth'))
pip <- download_checked('https://files.pythonhosted.org/packages/c9/bc/b7db44f5f39f9d0494071bddae6880eb645970366d0a200022a1a93d57f5/pip-25.0.1-py3-none-any.whl',
  'pip.whl', 'c46efd13b6aa8279f33f2864459c8ce587ea6a1a59ee20de055868d8f7688f7f')
unzip(pip, exdir = site)
run_python <- function(args) {
  result <- processx::run(python, args, echo = TRUE, error_on_status = FALSE, windows_hide_window = TRUE)
  if (result$status != 0) stop('Python setup failed. Correct the error above and run 3_Install_AI.cmd again.')
}
run_python(c('-m', 'pip', 'install', '--no-cache-dir', '--only-binary=:all:',
  '-c', file.path(root, 'requirements-runtime.txt'),
  '--index-url', 'https://download.pytorch.org/whl/cpu', 'torch==2.2.2+cpu', 'torchvision==0.17.2+cpu'))
run_python(c('-m', 'pip', 'install', '--no-cache-dir', '--only-binary=:all:',
  '-r', file.path(root, 'requirements-runtime.txt')))
run_python(file.path(root, 'patch_runtime.py'))
weights <- file.path(root, 'weights', 'FastSAM-x.pt')
expected <- '752cadc2828edb1cd4bc4f9eb587100631af06ea2108f4c9ed56df4755701e76'
if (!file.exists(weights) || digest::digest(file = weights, algo = 'sha256') != expected) {
  model <- download_checked('https://github.com/ultralytics/assets/releases/download/v8.3.0/FastSAM-x.pt', 'FastSAM-x.pt', expected)
  dir.create(dirname(weights), showWarnings = FALSE)
  if (!file.copy(model, weights, overwrite = TRUE)) stop('Cannot save model')
}
run_python(file.path(root, 'portable_tools', 'check_runtime.py'))
installed <- processx::run(python, c('-m', 'pip', 'freeze'), error_on_status = TRUE)$stdout
writeLines(installed, file.path(root, 'logs', 'installed-python-packages.txt'))
# Remove only the known setup downloads, never user images or outputs.
unlink(file.path(cache, c('python.zip', 'pip.whl', 'FastSAM-x.pt')))
message('AI setup completed and CPU inference verified. Start with 2_Start.cmd.')
