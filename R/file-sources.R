# Keep familiar basenames unless they would merge different input articles.
file_sources <- function(files, dir = NULL) {
  sources <- basename(files)
  repeated <- duplicated(sources) | duplicated(sources, fromLast = TRUE)

  if (any(repeated)) {
    paths <- normalizePath(files[repeated], winslash = "/", mustWork = TRUE)
    if (!is.null(dir)) {
      root <- normalizePath(dir, winslash = "/", mustWork = TRUE)
      prefix <- paste0(sub("/+$", "", root), "/")
      paths <- substring(paths, nchar(prefix) + 1L)
    }
    sources[repeated] <- paths
  }

  sources
}
