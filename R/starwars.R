#' Star Wars
#'
#' Flushing Star Wars episodes in R console
#'
#' @param episode Episode to show. Currently, "sw1" and "sw2" are available.
#' @param fps Approximate frame per second.
#' @examples
#' #starwars("sw1", 20)
#' @export
starwars <- function(episode = "sw1", fps = 20) {

  os <- .Platform$OS.type
  if (os == "windows") { 
    stop("Sorry, Windows is not supported.")
  }

  height <- 13
  pkg_dir <- system.file(package = "rstarwars")
  epi <- paste0(pkg_dir, "/resources/", episode, ".txt")
  lines <- readLines(epi)

  # check terminal or console
  tmp <- paste0(tempfile("tmp"), ".txt")
  tmp2 <- paste("if [ -t 0 ]; then echo 'terminal' >",
        tmp,
        "; fi")
  system(tmp2)
  f <- strsplit(tmp, "/")[[1]]
  f2 <- f[length(f)]

  for (i in seq_len(length(lines))) {
    # terminal
    if (f2 %in% list.files(tempdir())) {
      if ((i - 1) %% (height + 1) == 0) {
        duration <- as.numeric(lines[i])
        Sys.sleep(duration / fps)
        system("clear")
      } else {
        message(lines[i], appendLF = TRUE)
      }
      flush.console()
    } else { # console
      if ((i - 1) %% (height + 1) == 0) {
        duration <- as.numeric(lines[i])
        Sys.sleep(duration / fps)
        cat("\f")
      } else {
        message(lines[i], appendLF = TRUE)
      }
      flush.console()
    }
  }
}
