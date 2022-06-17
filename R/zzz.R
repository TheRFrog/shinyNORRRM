.onAttach <- function(libname, pkgname) {
  lines = c("--",
            sprintf("Remember to cite, run citation(package = '%s') for further info.",pkgname),
            "--",
            "Welcome to shinyNORRRM, have Fun!",
            "--")
  msg = paste(lines,collapse="\n")
  packageStartupMessage(msg)
  addResourcePath("logos", system.file("logos", package = "shinyNORRRM"))
  }
