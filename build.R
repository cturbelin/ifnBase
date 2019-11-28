# From https://stackoverflow.com/questions/54634056/how-to-include-an-html-vignette-in-a-binary-r-package
library(cli)

build_vignettes_to_inst <- function() {
  targets = c('doc','Meta')

  lapply(targets, function(target) {
    to = paste0("inst/", target)
    unlink(to, recursive = TRUE) # Remove the directories if they exist
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    ok = file.copy(list.files(target, full.names = TRUE), to = to)
    if(all(ok)) {
      cat_bullet("Files intalled into ", to, bullet="tick", col = "green")
    } else {
      cat_bullet("Problem during copy into ", to, bullet="cross", col = "red")
    }
  })
  invisible()
}

devtools::build_vignettes()
build_vignettes_to_inst()
devtools::build(binary=TRUE)
devtools::build()
