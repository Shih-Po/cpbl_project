.e <- list()
f <- function() {
  .e$a <- 1
  browser()
}
f()
.e$a
