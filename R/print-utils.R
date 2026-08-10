.header <- function(x){
  hline <- strrep("-", nchar(x))
  cat(hline, "\n", sep = "")
  cat(x, "\n", sep = "")
  cat(hline, "\n", sep = "")
}

.space <- function(n = 1, cat = TRUE){
  sp <- rep("\n", n)
  if(cat){
    cat(sp)
  } else{
    sp
  }
}

.main <- function(..., space = 1){
  cat(..., .space(space, cat = FALSE))
}