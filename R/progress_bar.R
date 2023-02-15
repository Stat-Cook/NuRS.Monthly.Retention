progress_bar_init <- function(max, min=1){
  #' @importFrom progress progress_bar
  pb <- progress_bar$new(
    format = "Progress [:bar] :percent",
    total = max - min + 1, clear=F, show_after=0
  )
  pb$tick(0)
  
  pb
}

