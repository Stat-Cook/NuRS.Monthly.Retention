data.set.hash <- function(data){
  #' @export
  #' @importFrom openssl sha256
  str <- paste(data, collapse="\n", sep="-")
  sha256(str)
}
