data.set.hash <- function(data){
  #' Convert a data frame to a hash value.
  #' 
  #' @export
  #' @importFrom openssl sha256
  str <- paste(data, collapse="\n", sep="-")
  sha256(str)
}
