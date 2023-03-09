data.set.hash <- function(data){
  #' Convert a data frame to a hash value.
  #' @param data Data frame to be hashed
  #' @export
  #' @importFrom openssl sha256
  str <- paste(data, collapse="\n", sep="-")
  sha256(str)
}
