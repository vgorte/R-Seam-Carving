#' lowest_k_indices
#'
#' Function returning the indices of the lowest k entries in the bottom row of the cost map.
#' @param vec vector of last  entries of the cost map
#' @param k number of entries to return
#' @param rev boolean, true if image has been rotated
#' @return vector with k lowest entries
lowest_k_indices <- function(vec, k, rev=FALSE) {
  ncol <- as.numeric(dim(vec)[1])
  nrow <- as.numeric(dim(vec)[2])
  a <- dim(vec)
  if(!rev){
    row <- vec[nrow, ]
  }else{
    row <- vec[,nrow]
  }
  min_indices_array <- vector()

  for(i in 1:k){
    min <- which.min(row)
    row[min] = Inf
    min_indices_array = c(min_indices_array, min)
  }

  return(min_indices_array)
}
