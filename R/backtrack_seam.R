#' backtrack_seam
#'
#' Function returning the column idices of a seam, given its start location in the backtrack map
#' @param img EBImage Image class object
#' @param backtrack Backtrack map used for finding seams based on a start point
#' @param index Start point for backtrack map
#' @return Array of seam pixel locations
backtrack_seam <- function(img, backtrack, index){
  nrow <- as.numeric(dim(img)[2])

  seam <- array(0, dim=nrow)
  counter = nrow
  for (r in rev(c(1:nrow))) {
    seam[counter]= index
    counter = counter - 1
    dims <- dim(backtrack)
    index = backtrack[index, r]
  }
  return(seam)
}
