#' mark_seam
#'
#' Function for replacing the values of the lowest cost seam in the image with NA's.
#' Image is reshaped to new dimensions after.
#' @param img Image
#' @param mark Boolean, if TRUE the seam is not replaced with NA's but with ones.
#' @return Image <EBImage class>
mark_Seam <- function(img, mark = FALSE) {
  ncol <- as.numeric(dim(img)[1])
  nrow <- as.numeric(dim(img)[2])

  imgEMap <- min_cost_map(img)
  M <- imgEMap$M
  backtrack <- imgEMap$backtrack

  img_array <- as.array(img)

  c <- which.min(M[, nrow])

  for (r in rev(c(1:nrow))) {
    if(mark){
      img_array[c, r, 1] = 1
      c = backtrack[c, r]
    }else{
      img_array[c, r, ] = NA
      c = backtrack[c, r]
    }
  }

  test <- na.omit(img)
  return(Image(img_array, colormode = 2))
}
