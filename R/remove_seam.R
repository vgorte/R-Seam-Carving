#' remove_seam
#'
#' Function for omitting all NA's in the image, which represent the seam.
#' Image is reshaped to new dimensions after.
#' @param img EBImage Image
#' @return Image <EBImage class>
remove_seam <- function(img) {
  img_reduced <- aperm(apply(img, c(2, 3), na.omit), c(1, 2, 3))
  return(Image(img_reduced, colormode = 2))
}

#' remove_columns
#'
#' Function for removing n lowest cost seams in image. (Vertical)
#' @param img EBImage Image class object
#' @param n Number by which the width has to be
#' @return Image <EBImage class>
remove_columns <- function(img, n) {
  if(n != 0){
    for (i in 1:n) {
      if(i != 0){
        img = mark_Seam(img)
        img = remove_seam(img)
      }
    }
  }
  return(img)
}

#' remove_rows
#'
#' Function for removing n lowest cost seams in image (Horizontal)
#' @param img EBImage Image class object
#' @param n Number by which the height has to be decreased
#' @return Image <EBImage class>

remove_rows <- function(img, n) {
  #rotation
  if(n != 0){
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
    for (i in 1:n) {
      if(i != 0){
        img = mark_Seam(img)
        img = remove_seam(img)
      }
    }
  #rotate back to original orientation
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  }
  return(img)
}
