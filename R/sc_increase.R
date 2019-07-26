#' sc_increase
#'
#' Increases the dimensions of an image by duplicating a given number of least cost seams
#' @param imgPath Path to a RGB image. (.PNG / .JPG / .TIF)
#' @param ncols Number by which the width has to be increased
#' @param nrows Number by which the height has to be increased
#' @return SC-Image <Resized Image>
#' @export
#' @examples
#' library(EBImage)
#' filePath <- system.file("images", "sample-color.png", package="EBImage")
#' increased_image <- sc_increase(filePath, 10, 10)
#' display(increased_image$increased_img)
#' class(increased_image)
sc_increase <- function(imgPath, ncols = 0, nrows = 0) {
  img <- parseImage(imgPath)
  checkUserInput(img, ncols, nrows)

  result1 <- img
  counter <- 0
  if(ncols > 0){
    costMap <- min_cost_map(result1)
    k <- lowest_k_indices(costMap$M, ncols)
    k_sorted <- sort(k, decreasing = TRUE)#Sorting in decreasing order so that indices dont change when duplicating seams
    for(i in k_sorted){
      seam <- backtrack_seam(img, costMap$backtrack, i)
      result1 <- duplicate_seam_in_img(result1, seam)
    }
  }
  result2 <- result1
  if(nrows > 0){
    #ROTATION
    result1 <- Image(aperm(result1, c(2, 1, 3)), colormode = 2)
    result2 <- Image(aperm(result2, c(2, 1, 3)), colormode = 2)
    costMap <- min_cost_map(result1)
    k <- lowest_k_indices(costMap$M, nrows, TRUE)
    k_sorted <- sort(k, decreasing = TRUE) #Sorting in decreasing order so that indices dont change when duplicating seams
    for(i in k_sorted){
      seam <- backtrack_seam(result1, costMap$backtrack, i)
      result2 <- duplicate_seam_in_img(result2, seam)
    }
    #ROTATE TO ORIGINAL
    result2 <- Image(aperm(result2, c(2, 1, 3)), colormode = 2)
  }
  r <- structure(list(increased_img = result2), class="SC-Image")
  return(r)
}
