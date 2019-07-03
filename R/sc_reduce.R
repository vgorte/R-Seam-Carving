#' Seam Carving <Reduce>
#'
#' Removes least energy vertical and/or horizontal pixel seams to reduce the size of a given image with respect to its contents.
#' @param imgPath Path to a RGB image. (.PNG / .JPG / .TIF)
#' @param ncols Number by which the width has to be decreased
#' @param nrows Number by which the height has to be decreased
#' @return SC-Image <EnergyMap & Resized Image>
#' @export
#' @examples
#' library(EBImage)
#' filePath <- system.file("images", "sample-color.png", package="EBImage")
#' reducedImage <- sc_reduce(filePath, ncols = 1, nrows = 1)
#' display(reducedImage$energy_map)
#' display(reducedImage$reduced_img)
#' class(reducedImage)
sc_reduce <- function(imgPath, ncols = 0, nrows = 0) {
  img <- parseImage(imgPath)
  checkUserInput(img, ncols, nrows)
  eMap <- calc_energy(img)
  img_reduced_r <- remove_rows(img, nrows)
  img_reduced <- remove_columns(img_reduced_r, ncols)
  result <- structure( list(energy_map = eMap, reduced_img = img_reduced), class="SC-Image")
  return(result)
}
