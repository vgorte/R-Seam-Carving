#' Seam Carving <Mark Least Cost Seam>
#'
#' Marks the least cost seam in vertical direction.
#' @param imgPath Path to a three channel image. (.PNG / .JPG / .TIF)
#' @return SC-SImage <EnergyMap & Image with marked seam>
#' @export
#' @examples
#' library(EBImage)
#' filePath <- system.file("images", "sample-color.png", package="EBImage")
#' seam_image <- sc_mark_leastCSeam(filePath)
#' display(seam_image$energy_map)
#' display(seam_image$seam_image)
#' class(seam_image)
sc_mark_leastCSeam <- function(imgPath) {
  img <- parseImage(imgPath)
  eMap <- calc_energy(img)
  seamImage <- mark_Seam(img, mark = TRUE)
  result <- structure( list(energy_map = eMap, seam_image = seamImage), class="SC-SImage")
  return(result)
}
