#' parseImage
#'
#' Function for reading an image from a given directory path
#' @param imgPath Path to the image
#' @return Image <EBImage class>

parseImage <- function(imgPath) {
  tryCatch(
    expr = {
      img <- readImage(imgPath)
      return(img)
    },
    error = function(e) {
      stop(e)
    }
  )
}
