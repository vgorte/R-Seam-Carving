#' checkUserInput
#'
#' Function for validating the user input that throws errors on incorrect values.
#' @param img EBImage Image object
#' @param ncols Number by which the width is reduced
#' @param nrows Number by which the height is reduced
checkUserInput <- function(img, ncols, nrows) {
  tryCatch(
    expr = {
      img_cols <- as.numeric(dim(img)[1])
      img_rows <- as.numeric(dim(img)[2])

      if (img_cols < 2 ||
          img_rows < 2)
        stop("Image is 2x2 or smaller.")
    },
    error = function(e) {
      stop("Dimensions of image could not be identified.")
    }
  )

  tryCatch(
    expr = {
      channels <- as.numeric(dim(img)[3])

      if (channels != 3)
        stop("Number of channels in image needs to be 3. RGB images only.")
    },
    error = function(e) {
      stop("Number of channels could not be identified. Three channel RGB image required.")
    }
  )

  tryCatch(
    expr = {
      na_in_image <-any(apply(img, c(2, 3), function(x)sum(is.na(x))) != 0)

      if (na_in_image)
        stop("Image contains NA.")
    },
    error = function(e) {
      stop("Number of NA's in image could not be checked.")
    }
  )

  if (!is.numeric(ncols) || !is.numeric(nrows)) {
    stop("Arguments for number of columns and rows to be removed have to be integers.")
  } else if (ncols < 0 || nrows < 0) {
    stop("Arguments for number of columns and rows to be removed have to be positive integers.")
  } else if (ncols == 0 && nrows == 0) {
    stop("At least one argument of ncols or nrows has to be provided.")
  } else if (ncols >= img_cols || nrows >= img_rows) {
    stop("Arguments for number of columns and rows have to be smaller than dimensions of image.")
  }
}
