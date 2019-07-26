#' duplicate_seam_in_img
#'
#' Function for duplicating a given seam in an image, causing the image to increase in size
#' @param image Image
#' @param seam array of pixel coordinates
#' @return Image with one duplicated pixel per row
duplicate_seam_in_img <- function(image, seam){
  img <- imageData(image)
  ncol <- as.numeric(dim(image)[1])
  nrow <- as.numeric(dim(image)[2])
  result <- array(0, dim= c(ncol+1, nrow, 3)) # init

  for(c in 1:3){ #for every channel
    for(r in 1:nrow){ #for every row
      a <- img[seam[r], r, c]
      result[, r, c] = insert(img[, r, c], seam[r], img[seam[r], r, c])
    }
  }
  return(Image(result, colormode = 2))
}
