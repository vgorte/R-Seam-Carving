#' calc_energy
#'
#' Function for calculating the energy map of a given EBImage Image.
#' Energy is calculated by using horizontal and vertical sobel kernals.
#' @param img EBImage Image class object
#' @return Array
calc_energy <- function(img) {
  # horizontal and vertical Sobel kernel
  kernal_h <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3)
  kernal_v <- t(kernal_h)

  # horizontal and vertical edges
  imgH <- filter2(img, kernal_h, boundary = "replicate")

  imgV <- filter2(img, kernal_v, boundary = "replicate")

  # combine edge pixel data to get overall edge data
  hdata <- imageData(imgH)
  vdata <- imageData(imgV)
  edata <- sqrt(hdata ^ 2 + vdata ^ 2)

  # transform edge data to image
  imgE <- Image(edata, colormode = 2)

  #display(combine(img, imgH, imgV, imgE), method = "raster", all = T)

  imgE_summed <- (imgE@.Data[, , 1] + imgE@.Data[, , 2] + imgE@.Data[, , 3]) / 3
  return(imgE_summed)
}
