#' Seam Carving: A package for Content Aware Image Resizing
#'
#' For now, the Seam Carving package provides a function for reducing image dimensions by removing seams.
#'
#' @section Functions:
#' sc_remove(...)
#'
#' @docType package
#' @name SeamCarving
NULL


#' calc_energy
#'
#' Function for calculating the energy map.
#' Energy is calculated by using horizontal and vertical sobel kernals.
#' @param img EBImage Image class object
#' @return EBImage Image
calc_energy <- function(img) {
  #print(img, short = T)

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

  display(combine(img, imgH, imgV, imgE), method = "raster", all = T)

  imgE_summed <- (imgE@.Data[, , 1] + imgE@.Data[, , 2] + imgE@.Data[, , 3]) / 3
  return(imgE_summed)
}

#' minimum_seam
#'
#' Function for calculating the minimal cost map and a backtrack map for finding seams.
#' @param img EBImage Image class object
#' @param ncol Number by which the width has to be decreased
#' @param nrow Number by which the height has to be decreased
#' @return list(EnergyMap , BacktrackMap)
minimum_seam <- function(img, ncol, nrow) {
  energy_map <- calc_energy(img)
  M <- energy_map #Map of pixel costs
  backtrack <- energy_map * 0L # used for finding min energy seam

  for (c in c(1:ncol)) {
    for (r in c(2:nrow)) {
      if (c == 1) {
        idx = which.min(c(M[c, r - 1], M[c + 1, r - 1]))
        backtrack[c, r] = idx + c - 1
        min_energy = M[idx + c - 1, r - 1]
      } else if (c == ncol) {
        idx = which.min(c(M[c - 1, r - 1], M[c, r - 1]))
        backtrack[c, r] = c + idx - 2
        min_energy = M[c + idx - 2, r - 1]
      } else{
        idx = which.min(c(M[c - 1, r - 1], M[c, r - 1], M[c + 1, r - 1]))
        backtrack[c, r] = c + idx - 2
        min_energy = M[c + idx - 2, r - 1]
      }
    }
  }

  M[c, r] = M[c, r] + min_energy
  result <- list(M = M, backtrack = backtrack)
  return(result)
}

#' mark_seam
#'
#' Function for replacing the values of the lowest cost seam in the image with NA's.
#' Image is reshaped to new dimensions after.
#' @param img EBImage Image class object
#' @return Image <EBImage class>
mark_Seam <- function(img) {
  ncol <- as.numeric(dim(img)[1])
  nrow <- as.numeric(dim(img)[2])

  imgEMap <- minimum_seam(img, ncol, nrow)
  M <- imgEMap$M
  backtrack <- imgEMap$backtrack

  img_array <- as.array(img)

  c <- which.min(M[, nrow])

  for (r in rev(c(1:nrow))) {
    img_array[c, r, ] = NA
    c = backtrack[c, r]
  }

  test <- na.omit(img)
  return(Image(img_array, colormode = 2))
}

#' remove_seam
#'
#' Function for omitting all NA's in the image, which represent the seam.
#' Image is reshaped to new dimensions after.
#' @param img EBImage Image class object
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
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  if(n != 0){
    for (i in 1:n) {
      if(i != 0){
        img = mark_Seam(img)
        img = remove_seam(img)
      }
    }
  }
  #rotate back to original orientation
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  return(img)
}

#' parseImage
#'
#' Function for reading an image from a given directory path
#' @param imgPath EBImage Image class object
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

#' checkUserInput
#'
#' Function for validating the user input that throws errors on incorrect values.
#' @param img EBImage Image class object
#' @param ncols Number by which the width has to be decreased
#' @param nrows Number by which the height has to be decreased
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
    stop("Arguments for number of columns and rfilows to be removed have to be integers.")
  } else if (ncols < 0 || nrows < 0) {
    stop("Arguments for number of columns and rows to be removed have to be positive integers.")
  } else if (ncols == 0 && nrows == 0) {
    stop("At least one argument of ncols or nrows has to be provided.")
  } else if (ncols >= img_cols || nrows >= img_rows) {
    stop("Arguments for number of columns and rows have to be smaller than dimensions of image.")
  }
}

#' Seam Carving Reduce
#'
#' Removes least energy vertical and/or horizontal pixel seams to reduce the size of a given image with respect to its contents.
#' @param imgPath Path to a three channel (RGB) image. (.PNG / .JPG / .TIF)
#' @param ncols Number by which the width has to be decreased
#' @param nrows Number by which the height has to be decreased
#' @return Image <EBImage class>
#' @keywords seamcarving
#' @export
#' @examples
#' library(EBImage)
#' filePath <- system.file("images", "sample-color.png", package="EBImage")
#' reducedImage <- sc_reduce(filePath, ncols = 1, nrows = 3)
#' display(reducedImage)
sc_reduce <- function(imgPath, ncols = 0, nrows = 0) {
  img <- parseImage(imgPath)
  checkUserInput(img, ncols, nrows)
  img_reduced_r <- remove_rows(img, nrows)
  img_reduced <- remove_columns(img_reduced_r, ncols)
  return(img_reduced)
}
