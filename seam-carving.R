#installation of EBImage:
#RUN:
#
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")
#DOCS: browseVignettes("EBImage")


library(EBImage)

# Function for calculating the energy map using Sobel
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

# Function for calculating the minimal cost map and a backtrack map for finding seams.
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

# Function for replacing the values of the lowest cost seam in the image with NA
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

# Function for omitting all NA's in the image, which represent the seam.
# Image is reshaped to new dimensions after.
remove_seam <- function(img) {
  img_reduced <- aperm(apply(img, c(2, 3), na.omit), c(1, 2, 3))
  return(Image(img_reduced, colormode = 2))
}

# Function for removing n lowest cost seams in image (Vertical)
remove_columns <- function(img, n) {
  for (i in 1:n) {
    img = mark_Seam(img)
    img = remove_seam(img)
  }
  return(img)
}

# Function for removing n lowest cost seams in image (Horizontal).
remove_rows <- function(img, n) {
  #rotation
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  
  for (i in 1:n) {
    img = mark_Seam(img)
    img = remove_seam(img)
  }
  
  #rotate back to original orientation
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  return(img)
}

# Function for reading the image from the given path.
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

# Function for validating the input arguments.
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
    stop("Arguments for number of columns and rows to be removed have integers.")
  } else if (ncols < 0 || nrows < 0) {
    stop("Arguments for number of columns and rows to be removed have to be positive integers.")
  } else if (ncols == 0 && nrows == 0) {
    stop("At least of argument of ncols or nrows has to be provided.")
  } else if (ncols >= img_cols || nrows >= img_rows) {
    stop("Arguments for number of columns and rows have to be smaller than dimensions of image.")
  }
}

# Function for removing x columns and y rows of a given RGB image.
seam_carver <- function(imgPath, ncols = 0, nrows = 0) {
  img <- parseImage(imgPath)
  checkUserInput(img, ncols, nrows)
  img_reduced_r <- remove_rows(img, nrows)
  img_reduced <- remove_columns(img_reduced_r, ncols)
  return(img_reduced)
}



path <- "test.jpg"
resized_img <- seam_carver(path, 1, 1)
display(resized_img)
