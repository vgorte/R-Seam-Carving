#installation of EBImage:
#RUN:
#
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("EBImage")
#DOCS: browseVignettes("EBImage")


library(EBImage)

calc_energy <- function(img) {
  #print(img, short = T)
  
  # define horizontal and vertical Sobel kernel
  kernal_h <- matrix(c(1, 2, 1, 0, 0, 0,-1,-2,-1), nrow = 3)
  kernal_v <- t(kernal_h)
  
  # get horizontal and vertical edges
  imgH <- filter2(img, kernal_h, boundary = "replicate")
  
  imgV <- filter2(img, kernal_v, boundary = "replicate")
  
  # combine edge pixel data to get overall edge data
  hdata <- imageData(imgH)
  vdata <- imageData(imgV)
  edata <- sqrt(hdata ^ 2 + vdata ^ 2)
  
  # transform edge data to image
  imgE <- Image(edata, colormode = 2)
  
  display(combine(img, imgH, imgV, imgE),method = "raster",all = T)

  imgE_summed <- (imgE@.Data[, , 1] + imgE@.Data[, , 2] + imgE@.Data[, , 3]) / 3
  return(imgE_summed)
}

minimum_seam <- function(img, ncol, nrow) {
  energy_map <- calc_energy(img)
  
  M <- energy_map #Map of pixel costs
  backtrack <- energy_map * 0L # used for finding min energy path
  
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

mark_Seam <- function(img) {
  ncol <- as.numeric(dim(img)[1])
  nrow <- as.numeric(dim(img)[2])
  
  imgEMap <- minimum_seam(img, ncol, nrow)
  M <- imgEMap$M
  backtrack <- imgEMap$backtrack
  
  img_array <- as.array(img)
  
  c <- which.min(M[, nrow])
  
  for (r in rev(c(1:nrow))) {
    img_array[c, r,] = NA
    c = backtrack[c, r]
  }
  
  test <- na.omit(img)
  return(Image(img_array, colormode = 2))
}

remove_seam <- function(img) {
  #Check if there is exactly 1 NA per row
  if (any(apply(img, c(2, 3), function(x)
    sum(is.na(x))) != 1)) {
    stop("Error: Multiple NA's in one row")
    
  } else{
    img_reduced <- aperm(apply(img, c(2, 3), na.omit), c(1, 2, 3))
    return(Image(img_reduced, colormode = 2))
  }
}

#Mark and remove one seam at a time, n times
remove_columns <- function(img, n) {
  for (i in 1:n) {
    img = mark_Seam(img)
    img = remove_seam(img)
  }
  return(img)
}

#Same as in remove_columns function, but first the image is rotated
remove_rows <- function(img, n) {
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  
  for (i in 1:n) {
    img = mark_Seam(img)
    img = remove_seam(img)
  }
  
  img <- Image(aperm(img, c(2, 1, 3)), colormode = 2)
  return(img)
}

#Function for removing x columns and y rosw of a given RGB 
seam_carver <- function(imgPath, ncols, nrows) {
  img <- readImage(imgPath)
  print("original dimensions:")
  print(dim(img))
  img_reduced_r<- remove_rows(img, nrows)
  print("rows removed...")
  img_reduced <- remove_columns(img_reduced_r, ncols)
  print("columns removed...")
  print("reduced dimensions:")
  print(dim(img_reduced))
  
  display(img_reduced)
  return(img_reduced)
}

path <- "testimg.jpg"
resized_img <- seam_carver(path, 10, 13)
display(resized_img)
