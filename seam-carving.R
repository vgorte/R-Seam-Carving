#installation of EBImage:
#RUN:
#
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("EBImage")



library(EBImage)

calc_energy <- function(img) {
  print(img, short = T)
  
  # define horizontal and vertical Sobel kernel
  kernal_h <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3)
  kernal_v <- t(kernal_h)
  
  # get horizontal and vertical edges
  imgH <- filter2(img, kernal_h, boundary = "replicate")
  imgV <- filter2(img, kernal_v, boundary = "replicate")
  
  # combine edge pixel data to get overall edge data
  hdata <- imageData(imgH)
  vdata <- imageData(imgV)
  edata <- sqrt(hdata^2 + vdata^2)
  
  # transform edge data to image
  imgE <- Image(edata, colormode = 2)
  
  print(display(combine(img, imgH, imgV, imgE), method = "raster", all = T))
  
  imgE_summed <- (imgE@.Data[,,1] + imgE@.Data[,,2] + imgE@.Data[,,3]) / 3
  return(imgE_summed)
}


minimum_seam <- function(img) {
  ncol <- dim(img)[1]
  nrow <- dim(img)[2]
  energy_map <- calc_energy(img)
  
  M <- energy_map
  backtrack <- energy_map *0L

  for (i in c(1:ncol)) { #First row stays zero
    for (j in  c(2:nrow)) {
      if (j == 1) {
        idx = min(c(M[i-1, j], M[i-1, j+1], M[i-1, j+2]))
        backtrack[i,j] = idx + j
        min_energy = M[i-1, idx+j]
      }else if(j == ncol-1){
        print("hi")
        idx = min(c(M[i-1, j-1], M[i-1, j], M[i-1, j+1]))
        backtrack[i,j] = idx + j - 1
        min_energy = M[i-1, idx+j-1]
      }else if(j == ncol){
        print("yo")
        idx = min(c(M[i-1, j-1], M[i-1, j]))
        backtrack[i,j] = idx + j - 1
        min_energy = M[i-1, idx+j-1]
      }else{
        idx = min(c(M[i-1, j-1], M[i-1, j], M[i-1, j+1], M[i-1, j+2]))
        backtrack[i,j] = idx + j - 1
        min_energy = M[i-1, idx+j-1]
        return(which(M == idx, arr.ind = TRUE))
      }
      M[i,j] = M[i,j] + min_energy
    }
  }
  
  result <- list(M, backtrack)
  return(result)
}


img <- readImage("testimg.jpg")
imgEMap <- minimum_seam(img)
