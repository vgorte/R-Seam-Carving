#installation of EBImage:
#RUN:
#
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("EBImage")
#DOCS: browseVignettes("EBImage")



library(EBImage)

#working
calc_energy <- function(img) {
  print(img, short = T)
  
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
  
  print(display(combine(img, imgH, imgV, imgE),method = "raster",all = T))
  
  imgE_summed <-(imgE@.Data[, , 1] + imgE@.Data[, , 2] + imgE@.Data[, , 3]) / 3
  return(imgE_summed)
}




minimum_seam <- function(img, ncol, nrow) {
  energy_map <- calc_energy(img)
  M <- energy_map
  backtrack <- energy_map * 0L
  
  for (c in c(1:nrow)) {
    for (r in c(2:ncol)) {
      
      
      
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
  min_energy
  
  
  M[c, r] = M[c, r] + min_energy
  result <- list(M = M, backtrack = backtrack)
  return(result)
}




carve_column <- function(img , ncol, nrow) {
  imgEMap <- minimum_seam(img, ncol, nrow)
  M <- imgEMap$M
  backtrack <- imgEMap$backtrack
  
  #Creating a (c,r) matrix filled with the value TRUE
  #All pixels that need to be removed will be marked as FALSE in the mask
  mask <- matrix(TRUE, ncol = ncol, nrow = nrow,)
  
  #Position of smallest element in the last row of M
  ielem <- which.min(imgEMap$M[, nrow])
  
  for (r in rev(c(1:nrow))) {
    mask[r, ielem] = FALSE
    ielem = backtrack[ielem, r]
  }
  
  return(mask)
}



img <- readImage("testimg.jpg")
ncol <- dim(img)[1]
nrow <- dim(img)[2]

mask <- carve_column(img, ncol, nrow)

#length(which(mask)) #Number of TRUE's in mask -> Should be (ncol * nrow) - nrow


r <- img[, , 1]
g <- img[, , 2]
b <- img[, , 3]


r_masked <- Image(matrix(r[mask], nrow = nrow, ncol = ncol - 1))
g_masked <- Image(matrix(g[mask], nrow = nrow, ncol = ncol - 1))
b_masked <- Image(matrix(b[mask], nrow = nrow, ncol = ncol - 1))

test_masked <-rgbImage(red = r_masked, green = g_masked, blue = b_masked)
display(test_masked)
