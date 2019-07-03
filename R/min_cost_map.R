#' min_cost_map
#'
#' Function for calculating the minimal cost map and a backtrack map for finding seams.
#' @param img EBImage Image class object
#' @param ncol Number by which the width has to be decreased
#' @param nrow Number by which the height has to be decreased
#' @return list(EnergyMap , BacktrackMap)
min_cost_map <- function(img, ncol, nrow) {
  energy_map <- calc_energy(img)
  M <- energy_map #Map of pixel costs
  backtrack <- energy_map * 0L # used for finding min energy seam

  for (c in c(1:ncol)) {
    for (r in c(2:nrow)) {
      if (c == 1) {
        a <- M[c, r - 1]
        b <- M[c + 1, r - 1]
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
