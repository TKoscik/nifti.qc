nii.qc.rpve <- function(tissue.prob, tissue.vol=1L) {

  pvmap <- read.nii.volume(tissue.prob, tissue.vol)
  pvmap[pvmap < 0] <- 0
  pvmap[pvmap > 1] <- 1
  total.vol <- sum(pvmap > 0)
  hi <- quantile(pvmap[pvmap > 0], 0.98)
  lo <- quantile(pvmap[pvmap > 0], 0.02)
  pvmap <- thresh.apply(pvmap, "btt", c(lo,hi), "zeroed")
  rpve <- (sum(pvmap[pvmap > 0.5]) + (1-sum(pvmap[pvmap <= 0.5]))) / total.vol
  return(rpve)
}
