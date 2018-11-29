nii.qc.snr <- function(
  img.nii, img.vol=1L,
  mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)
  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- thresh.apply(mask, mask.dir, mask.thresh, "index.arr")
  n <- nrow(mask)
  snr <- (mean(img[mask]))/(sd(img[mask])*sqrt(n/(n-1)))
  return(snr)
}
