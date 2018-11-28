nii.qc.snr <- function(
  img.nii, img.vol=1L,
  mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)

  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- switch(mask.dir,
                 `gt`=(mask > mask.thresh) * 1,
                 `ge`=(mask >= mask.thresh) * 1,
                 `lt`=(mask < mask.thresh) * 1,
                 `le`=(mask <= mask.thresh) * 1,
                 `eq`=(mask == mask.thresh) * 1)
  mask <- which(mask==1, arr.ind = TRUE)
  n <- nrow(mask)

  snr <- (mean(img[mask]))/(sd(img[mask])*sqrt(n/(n-1)))
  return(snr)
}
