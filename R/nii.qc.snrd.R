nii.qc.snrd <- function(
  img.nii, img.vol,
  mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0,
  air.nii, air.vol=1L, air.dir="eq", air.thresh=1) {

  img <- read.nii.volume(img.nii, img.vol)
  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- thresh.apply(mask, mask.dir, mask.thresh, "index.arr")
  air <- read.nii.volume(air.nii, air.vol)
  air <- thresh.apply(air, air.dir, air.thresh, "index.arr")
  snr_d <- (mean(img[mask]))/(sqrt(2/(4-pi))*sd(img[air]))
  return(snr_d)
}
