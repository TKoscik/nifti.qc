nii.qc.wm2max <- function(
  img.nii, img.vol=1L,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0) {
  img <- read.nii.volume(img.nii, img.vol)
  wm <- read.nii.volume(wm.nii, wm.vol)
  wm <- thresh.apply(wm, wm.dir, wm.thresh, "index.arr")
  wm2max <- mean(img[wm]) / quantile(img[wm], 0.9995)
  return(wm2max)
}
