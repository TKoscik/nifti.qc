nii.qc.cjv <- function(
  img.nii, img.vol=1L,
  gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)
  gm <- read.nii.volume(gm.nii, gm.vol)
  gm <- thresh.apply(gm, gm.dir, gm.thresh, "index.arr")
  wm <- read.nii.volume(wm.nii, wm.vol)
  wm <- thresh.apply(wm, wm.dir, wm.thresh, "index.arr")
  cjv <- (sd(img[gm]) + sd(img[wm])) / abs(mean(img[wm]) - mean(img[gm]))
  return(cjv)
}
