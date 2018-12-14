nii.qc.cnr <- function(
  img.nii, img.vol=1L,
  gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0,
  air.nii, air.vol=1L, air.dir="eq", air.thresh=1) {

  img <- read.nii.volume(img.nii, img.vol)
  gm <- read.nii.volume(gm.nii, gm.vol)
  gm <- thresh.apply(gm, gm.dir, gm.thresh, "index.arr")
  wm <- read.nii.volume(wm.nii, wm.vol)
  wm <- thresh.apply(wm, wm.dir, wm.thresh, "index.arr")
  air <- read.nii.volume(air.nii, air.vol)
  air <- thresh.apply(air, air.dir, air.thresh, "index.arr")
  cnr <- abs(mean(img[gm]) - mean(img[wm]))/sqrt(sd(img[air])^2 + sd(img[gm])^2 + sd(img[wm])^2)
  return(cnr)
}
