nii.qc.cjv <- function(
  img.nii, img.vol=1L,
  gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)

  gm <- read.nii.volume(gm.nii, gm.vol)
  gm <- switch(gm.dir,
               `gt`=(gm > gm.thresh) * 1,
               `ge`=(gm >= gm.thresh) * 1,
               `lt`=(gm < gm.thresh) * 1,
               `le`=(gm <= gm.thresh) * 1,
               `eq`=(gm == gm.thresh) * 1)
  gm <- which(gm==1, arr.ind=TRUE)

  wm <- read.nii.volume(wm.nii, wm.vol)
  wm <- switch(wm.dir,
               `gt`=(wm > wm.thresh) * 1,
               `ge`=(wm >= wm.thresh) * 1,
               `lt`=(wm < wm.thresh) * 1,
               `le`=(wm <= wm.thresh) * 1,
               `eq`=(wm == wm.thresh) * 1)
  wm <- which(wm==1, arr.ind=TRUE)

  cjv <- (sd(img[gm]) + sd(img[wm])) / abs(mean(img[wm]) - mean(img[gm]))

  return(cjv)
}
