nii.qc.wm2max <- function(
  img.nii, img.vol=1L,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0) {

  img <- read.nii.volume(img.nii, img.vol)

  wm <- read.nii.volume(wm.nii, wm.vol)
  wm <- switch(wm.dir,
               `gt`=(wm > wm.thresh) * 1,
               `ge`=(wm >= wm.thresh) * 1,
               `lt`=(wm < wm.thresh) * 1,
               `le`=(wm <= wm.thresh) * 1,
               `eq`=(wm == wm.thresh) * 1)
  wm <- which(wm==1, arr.ind=TRUE)

  wm2max <- mean(img[wm]) / quantile(img[wm], 0.9995)
  return(wm2max)
}
