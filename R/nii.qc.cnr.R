nii.qc.cnr <- function(
  img.nii, img.vol=1L,
  gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0,
  wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0,
  air.nii, air.vol=1L, air.dir="eq", air.thresh=1) {

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

  air <- read.nii.volume(air.nii, air.vol)
  air <- switch(air.dir,
                 `gt`=(air > air.thresh) * 1,
                 `ge`=(air >= air.thresh) * 1,
                 `lt`=(air < air.thresh) * 1,
                 `le`=(air <= air.thresh) * 1,
                 `eq`=(air == air.thresh) * 1)
  air <- which(air==1, arr.ind=TRUE)

  cnr <- abs(mean(img[gm] - img[wm]))/sqrt(sd(img[air])^2 + sd(img[gm])^2 + sd(img[wm])^2)

  return(cnr)
}
