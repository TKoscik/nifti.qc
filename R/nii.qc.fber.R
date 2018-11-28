nii.qc.fber <- function(
  img.nii, img.vol=1L,
  brain.mask, brain.vol=1L, brain.dir="gt", brain.thresh = 0,
  frame.mask=NULL, frame.vol=1L, frame.dir="gt", frame.thresh = 0) {

  img <- read.nii.volume(img.nii, img.vol)

  brain <- read.nii.volume(brain.nii, brain.vol)
  brain <- switch(brain.dir,
               `gt`=(brain > brain.thresh) * 1,
               `ge`=(brain >= brain.thresh) * 1,
               `lt`=(brain < brain.thresh) * 1,
               `le`=(brain <= brain.thresh) * 1,
               `eq`=(brain == brain.thresh) * 1)
  brain <- which(brain==1, arr.ind=TRUE)

  if (!is.null(frame.mask)) {
    frame <- read.nii.volume(frame.nii, frame.vol)
    frame <- switch(frame.dir,
                 `gt`=(frame > frame.thresh) * 1,
                 `ge`=(frame >= frame.thresh) * 1,
                 `lt`=(frame < frame.thresh) * 1,
                 `le`=(frame <= frame.thresh) * 1,
                 `eq`=(frame == frame.thresh) * 1)
  } else {
    frame <- array(1, dim=dim(img))
  }
  frame[brain] <- 0
  frame <- which(frame==1, arr.ind=TRUE)

  fg.mu <- median(abs(img[brain>0])^2)
  bg.mu <- median(abs(img[frame>0])^2)

  if (bg_mu < 1e-3) {
    fber <- 0
  } else {
    fber <- fg_mu / bg_mu
  }
  return(fber)
}
