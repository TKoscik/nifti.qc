nii.qc.fber <- function(
  img.nii, img.vol=1L,
  brain.mask, brain.vol=1L, brain.dir="gt", brain.thresh = 0,
  frame.mask=NULL, frame.vol=1L, frame.dir="gt", frame.thresh = 0) {

  img <- read.nii.volume(img.nii, img.vol)

  brain <- read.nii.volume(brain.mask, brain.vol)
  brain <- thresh.apply(brain, brain.dir, brain.thresh, "index.arr")

  if (!is.null(frame.mask)) {
    frame <- read.nii.volume(frame.nii, frame.vol)
    frame <- thresh.apply(frame, frame.dir, frame.thresh, "binary")
  } else {
    frame <- array(1, dim=dim(img))
  }
  frame[brain] <- 0
  frame <- which(frame==1, arr.ind=TRUE)

  fg.mu <- median(abs(img[brain])^2)
  bg.mu <- median(abs(img[frame])^2)

  if (bg.mu < 1e-3) {
    fber <- 0
  } else {
    fber <- fg.mu / bg.mu
  }
  return(fber)
}
