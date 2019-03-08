nii.qc.efc <- function(
  img.nii, img.vol=1L,
  frame.mask=NULL, frame.vol=1L, frame.dir, frame.thresh = 0) {

  img <- read.nii.volume(img.nii, img.vol)
  if (!is.null(frame.mask)) {
    mask <- read.nii.volume(frame.mask, frame.vol)
    mask <- thresh.apply(mask, frame.dir, frame.thresh, "index.arr")
    img <- img[mask]
  }
  img.max <- sqrt(sum(img^2))
  efc.temp <- -sum((img/img.max)*log((img+1e-16)/img.max)) # add 1e-16 to deal with log(0) being Inf
  n <- length(img)
  efc <- efc.temp / (n/sqrt(n)*log(1/sqrt(n)))
  return(efc)
}
