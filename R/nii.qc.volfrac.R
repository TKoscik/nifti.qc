nii.qc.volfrac <- function(
  whole.nii, whole.vol=1L, whole.dir="eq", whole.thresh=1,
  part.nii, part.vol=1L, part.dir="eq", part.thresh=1) {

  whole <- read.nii.volume(whole.nii, whole.vol)
  whole <- thresh.apply(whole, whole.dir, whole.thresh, "binary")
  whole <- sum(whole)

  part <- read.nii.volume(part.nii, part.vol)
  part <- thresh.apply(part, part.dir, part.thresh, "binary")
  part <- sum(part)

  volfrac <- part / whole
  return(volfrac)
}
