nii.qc.volfrac <- function(
  whole.nii, whole.vol=1L, whole.dir="eq", whole.thresh=1,
  part.nii, part.vol=1L, part.dir="eq", part.thresh=1) {

  whole <- read.nii.volume(whole.nii, whole.vol)
  whole <- switch(whole.dir,
                  `gt`=(whole > whole.thresh) * 1,
                  `ge`=(whole >= whole.thresh) * 1,
                  `lt`=(whole < whole.thresh) * 1,
                  `le`=(whole <= whole.thresh) * 1,
                  `eq`=(whole == whole.thresh) * 1)
  whole <- sum(whole)

  part <- read.nii.volume(part.nii, part.vol)
  part <- switch(part.dir,
                  `gt`=(part > part.thresh) * 1,
                  `ge`=(part >= part.thresh) * 1,
                  `lt`=(part < part.thresh) * 1,
                  `le`=(part <= part.thresh) * 1,
                  `eq`=(part == part.thresh) * 1)
  part <- sum(part)

  volfrac <- part / whole
  return(volfrac)
}
