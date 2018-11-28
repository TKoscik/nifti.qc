nii.qc.anat.snr_d <- function(img.nii, img.vol,
                              mask.nii, mask.vol, mask.dir="gt", mask.thresh=0,
                              air.nii, air.vol, air.dir="eq", air.thresh=1) {

  img <- read.nii.volume(img.nii, img.vol)

  mask <- read.nii.volume(mask.nii, mask.vol)
  mask <- switch(mask.dir,
                 `gt`=(mask > mask.thresh) * 1,
                 `ge`=(mask >= mask.thresh) * 1,
                 `lt`=(mask < mask.thresh) * 1,
                 `le`=(mask <= mask.thresh) * 1,
                 `eq`=(mask == mask.thresh) * 1)
  mask <- which(mask==1, arr.ind = TRUE)

  air <- read.nii.volume(air.nii, air.vol)
  air <- switch(air.dir,
                 `gt`=(air > air.thresh) * 1,
                 `ge`=(air >= air.thresh) * 1,
                 `lt`=(air < air.thresh) * 1,
                 `le`=(air <= air.thresh) * 1,
                 `eq`=(air == air.thresh) * 1)
  air <- which(air==1, arr.ind=TRUE)

  snr_d <- (mean(img[mask]))/(sqrt(2/(4-pi))*sd(img[air]))

  return(snr_d)
}
