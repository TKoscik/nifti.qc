nii.qc.anat.volfrac <- function(brain.nii, brain.vol=1, brain.dir="eq", brain.thresh=1,
                                tissue.nii, tissue.vol=1, tissue.dir="eq", tissue.thresh=1) {

  brain <- read.nii.volume(brain.nii, brain.vol)
  brain <- switch(brain.dir,
                  `gt`=(brain > brain.thresh) * 1,
                  `ge`=(brain >= brain.thresh) * 1,
                  `lt`=(brain < brain.thresh) * 1,
                  `le`=(brain <= brain.thresh) * 1,
                  `eq`=(brain == brain.thresh) * 1)
  total.vol <- sum(brain)

  tissue <- read.nii.volume(tissue.nii, tissue.vol)
  tissue <- switch(tissue.dir,
                  `gt`=(tissue > tissue.thresh) * 1,
                  `ge`=(tissue >= tissue.thresh) * 1,
                  `lt`=(tissue < tissue.thresh) * 1,
                  `le`=(tissue <= tissue.thresh) * 1,
                  `eq`=(tissue == tissue.thresh) * 1)
  tissue.vol <- sum(tissue)

  volfrac <- tissue.vol / total.vol
  return(volfrac)
}
