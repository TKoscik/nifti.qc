nii.qc.artefacts <- function(
  img.nii, img.vol=1L,
  air.nii, air.vol=1L, air.dir="eq", air.thresh=1,
  save.dir, file.name) {

  img <- read.nii.volume(img.nii, img.vol)
  air <- read.nii.volume(air.nii, air.vol)
  air <- thresh.apply(air,air.dir,air.thresh, "index.arr")

  # find background threshold (mode excluding 0)
  img[air] <- scale(img[air],
                    center = median(img[air]),
                    scale = mad(img[air]))

  # threshold image
  img <- img * (img > 0.1)
  img <- (img > 10)*1

  # open image, using mmand package
  img <- opening(img, array(1, dim=c(3,3,3)))

  # save artefact image
  init.nii(file.name = paste0(save.dir, "/", file.name),
           dims = dim(img),
           pixdim = unlist(nii.hdr(img.nii, "pixim")),
           orient = nii.orient(img.nii))
  write.nii.volume(paste0(save.dir, "/", file.name), 1, img)
  return(img)
}
