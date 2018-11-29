nii.qc.fwhm <- function(img.nii, mask.nii, save.dir, file.name) {

  afni.fwhm.cmd <- sprintf("3dFWHMx -mask %s -out %s/%s %s",
                           mask.nii, save.dir, file.name, img.nii)
  system(command = afni.fwhm.cmd)
  fwhm <- data.frame(average = mean(as.numeric(read.delim(paste(save.dir, file.name, sep="/"),
                     header=FALSE, sep=""))),
                     x=as.numeric(read.delim(paste(save.dir, file.name, sep="/"),
                                  header=FALSE, sep="")[1]),
                     y=as.numeric(read.delim(paste(save.dir, file.name, sep="/"),
                                  header=FALSE, sep="")[2]),
                     z=as.numeric(read.delim(paste(save.dir, file.name, sep="/"),
                                  header=FALSE, sep="")[3]))
  return(fwhm)
}
