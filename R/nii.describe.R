nii.describe <- function(nii.file, save.file=NULL) {

  # debug ----
  rm(list=ls())
  gc()
  library(tools)
  library(jsonlite)
  library(R.utils)
  library(nifti.io)
  top.dir <- "/Shared/koscikt_scratch/nifti.describe_test"
  sub <- "707"
  ses <- "32b8tm8xit"
  nii.file <- paste0(top.dir, "/sub-", sub, "/ses-", ses, "/anat",
                     "/sub-", sub, "_ses-", ses, "_site-00201_acq-sagMPRAGEPROMO_T1w.nii.gz")
  # ----

  nii.dir <- paste(unlist(strsplit(nii.file, "/"))[-length(unlist(strsplit(nii.file, "/")))],collapse="/")
  nii.ext <- file_ext(nii.file)
  if (nii.ext == "gz") {
    nii.ext <- paste(file_ext(file_path_sans_ext(nii.file)), nii.ext, sep=".")
  } else if (nii.ext != "nii") {
    stop("unrecognized file type")
  }
  nii.file <- unlist(strsplit(file_path_sans_ext(nii.file, compression=T), "/"))[length(unlist(strsplit(file_path_sans_ext(nii.file, compression=T), "/")))]

  which.type <- unlist(strsplit(nii.dir, "/"))[length(unlist(strsplit(nii.dir, "/")))]
  which.mod <- unlist(strsplit(nii.file, "_"))[length(unlist(strsplit(nii.file, "_")))]
  which.acq <- unlist(strsplit(unlist(strsplit(nii.file, "_"))[length(unlist(strsplit(nii.file, "_")))-1], "-"))[2]

  json.file <- paste0(nii.dir, "/", nii.file, ".json")
  json.data <- read_json(json.file)

  gunzip(paste0(nii.dir, "/", nii.file, ".nii.gz"), remove=F)
  hdr.data <- nii.hdr(paste0(nii.dir, "/", nii.file, ".nii"))
  file.remove(paste0(nii.dir, "/", nii.file, ".nii"))

  if (which.type == "anat") {
    if (which.mod == "T1w") {
      # T1-weighted images were collected on a 3T GE Discovery MR750w scanner
      # using a Magnetization-Prepared Rapid Acquisition Gradient Echo (MPRAGE)
      # sequence with prospective motion correction (PROMO) with the following
      # parameters in the sagittal plane:
      #    acquisition time = ### s
      #    matrix = ### x ###
      #    voxel size = ### x ### x ### mm
      #    slice thickness = ###
      #    TR = ### ms
      #    TE = ### ms
      #    TI = ### ms
      #    bandwidth = ### Hz
      #    flip angle = ### degrees
      #    FoV read = ### mm
      #    FoV phase = ### %
    } else if (which.mod == "T2w") {

    } else if (which.mod == "T1rho") {

    } else if (which.mod == "FLAIR") {

    } else {
      stop(sprintf("Modality %s not implemented", which.acq))
    }

  } else if (which.type == "dwi") {

  } else if (which.type == "fmap") {

  } else if (which.type == "func") {

  } else {
    stop(sprintf("Data type %s is not implemented", which.type))
  }

}
