nii.describe <- function(nii.file, save.file=NULL) {
  
  # debug ----
  library(tools)
  library(jsonlite)
  
  nii.file <- "/Shared/nopoulos/sca_pilot/nifti/sub-459/ses-326vrma3kb/anat/sub-459_ses-326vrma3kb_site-00201_acq-sagMPRAGEPROMO_T1w.nii.gz"
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
  
  if (which.type == "anat") {
    if (which.mod == "T1w") {
      # T1-weighted images were collect on a 3T GE Discovery MR750w scanner
      # using an MPRAGE PROMO sequence in the sagittal plane using the following
      # parameters: TR=##ms, TE=##ms, TI=##ms, flip angle=8 degrees,
      # 320x512 matrix, 0.8x0.8mm voxels, 0.8mm thick slices,
      # 0.8mm slice spacing
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
