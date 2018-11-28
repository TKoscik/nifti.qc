nii.qc.tpm <- function(template.nii, template.vol=1L,
                       tissue.nii, tissue.vol=1L) {
  template <- as.numeric(read.nii.volume(template.nii, template.vol))
  tissue <- as.numeric(read.nii.volume(tissue.nii, tissue.vol))

  tpm <- sum((template < tissue)*template + (tissue < template)*tissue) / sum((template > tissue)*template + (tissue > template)*tissue)
  return(tpm)
}
