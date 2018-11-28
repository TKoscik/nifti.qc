\name{nii.qc.descriptive}
\alias{nii.qc.descriptive}
\title{Descriptive statistics on voxel intensities within an ROI}
\description{
Descriptive statistics on voxel intensities within an ROI.
Including: mean, standard deviation, median, median absolute deviation, skewness, kurtosis, 5\% and 95\% quantiles, and a density plot of intensities.
}
\usage{
nii.qc.descriptive(img.nii, img.vol=1L, mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{mask.nii}{Full directory listing to desired nifti image to use for masking (unzipped). Must be identical pixel dimensions to base image.}
  \item{mask.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{mask.dir="gt"}{Direction to threshold the mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{mask.thresh=0}{Value at which to threshold the mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
returns a named list with descriptors of central tendency and a density plot of image intensities within the masked region
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
