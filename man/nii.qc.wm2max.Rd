\name{nii.qc.wm2max}
\alias{nii.qc.wm2max}
\title{White Matter-to-Maximum Ratio}
\description{
Calculate the WM2MAX ratio, defined as the maximum intensity found in the volume w.r.t. the mean value of the white matter tissue. Values close to 1.0 are better.
}
\usage{
nii.qc.wm2max(img.nii, img.vol=1L, wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{wm.nii}{Full directory listing to desired nifti image to use for WM masking (unzipped). Must be identical pixel dimensions to base image.}
  \item{wm.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{wm.dir="gt"}{Direction to threshold the mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{wm.thresh=0}{Value at which to threshold the mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
a numeric value indicating WM2MAX
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
