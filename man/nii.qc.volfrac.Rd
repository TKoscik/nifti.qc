\name{nii.qc.volfrac}
\alias{nii.qc.volfrac}
\title{Fractional Volume}
\description{
Calculate fractional volume.
}
\usage{
nii.qc.volfrac(whole.nii, whole.vol=1L, whole.dir="eq", whole.thresh=1, part.nii, part.vol=1L, part.dir="eq", part.thresh=1)
}
\arguments{
  \item{whole.nii}{Full directory listing to desired nifti image to use for masking the whole ROI (e.g., brain mask) (unzipped). Must be identical pixel dimensions to base image.}
  \item{whole.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{whole.dir="gt"}{Direction to threshold the mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{whole.thresh=0}{Value at which to threshold the mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{part.nii}{Full directory listing to desired nifti image to use for masking the part ROI (e.g., tissue mask) (unzipped). Must be identical pixel dimensions to base image.}
  \item{part.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{part.dir="gt"}{Direction to threshold the mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{part.thresh=0}{Value at which to threshold the mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
a numeric value indicating partial volume fraction
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
