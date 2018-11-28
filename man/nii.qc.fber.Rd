\name{nii.qc.fber}
\alias{nii.qc.fber}
\title{Foreground to Background Energy Ratio}
\description{
Calculate the FBER, defined as the mean energy of image values within the head relative to outside the head. Higher values are better.

\deqn{\displaystyle FBER = \frac{E|F|^2}{E|B|^2}}{FBER = E|F|^2 / E|B|^2}

  \describe{
    \item{}{Shehzad Z, Giavasis S, Li Q, Benhajali Y, Yan C, Yang Z, Milham M, Bellec P, Craddock C. The Preprocessed Connectomes Project Quality Assessment Protocol—a resource for measuring the quality of MRI data. Frontiers in neuroscience. 2015.}
  }
}
\usage{
nii.qc.fber(img.nii, img.vol=1L, brain.mask, brain.vol=1L, brain.dir="gt", brain.thresh = 0, frame.mask=NULL, frame.vol=1L, frame.dir="gt", frame.thresh = 0)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{brain.mask}{Full directory listing to desired nifti image to use for masking to ROI (unzipped). Must be identical pixel dimensions to base image.}
  \item{brain.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{brain.dir="gt"}{Direction to threshold the mask image to generate the ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{brain.thresh=0}{Value at which to threshold the ROI mask image. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{frame.mask}{Full directory listing to desired nifti image to use for masking to scan area (unzipped). Must be identical pixel dimensions to base image.}
  \item{frame.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{frame.dir="gt"}{Direction to threshold the mask image to generate the extent of the scanned image. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{frame.thresh=0}{Value at which to threshold the mask image. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  }
\value{
a numeric value indicating Foreground to Background Energy Ratio
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
