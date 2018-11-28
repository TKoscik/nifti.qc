\name{nii.qc.efc}
\alias{nii.qc.efc}
\title{Entropy Focus Criterion}
\description{
Calculate the EFC. Uses the Shannon entropy of voxel intensities as an indication of ghosting and blurring induced by head motion. A range of low values is better, with EFC = 0 for all the energy concentrated in one pixel.

\deqn{\displaystyle E = -\sum^N_{j=1} \frac{x_j}{x_max} ln \left(\frac{x_j}{x_max}\right)}{E = - \sum_j((x_j/x_max) * ln (x_j/x_max))}

\deqn{\displaystyle x_max = \sqrt{\sum^N_{j=1} x^2_j}}

The original equation is normalized by the maximum entropy, so that the EFC can be compared across images with different dimensions:

\deqn{\displaystyle EFC = \left(\frac{N}{\sqrt{N}} log\sqrt{N}^-1 \right) E}{EFC = ((N/\sqrt(N)) log(\sqrt(N)^-1)) E}

  \describe{
    \item{}{Atkinson D, Hill DL, Stoyle PN, Summers PE, Keevil SF. Automatic correction of motion artifacts in magnetic resonance images using an entropy focus criterion. IEEE Transactions on Medical imaging. 1997 Dec;16(6):903-10.}
  }
}
\usage{
nii.qc.efc(img.nii, img.vol=1L, frame.mask=NULL, frame.vol=1L, frame.dir, frame.thresh = 0)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{frame.mask}{Full directory listing to desired nifti image to use for masking to scan area (unzipped). Must be identical pixel dimensions to base image.}
  \item{frame.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{frame.dir="gt"}{Direction to threshold the mask image to generate the extent of the scanned image. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{frame.thresh=0}{Value at which to threshold the mask image. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  }
\value{
a numeric value indicating entropy focus criterion
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
