\name{nii.qc.snrd}
\alias{nii.qc.snrd}
\title{Signal-to-Noise Ratio of Tissue Relative to Air}
\description{
Calculate the signal to noise ratio within the masked region relative to the background.

\deqn{\displaystyle SNR_d = \frac{\mu_ROI}{\sqrt{\frac{2}{4-\pi}\sigma_AIR}}}{SNR_d = \mu_ROI/[\sqrt((2/(4-\pi))\sigma_AIR)]}

  \describe{
    \item{}{Dietrich O, Raya JG, Reeder SB, Reiser MF, Schoenberg SO. Measurement of signal‐to‐noise ratios in MR images: influence of multichannel coils, parallel imaging, and reconstruction filters. Journal of Magnetic Resonance Imaging: An Official Journal of the International Society for Magnetic Resonance in Medicine. 2007 Aug;26(2):375-85.}
  }
}
\usage{
nii.qc.snrd(img.nii, img.vol=1L, mask.nii, mask.vol=1L, mask.dir="gt", mask.thresh=0, air.nii, air.vol=1L, air.dir="eq", air.thresh=1)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{mask.nii}{Full directory listing to desired nifti image to use for masking (unzipped). Must be identical pixel dimensions to base image.}
  \item{mask.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{mask.dir="gt"}{Direction to threshold the mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{mask.thresh=0}{Value at which to threshold the mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{air.nii}{Full directory listing to desired nifti image to use for identifying the air region around the head (unzipped). Must be identical pixel dimensions to base image.}
  \item{air.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.dir="gt"}{Direction to threshold the air mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{air.thresh=0}{Value at which to threshold the air mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
a numeric value indicating signal-to-noise-ratio relative to air
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
