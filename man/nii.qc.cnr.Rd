\name{nii.qc.cnr}
\alias{nii.qc.cnr}
\title{Contrast-to-Noise Ratio}
\description{
Calculate the the contrast ratio between GM and WM regions relative to background noise.

\deqn{\displaystyle CNR = \frac{|\mu_GM - \mu_WM|}{\sqrt{\sigma^2_AIR + \sigma^2_WM + \sigma^2_GM}}}{CNR = |\mu_GM - \mu_WM| / [\sqrt(\sigma^2_AIR + \sigma^2_WM + \sigma^2_GM)]}

  \describe{
    \item{}{Magnotta VA, Friedman L, BIRN F. Measurement of signal-to-noise and contrast-to-noise in the fBIRN multicenter imaging study. Journal of digital imaging. 2006 Jun 1;19(2):140-7.}
  }
}
\usage{
nii.qc.cnr(img.nii, img.vol=1L, gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0, wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0, air.nii, air.vol=1L, air.dir="eq", air.thresh=1)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{gm.nii}{Full directory listing to desired nifti image to use for GM (unzipped). Must be identical pixel dimensions to base image.}
  \item{gm.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{gm.dir="gt"}{Direction to threshold the GM mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{gm.thresh=0}{Value at which to threshold the GM mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{wm.nii}{Full directory listing to desired nifti image to use for WM (unzipped). Must be identical pixel dimensions to base image.}
  \item{wm.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{wm.dir="gt"}{Direction to threshold the WM mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{wm.thresh=0}{Value at which to threshold the WM mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{air.nii}{Full directory listing to desired nifti image to use for identifying the air region around the head (unzipped). Must be identical pixel dimensions to base image.}
  \item{air.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.dir="gt"}{Direction to threshold the air mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{air.thresh=0}{Value at which to threshold the air mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
a numeric value indicating contrast-to-noise-ratio relative to air
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
