\name{nii.qc.cjv}
\alias{nii.qc.cjv}
\title{Coefficient of Joint Variation}
\description{
CJV, a measure related to SNR and CNR that is presented as a proxy for the INU artifact. Lower is better.

\deqn{\displaystyle CJV = \frac{\sigma_WM + \sigma_GM}{|\mu_WM - \mu_GM|}}{CJV = (\sigma_WM + \sigma_GM)/|\mu_WM - \mu_GM|}

\describe{
    \item{}{Ganzetti M, Wenderoth N, Mantini D. Intensity inhomogeneity correction of structural MR images: a data-driven approach to define input algorithm parameters. Frontiers in neuroinformatics. 2016 Mar 15;10:10.}
}
}
\usage{
nii.qc.cjv(img.nii, img.vol=1L, gm.nii, gm.vol=1L, gm.dir="gt", gm.thresh=0, wm.nii, wm.vol=1L, wm.dir="gt", wm.thresh=0)
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
}
\value{
returns a numeric value corresponding to CJV
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
