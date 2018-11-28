\name{nii.qc.mortamet}
\alias{nii.qc.mortamet}
\title{Generate Mortamet's Quality Indices}
\description{
Generate Quality Indices from Mortamet, et al. (2009).

QI1 is the proportion of voxels with intensity corrupted by artifacts normalized by the number of voxels in the background
\deqn{\displaystyle QI_1 = \frac{1}{N}\sum_{x \in X_ART}1}{QI1 = (1/N)\sum_[x \in X_ART](1)}

QI2 is based on the goodness-of-fit of a centered χ2 distribution onto the intensity distribution of non-artifactual background (within the “hat” mask)
\deqn{\displaystyle \chi^2_n = \frac{2}{\left(\sigma\sqrt{2}\right)^2n \left(n-1\right)!}x^2n-1 e^-\frac{x}{2}}{\chi^2_n = 2/(((\sigma\sqrt2)^2n)(n-1)!) x^(2n-1) e^(-x/2)}

\describe{
    \item{}{Mortamet B, Bernstein MA, Jack Jr CR, Gunter JL, Ward C, Britson PJ, Meuli R, Thiran JP, Krueger G. Automatic quality assessment in structural brain magnetic resonance imaging. Magnetic Resonance in Medicine: An Official Journal of the International Society for Magnetic Resonance in Medicine. 2009 Aug;62(2):365-72.}
}
}
\usage{
nii.qc.mortamet(img.nii, img.vol=1L, air.nii, air.vol=1L, air.dir="eq", air.thresh=1, art.nii, art.vol=1L, art.dir="eq", art.thresh=1)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.nii}{Full directory listing to desired nifti image to use for identifying the air region around the head (unzipped). Must be identical pixel dimensions to base image.}
  \item{air.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.dir="gt"}{Direction to threshold the air mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{air.thresh=0}{Value at which to threshold the air mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{art.nii}{Full directory listing to desired nifti image to use for identifying the artefact region around the head (unzipped). Must be identical pixel dimensions to base image.}
  \item{art.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{art.dir="gt"}{Direction to threshold the artefact mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{art.thresh=0}{Value at which to threshold the artefact mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
}
\value{
returns a named list containing the values of "qi1", "qi2", "kde" (observed noise probability distribution), "fit.chi" (the fitted χ2 distribution), "obs.noise" (a dataframe of the observed noise values), "exp.dist" (a dataframe of the expected noise probability distribution), "plot" (a density plot to compare observed and expected noise).
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
