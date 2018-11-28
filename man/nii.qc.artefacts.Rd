\name{nii.qc.artefacts}
\alias{nii.qc.artefacts}
\title{Generate Artefact Mask}
\description{
Identify and generate binary mask of image artefacts within the background of a NIFTI image.
  \describe{
    \item{}{Mortamet B, Bernstein MA, Jack Jr CR, Gunter JL, Ward C, Britson PJ, Meuli R, Thiran JP, Krueger G. Automatic quality assessment in structural brain magnetic resonance imaging. Magnetic Resonance in Medicine: An Official Journal of the International Society for Magnetic Resonance in Medicine. 2009 Aug;62(2):365-72.}
  }
}
\usage{
nii.qc.artefacts(img.nii, img.vol=1L, air.nii, air.vol=1L, air.dir="eq", air.thresh=1, save.dir, file.name)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image (unzipped)}
  \item{img.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.nii}{Full directory listing to desired nifti image to use for identifying the air region around the head (unzipped). Must be identical pixel dimensions to base image.}
  \item{air.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
  \item{air.dir="gt"}{Direction to threshold the air mask image to generate the desired ROI. Options include ("gt") [>] Default, "ge" [>=], "lt" [<], "le" [<=], "eq" [==]}
  \item{air.thresh=0}{Value at which to threshold the air mask image to generate an ROI. E.g., for binary masks, where the value 1 indicates the ROI, default of >0, will generate the appropriate mask}
  \item{save.dir}{Directory to save binary mask NIFTI image}
  \item{file.name}{file name for artefact mask, must include .nii extension}
}
\value{
returns an array with the same dimensions as the input image with binary values where 1 corresponds to artefacts. Also writes this image to a NIFTI file.
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
