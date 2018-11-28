\name{nii.qc.fwhm}
\alias{nii.qc.fwhm}
\title{Full-Width Half Maximum}
\description{
The FWHM of the spatial distribution of the image intensity values in units of voxels. Lower values are better. A wrapper of implementation in AFNI.

\describe{
    \item{}{Forman SD, Cohen JD, Fitzgerald M, Eddy WF, Mintun MA, Noll DC. Improved assessment of significant activation in functional magnetic resonance imaging (fMRI): use of a cluster‐size threshold. Magnetic Resonance in medicine. 1995 May;33(5):636-47.}
}
}
\usage{
nii.qc.fwhm(img.nii, mask.nii, save.dir, file.name)
}
\arguments{
  \item{img.nii}{Full directory listing to desired nifti image.}
  \item{mask.nii}{Full directory listing to desired ROI mask.}
  \item{save.dir}{Directory to save text output from AFNI}
  \item{file.name}{file name for 3dFWHMx AFNIoutput}
}
\value{
returns a dataframe with 1observation of 3 values, average FWHM, x, y, and z values
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
