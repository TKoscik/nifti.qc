\name{nii.qc.rpve}
\alias{nii.qc.rpve}
\title{Residual Partial Voluming Error}
\description{
Calculate residual partial voluming error for a tissue.
}
\usage{
nii.qc.rpve(tissue.prob, tissue.vol=1L)
}
\arguments{
  \item{tissue.prob}{Full directory listing to desired nifti image (unzipped)}
  \item{tissue.vol=1L}{For multi volume images; an integer indicating the volume to load (1..N)}
}
\value{
a numeric value indicating Foreground to Background Energy Ratio
}
\author{
  Timothy R. Koscik <timkoscik+niiqc@gmail.com>
}
\examples{}
