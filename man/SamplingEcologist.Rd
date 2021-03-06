% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_internal_r_code.R
\name{SamplingEcologist}
\alias{SamplingEcologist}
\title{The sampling procedure for a certain combination of habitat and ecologist}
\usage{
SamplingEcologist(data, sampling.area, detection.probability,
  identification.error, probability.missed.visits)
}
\arguments{
\item{data}{Subdataset for a certain combination of ecologist and habitat}

\item{sampling.area}{Percentage of sampled area}

\item{detection.probability}{Detection probability of the species}

\item{identification.error}{Identification error of the species}

\item{probability.missed.visits}{Probability of a missed visit for the cell}
}
\value{
data Dataset with the sampled number of individuals per cell.
}
\description{
\code{SamplingEcologist} selects a certain combination of plots and ecologist.
}

