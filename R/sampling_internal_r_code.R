#' Select plots of the dataset.
#'
#' \code{ChoosePlots} creats a vector in which every plot ID occurs once and
#' than selects randomly a certain number of plot IDs and saves them ordered
#' into a new vector.
#'
#' @param data Complete dataset
#' @param num.of.plots A number
#'
#' @return chosenplots A vector with the name/number of the selected plots
#' @export

ChoosePlots <- function (data, num.of.plots) {

  chosenplots <- sort (sample (unique (data[, 1]), num.of.plots, replace = F))

  return (chosenplots)
}

#' Selects the plots which are monitored by paid observers.
#'
#' \code{PaidPlots} selects randomly the plots of the \code{chosenplots} which
#' are monitored by paid observers.
#'
#' @param chosenplots Output of the function \code{ChoosePlots}
#' @param num.of.plots Number of sampled plots
#' @param num.paid Number of paid observers
#'
#' @return paid.plots A vector with the plot IDs of the cells which are sampled
#' by paid observers.
#' @export

PaidPlots <- function (chosenplots, num.of.plots, num.paid) {

  if (num.of.plots == 1 & num.paid == 1) {
    paid.plots <- chosenplots
  } else {
    paid.plots <- sample (chosenplots, num.paid)
  }

  return (paid.plots)
}

#' Selects the plots which are monitored by volunteers.
#'
#' \code{VolunteerPlots} saves all plots of \code{chosenplots} which are not
#' sampled by paid observers.
#'
#' @param chosenplots Output of the function \code{ChoosePlots}
#' @param paid.plots Plots visited by paid observers
#'
#' @return volunteer.plots A vector with the name/number of the plots.
#' @export

VolunteerPlots <- function (chosenplots, paid.plots) {

  volunteer.plots <- chosenplots[!(chosenplots %in% paid.plots)]

  return (volunteer.plots)
}

#' Prepares a subdataset for a certain combination of habitat and ecologist.
#'
#' \code{PrepareSamplingEcologist} selects a certain combination of plots and
#' ecologist which is then used in the \code{SamplingEcologist} function.
#'
#' @param data Dataset
#' @param ecologist Type of ecologist which sampled the plot
#' @param habitat Habtiat type for which the sampling dataset should be prepared.
#' @param sampling.area Percentage of sampled cell area
#' @param detection.probability Detection probability of the species
#' @param identification.error Identification error of the species
#' @param probability.missed.visits Probability of a missed visit for the cell

#' @return data Subdataset
#' @export

PrepareSamplingEcologist <- function(data, ecologist, habitat, sampling.area, detection.probability,
                                     identification.error, probability.missed.visits) {

  data[data[, 10] == ecologist & data[, 9] == habitat, 2] <- SamplingEcologist(
                        data[data[, 10] == ecologist & data[, 9] == habitat, 2],
                        sampling.area, detection.probability, identification.error,
                        probability.missed.visits)
  return(data)
}

#' The sampling procedure for a certain combination of habitat and ecologist
#'
#' \code{SamplingEcologist} selects a certain combination of plots and ecologist.
#'
#' @param data Subdataset for a certain combination of ecologist and habitat
#' @param sampling.area Percentage of sampled area
#' @param detection.probability Detection probability of the species
#' @param identification.error Identification error of the species
#' @param probability.missed.visits Probability of a missed visit for the cell
#'
#' @return data Dataset with the sampled number of individuals per cell.
#' @export

SamplingEcologist <- function (data, sampling.area, detection.probability,
                         identification.error, probability.missed.visits) {

  for(i in 1:length(data)) {
    data[i] <- rbinom(1, data[i], (sampling.area / 100))
    data[i] <- rbinom(1, data[i], (detection.probability / 100))
    data[i] <- rbinom(1, data[i], (1 - (identification.error / 100)))
    data[i] <- ifelse (sample(0:100, 1) < probability.missed.visits, NA, data[i])
  }
  return(data)
}
