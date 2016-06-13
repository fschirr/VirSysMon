#' Run the Model with repetitions
#'
#' \code{Evaluation} runs the 'Sampling' function with repetitions.
#'
#' @param data The input dataset.
#' @param num.of.plots.forest Number of plots with forest habitat
#' @param num.of.plots.farmland Number of plots with farmland habitat
#' @param num.of.plots.grassland Number of plots with grassland habitat
#' @param paid Vector created with \code{CreateEcologist} for paid observers
#' @param volunteer Vector created with \code{CreateEcologist} for volunteers
#' @param num.paid.forest Number of plots with forest habitat that are sampled by paid observers
#' @param num.paid.farmland Number of plots with farmland habitat that are sampled by paid observers
#' @param num.paid.grassland Number of plots with grassland habitat that are sampled by paid observers
#' @param frequency.year Yearly sampling frequency
#' @param frequency.month Montly sampling frequency
#' @param frequency.day Daily sampling frequency
#' @param outputall If `True` complete dataset is returned. If False only the sampled plots are returned
#' @param repetition Number of repetitions of the model
#'
#' @return Result
#' @export

Evaluation <- function (data, num.of.plots.forest, num.of.plots.farmland, num.of.plots.grassland, paid, volunteer,
                        num.paid.forest, num.paid.farmland, num.paid.grassland,
                        frequency.year, frequency.month, frequency.day,
                        outputall, repetition) {
outputdata <- NULL
  for (k in 1:repetition) {

    newdata <- Sampling (data, num.of.plots.forest, num.of.plots.farmland, num.of.plots.grassland, paid, volunteer,
                        num.paid.forest, num.paid.farmland, num.paid.grassland,
                        frequency.year, frequency.month, frequency.day,
                        outputall)

    newdata$repetition <- k

    outputdata <- rbind(outputdata, newdata)
  }

return (outputdata)
}
