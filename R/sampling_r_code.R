#' Main function
#'
#' \code{Sampling} runs the 'Sampling' function.
#'
#' @param data The input dataset
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
#' @param outputall If 'True' complete dataset is returned. If 'False' only the sampled plots are returned
#'
#' @return Depending on \code{outputall} the whole dataset or just the sampled cells are returned.
#' @export

Sampling <- function (data, num.of.plots.forest, num.of.plots.farmland, num.of.plots.grassland, paid, volunteer,
                       num.paid.forest, num.paid.farmland, num.paid.grassland,
                     frequency.year, frequency.month, frequency.day,
                     outputall) {

  currentdata <- data

  currentdata$paid.volunteer <- 0
  currentdata$costs <- 0

  chosenplots.forest <- ChoosePlots (currentdata[currentdata[, 9] == "forest", ], num.of.plots.forest)
  chosenplots.farmland <- ChoosePlots (currentdata[currentdata[, 9] == "farmland", ], num.of.plots.farmland)
  chosenplots.grassland <- ChoosePlots (currentdata[currentdata[, 9] == "grassland", ], num.of.plots.grassland)

  paid.plots.forest <- PaidPlots (chosenplots.forest, num.of.plots.forest, num.paid.forest)
  volunteer.plots.forest <- VolunteerPlots (chosenplots.forest, paid.plots.forest)

  paid.plots.farmland <- PaidPlots (chosenplots.farmland, num.of.plots.farmland, num.paid.farmland)
  volunteer.plots.farmland <- VolunteerPlots (chosenplots.farmland, paid.plots.farmland)

  paid.plots.grassland <- PaidPlots (chosenplots.grassland, num.of.plots.grassland, num.paid.grassland)
  volunteer.plots.grassland <- VolunteerPlots (chosenplots.grassland, paid.plots.grassland)

  year <- sort(unique (currentdata[, 6]))
  year <- year[seq (1, length(year), frequency.year)]

  if (frequency.month > 0) {
    month <- sort (unique (currentdata[, 7]))
    month <- month[seq (1, length(month), frequency.month)]
  } else {
    month <- 0
  }

  if (frequency.day > 0) {
    day <- sort (unique (currentdata[, 8]))
    day <- day[seq (1, length(day), frequency.day)]
  } else {
    day <- 0
  }

    currentdata$paid.volunteer[currentdata[, 1] %in% paid.plots.forest &
                                 currentdata[, 6] %in% year & currentdata[, 7]
                                 %in% month & currentdata[, 8]
                                 %in% day] <- "paid"

    currentdata$costs[currentdata[, 1] %in% paid.plots.forest & currentdata[, 6]
                      %in% year & currentdata[, 7] %in% month & currentdata[, 8]
                      %in% day] <- paid[7]

    currentdata$paid.volunteer[currentdata[, 1] %in% volunteer.plots.forest &
                                     currentdata[, 6] %in% year &
                                     currentdata[, 7] %in% month &
                                     currentdata[, 8] %in% day] <- "volunteer"

    currentdata$costs[currentdata[, 1] %in% volunteer.plots.forest & currentdata[, 6]
                        %in% year & currentdata[, 7] %in% month &
                          currentdata[, 8] %in% day] <- volunteer[7]

    currentdata$paid.volunteer[currentdata[, 1] %in% paid.plots.farmland &
                               currentdata[, 6] %in% year & currentdata[, 7]
                             %in% month & currentdata[, 8]
                             %in% day] <- "paid"

    currentdata$costs[currentdata[, 1] %in% paid.plots.farmland & currentdata[, 6]
                      %in% year & currentdata[, 7] %in% month & currentdata[, 8]
                      %in% day] <- paid[7]

    currentdata$paid.volunteer[currentdata[, 1] %in% volunteer.plots.farmland &
                               currentdata[, 6] %in% year &
                               currentdata[, 7] %in% month &
                               currentdata[, 8] %in% day] <- "volunteer"

    currentdata$costs[currentdata[, 1] %in% volunteer.plots.farmland & currentdata[, 6]
                  %in% year & currentdata[, 7] %in% month & currentdata[, 8]
                  %in% day] <- volunteer[7]

    currentdata$paid.volunteer[currentdata[, 1] %in% paid.plots.grassland &
                               currentdata[, 6] %in% year & currentdata[, 7]
                             %in% month & currentdata[, 8]
                             %in% day] <- "paid"

    currentdata$costs[currentdata[, 1] %in% paid.plots.grassland & currentdata[, 6]
                  %in% year & currentdata[, 7] %in% month & currentdata[, 8]
                  %in% day] <- paid[7]

    currentdata$paid.volunteer[currentdata[, 1] %in% volunteer.plots.grassland &
                               currentdata[, 6] %in% year &
                               currentdata[, 7] %in% month &
                               currentdata[, 8] %in% day] <- "volunteer"

    currentdata$costs[currentdata[, 1] %in% volunteer.plots.grassland & currentdata[, 6]
                  %in% year & currentdata[, 7] %in% month &
                    currentdata[, 8] %in% day] <- volunteer[7]

    if (num.paid.forest > 0) {
      currentdata <- PrepareSamplingEcologist (currentdata, "paid", "forest",
                                               paid[1], paid[2], paid[3], paid[4])
    }

    if (num.paid.forest != num.of.plots.forest) {
      currentdata <- PrepareSamplingEcologist (currentdata, "volunteer", "forest",
                                               volunteer[1], volunteer[2], volunteer[3], volunteer[4])
    }

    if (num.paid.farmland > 0) {
      currentdata <- PrepareSamplingEcologist (currentdata, "paid", "farmland",
                                               paid[1], paid[2], paid[3], paid[5])
    }

    if (num.paid.farmland != num.of.plots.farmland) {
      currentdata <- PrepareSamplingEcologist (currentdata, "volunteer", "farmland",
                                               volunteer[1], volunteer[2], volunteer[3], volunteer[5])
    }

    if (num.paid.grassland > 0) {
      currentdata <- PrepareSamplingEcologist (currentdata, "paid", "grassland",
                                               paid[1], paid[2], paid[3], paid[6])
    }

    if (num.paid.grassland != num.of.plots.grassland) {
      currentdata <- PrepareSamplingEcologist (currentdata, "volunteer", "grassland",
                                               volunteer[1], volunteer[2], volunteer[3], volunteer[6])
    }

  if (outputall) {
    return (currentdata)
  } else {
    currentdata <- currentdata[currentdata$paid.volunteer != 0, ]
    return (currentdata)
  }
}
