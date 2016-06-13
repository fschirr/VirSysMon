#' Rewrite the dataset into a usable format.
#'
#' \code{PrepareDataset} rewrites the dataset to make it useable for the
#' Sampling function of this package. The function comibines the different
#' columns and creates a new dataset. If there are now data from a column the
#' Column will be filled with zeros.
#'
#' @param data The input dataset.
#' @param plot Position of the row which contains the plot or cell IDs
#' @param num.of.individuals Position of the row which contains the number of individuals
#' @param species Position of the row which contains the species names
#' @param x.coord Position of the row which contains the x coordinates
#' @param y.coord Position of the row which contains the y coordinates
#' @param visit.year Position of the row which contains the year date
#' @param visit.month Position of the row which contains the month
#' @param visit.day Position of the row which contains the day
#' @param habitat The habitat of the plot
#'
#' @return A data frame with a least 10 rows Which are in an certain order.
#' @export
#'

PrepareDataset <- function(data, plot, num.of.individuals, species, x.coord,
                           y.coord, visit.year, visit.month, visit.day, habitat) {

  column.plot <- ifelse (plot > 0, data[plot], rep(0, length(data[, 1])))
  column.num.of.individuals <- ifelse (num.of.individuals > 0,
                                       data[num.of.individuals],
                                       rep(0, length(data[, 1])))
  column.species <- ifelse (species > 0, data[species], rep(0,
                                                            length(data[, 1])))
  column.x.coord <- ifelse (x.coord > 0, data[x.coord], rep(0,
                                                            length(data[, 1])))
  column.y.coord <- ifelse (y.coord > 0, data[y.coord], rep(0,
                                                            length(data[, 1])))
  column.visit.year <- ifelse (visit.year > 0, data[visit.year],
                               rep(0, length(data[, 1])))
  column.visit.month <- ifelse (visit.month > 0,data[visit.month],
                                rep(0, length(data[, 1])))
  column.visit.day <- ifelse (visit.day > 0, data[visit.day],
                              rep(0, length(data[, 1])))
  column.habitat <- ifelse (habitat > 0, data[habitat],
                              rep(0, length(data[, 1])))


  data <- data.frame (column.plot, column.num.of.individuals, column.species,
                      column.x.coord, column.y.coord, column.visit.year,
                      column.visit.month, column.visit.day, column.habitat)

  colnames (data) <- c("plot", "num.of.individuals", "species",
                       "x.coord", "y.coord", "year", "month", "day", "habitat")

  return (data)
}

#' Creates a vector with the behaviour of an ecologist.
#'
#' \code{CreateEcologist} creates a vector with characteristics of the behaviour
#' of an ecologist in the field. The characteristics are: The sampled area of a
#' plot, the detection probability, the identification error, the probability
#' of missed vistis and the costs. The first four characteristics are expected
#' to be in percent.
#'
#' @param sampling.area The sampled area of a cell in percent.
#' @param detection.probability A number in percent.
#' @param identification.error A number in percent.
#' @param probability.missed.visits.forest A number in percent.
#' @param probability.missed.visits.farmland A number in percent.
#' @param probability.missed.visits.grassland A number in percent.
#' @param costs A number in monetary units.
#'
#' @return A vector with the five characteristics of an ecologist.
#' @export
#'


CreateEcologist <- function (sampling.area, detection.probability,
                             identification.error, probability.missed.visits.forest,
                             probability.missed.visits.farmland,
                             probability.missed.visits.grassland, costs) {

  ecologist <- c(sampling.area, detection.probability, identification.error,
                 probability.missed.visits.forest, probability.missed.visits.farmland,
                 probability.missed.visits.grassland, costs)

  return (ecologist)
}
