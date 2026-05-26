#' Hurricane names and damage data
#'
#' A data set used to study the association between hurricane name femininity
#' and hurricane fatalities.
#'
#' @format A data frame with 94 rows and 13 variables:
#' \describe{
#'   \item{year}{Year of the hurricane.}
#'   \item{name}{Hurricane name.}
#'   \item{masfem}{Name femininity score, from 1 masculine to 11 feminine.}
#'   \item{min}{Minimum pressure.}
#'   \item{gender_mf}{Name gender, male or female.}
#'   \item{category}{Hurricane category.}
#'   \item{alldeaths}{Total deaths.}
#'   \item{ndam}{Normalized damage amount.}
#'   \item{elapsedyrs}{Years elapsed since the hurricane.}
#'   \item{source}{Data source.}
#'   \item{masfem_mturk}{Name femininity score from MTurk ratings.}
#'   \item{wind}{Highest wind speed.}
#'   \item{ndam15}{Damage amount normalized to 2015.}
#' }
#' @source Original analysis from Jung et al. (2014),
#' \url{https://www.pnas.org/content/111/24/8782}.
"hurricane"

#' Pima Indians diabetes data
#'
#' A data set of diabetes outcomes and clinical predictors for women from the
#' Pima Indian population.
#'
#' @format A data frame with 768 rows and 8 variables:
#' \describe{
#'   \item{npreg}{Number of pregnancies.}
#'   \item{glucose}{Plasma glucose concentration from an oral glucose tolerance test.}
#'   \item{pressure}{Diastolic blood pressure in mm Hg.}
#'   \item{skin}{Triceps skin fold thickness in mm.}
#'   \item{bmi}{Body mass index.}
#'   \item{pedigree}{Diabetes pedigree function.}
#'   \item{age}{Age in years.}
#'   \item{diabetes}{Diabetes diagnosis, with levels \code{neg} and \code{pos}.}
#' }
#' @references
#' Smith, J. W., Everhart, J. E., Dickson, W. C., Knowler, W. C.,
#' and Johannes, R. S. (1988). Using the ADAP learning algorithm to forecast
#' the onset of diabetes mellitus. In Proceedings of the Symposium on Computer
#' Applications in Medical Care.
"pimads"
