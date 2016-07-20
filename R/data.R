#' Dying outbreak example
#'
#' Figure 1 data
#'
#' @format A long format data frame with 172 rows and 3 variables:
#' \describe{
#'   \item{time}{day of outbreak simulation}
#'   \item{variable}{specifies asymptomatic/symptomatic}
#'   \item{value}{number of symptomatic versus asymptomatic individuals}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1, reporting rate=0.011, intro rate = 0.1, 15% symptomatic
"ex_dying_epidemic_data"

#' Growing outbreak example
#'
#' Figure 1 data
#'
#' @format A long format data frame with 220 rows and 3 variables:
#' \describe{
#'   \item{time}{day of outbreak simulation}
#'   \item{variable}{specifies asymptomatic/symptomatic}
#'   \item{value}{number of symptomatic versus asymptomatic individuals}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1, reporting rate=0.011, intro rate = 0.1, 15% symptomatic
"ex_growing_epidemic_data"

#' Dying outbreak arrow data
#'
#' Figure 1 data
#'
#' @format A long format data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{time}{day of outbreak simulation}
#'   \item{yval}{yvalue for number total infectious at the given time}
#'   \item{type}{either import or report to determine arrows and lines}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1, reporting rate=0.011, intro rate = 0.1, 15% symptomatic
"dying_arrows"

#' Growing outbreak arrow data
#'
#' Figure 1 data
#'
#' @format A long format data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{time}{day of outbreak simulation}
#'   \item{yval}{y value for number total infectious at the given time}
#'   \item{type}{either import or report to determine arrows and lines}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1, reporting rate=0.011, intro rate = 0.1, 15% symptomatic
"growing_arrows"


