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

#' Simulated stochastic outbreak data
#'
#' Figure 2a data
#'
#' @format A data frame with 1108351 rows and 15 variables:
#' \describe{
#'   \item{risk_level}{risk level for simulation "High Risk" or "1.1"}
#'   \item{disc_prob}{reporting rate of simulation}
#'   \item{.id}{Simulation number used to separate individual outbreaks}
#'   \item{time}{.id specific time of outbreak simulation}
#'   \item{New_Exposed}{Newly exposed individuals}
#'   \item{New_Infection}{Newly infectious individuals}
#'   \item{Total_Infections}{Total number of infectious individuals}
#'   \item{Cumulative_Infections}{Cumulative number of infectious individuals}
#'   \item{New_Intro}{Newly imported (introduced) infectious cases}
#'   \item{Total_Intro_Infections}{Total number of imported (introduced) infectious cases}
#'   \item{Cumulative_Intro_Infections}{Cumulative number of imported (introduced) infectious cases}
#'   \item{New_Detections}{Newly reported (detected) cases}
#'   \item{Cum_Detections}{Cumulative number of detected cases for simulation}
#'   \item{New_Intro_Detections}{Newly reported (detected) cases that were initially imported}
#'   \item{Cum_Intro_Detections}{Cumulative reported (detected) cases that were initially imported}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1.1 or high risk group, intro rate = 0.01 or high risk group
"ex_outbreak_sims"

#' Prevalence by reported cases data
#'
#' Figure 2b data
#'
#' @format A dataframe with 404 rows and 6 variables:
#' \describe{
#'   \item{risk_level}{risk level for simulation "High Risk" or "1.1"}
#'   \item{disc_prob}{reporting rate of simulation}
#'   \item{detected}{Number of reported autochthonous cases}
#'   \item{median}{median autochthonous prevalence upon seeing x reported cases}
#'   \item{min}{minimum of 50% CI for prevalence}
#'   \item{max}{maximum of 50% CI for prevalence}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1.1 or high risk group, intro rate = 0.01 or high risk group
"prev_by_reported_data"

#' Prevalence by reported cases data
#'
#' Figure 2b data
#'
#' @format A dataframe with 404 rows and 6 variables:
#' \describe{
#'   \item{risk_level}{risk level for simulation "High Risk" or "1.1"}
#'   \item{disc_prob}{reporting rate of simulation}
#'   \item{detected}{Number of reported autochthonous cases}
#'   \item{value}{probability of an epidemic given you've detected x cases}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1.1 or high risk group, intro rate = 0.01 or high risk group
"epi_prob_by_reported_data"



