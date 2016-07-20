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
#' Figure 2b data - Gives the median and 50% CI prevalence across all simulations that reach x detected cases.
#'
#' @format A dataframe with 404 rows and 6 variables:
#' \describe{
#'   \item{risk_level}{risk level for simulation "High Risk" or "1.1"}
#'   \item{disc_prob}{reporting rate of simulation}
#'   \item{detected}{Number of reported autochthonous cases}
#'   \item{median}{median autochthonous prevalence upon seeing x reported cases}
#'   \item{min}{minimum of 50\% CI for prevalence}
#'   \item{max}{maximum of 50\% CI for prevalence}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1.1 or high risk group, intro rate = 0.01 or high risk group
"prev_by_reported_data"

#' Epidemic probability by reported cases data
#'
#' Figure 2c data - Gives the probability of an impending epidemic across all simulations that reach x detected cases.
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

#' County Risk spreadsheet
#'
#' Data for importation/transmission risk for all counties
#'
#' @format A dataframe with 254 rows and 24 variables:
#' \describe{
#'   \item{Metro}{Metro area that county is associated with (integer)-not used}
#'   \item{id}{Texas County ID}
#'   \item{Geography}{County name format 'county_name "County, Texas"'}
#'   \item{GDP}{County GDP}
#'   \item{importation_probability}{Probability that next imported case occurs in that county}
#'   \item{importation.current}{Importation rate Quarter 1 for county}
#'   \item{importation.projected}{Projected county importation rate for Quarter 3}
#'   \item{importation.worse.projected}{Worst case county importation projection for Q3}
#'   \item{mosquito.abundance}{Mosquito abundance for county from Kraemer 2015 global distribution data}
#'   \item{rnott.expected.round}{Expected august r0 for county}
#'   \item{low.round}{Low GDP effect r0 for august}
#'   \item{high.round}{High GDP effect r0 for august}
#'   \item{hetero.round}{Heterogeneous GDP effect for r0 for august}
#'   \item{temperature}{Historic average county temperature for August}
#'   \item{eip}{Extrinsic incubation period for mosquitoes at average temperature}
#'   ...
#' }
#' @source Zika epidemic simulations with R0=1.1 or high risk group, intro rate = 0.01 or high risk group
"county_risk_data"

#' Texas Major city location
#'
#' Data for location of major texas cities
#'
#' @format A dataframe with 15 rows and 4 variables:
#' \describe{
#'   \item{ID}{Texas city ID number}
#'   \item{lat}{latitude of city}
#'   \item{lon}{longitude of city}
#'   \item{Name}{Name of city}
#'   ...
#' }
#' @source google maps
"major_city_loc"

#' Texas county map
#'
#' Spatial data for all counties in Texas, can be fortified to turn into dataframe
#'
#' @format A spatial dataset for all 254 Texas counties
#' \describe{
#' \item{counties}{Texas shapefile object, used to plot maps}
#' }
#' @source online
"texas_county_shp"

#' County Detection probability
#'
#' Figure 4a data - probability of observing x detected cases in county
#'
#' @format A dataframe with 1224 rows and 5 variables:
#' \describe{
#'   \item{r_not}{Texas county r0}
#'   \item{disc_prob}{daily probability of being reported in simulation}
#'   \item{intro_rate}{daily rate of introductions for simulation}
#'   \item{detected}{Number of autochthonous reported cases}
#'   \item{prob_detect}{probability detecting the number of detected cases}
#'   ...
#' }
#' @source county simulation data
"county_prob_detect_x"


#' Epidemic probability for counties
#'
#' Figure 4b data
#'
#' @format A dataframe with 1332 rows and 5 variables:
#' \describe{
#'   \item{r_not}{Texas county r0}
#'   \item{disc_prob}{daily probability of being reported in simulation}
#'   \item{intro_rate}{daily rate of introductions for simulation}
#'   \item{detected}{Number of autochthonous reported cases}
#'   \item{prob_epidemic}{probability of an epidemic give the number of reported cases}
#'   ...
#' }
#' @source county simulation data
"county_epi_prob_by_d"

#' County surveillance triggers
#'
#' Figure 4c data - number of reported cases necessary for each county to have 50% probability of an epidemic
#'
#' @format A dataframe with 149 rows and 9 variables:
#' \describe{
#'   \item{r_not}{Texas county r0}
#'   \item{disc_prob}{daily probability of being reported in simulation}
#'   \item{intro_rate}{daily rate of introductions for simulation}
#'   \item{prev_threshold}{threshold for prevalence analysis -- not used anymore}
#'   \item{epi_threshold}{prevalence threshold to define epidemic}
#'   \item{confidence}{threshold probability for triggers}
#'   \item{num_necessary}{Number of simulations necessary to reach certain number of cases to have trigger}
#'   \item{epi_trigger}{Trigger for reported cases necssary to have 50\% probability of epidemic}
#'   \item{prev_trigger}{Trigger for prevalence -- no longer used}
#'   ...
#' }
#' @source county simulation data 50% probability of an epidemic
"exp_triggers_data"


