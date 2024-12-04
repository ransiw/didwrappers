#' County Teen Employment Dataset
#'
#' A dataset containing (the log of) teen employment in 500 counties in the U.S. from 2004 to 2007.
#' This is a subset of the dataset used in Callaway and Sant'Anna (2021). See that paper for additional descriptions.
#'
#' @format ## `mpdta`
#' A data frame with 2500 rows and 6 columns:
#' \describe{
#'   \item{year}{the year of the observation}
#'   \item{countyreal}{a unique identifier for a particular county}
#'   \item{lpop}{the log of 1000s of population for the county}
#'   \item{lemp}{the log of teen employment in the county}
#'   \item{first.treat}{the year that the state where the county is located raised its minimum wage, it is set equal to 0 for counties that have minimum wages equal to the federal minimum wage over the entire period}
#'   \item{treat}{whether or not a particular county is treated in that year}
#'   ...
#' }
#' @source <https://bcallaway11.github.io/did/>
"mpdta"

#' GDP and democratization data (1960-2010)
#'
#' A dataset containing the gross domestic product per capita for 175 countries from 1960 to 2010.
#' This is a subset of the dataset used in:
#'
#' Acemoglu, D., Naidu, S., Restrepo, P., & Robinson, J. A. (2019). Democracy does cause growth. Journal of political economy, 127(1), 47-100.
#'
#' @format ## `demgdp`
#' A data frame with 6934 rows and 7 columns:
#' \describe{
#'   \item{year}{the year of the observation}
#'   \item{country_name}{the country name}
#'   \item{wbcode}{a unique identifier for a particular country}
#'   \item{gdppercapitaconstant2000us}{the GDP per capita in US dollars}
#'   \item{dem}{the indicator for whether a country is a democracy (1) or not (0)}
#'   \item{YearFirstDemocracy}{the year that the country first becomes a democracy since 1960. If already a democracy this is 1960, and if not a democracy by 2010, this is missing}
#'   \item{breakdown}{indicator for if the country has reverted to an autocracy since democratizing in 1960}
#'   \item{regionnum}{region in numbers: Africa (1), East Asia (2), East. Europe (3), Industrialized (4), Latin America (5), Middle East (6), South Asia (7)}
#'   ...
#' }
#' @source <https://economics.mit.edu/people/faculty/daron-acemoglu/data-archive>
"demgdp"
