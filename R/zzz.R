.onAttach <- function (libname,pkgname)
    {
        packageStartupMessage("Welcome to rSARP!  While you are reading this message two data files setting the 'terrain and weather' factors are being read.  See the package Vignette command for more information on how to edit these files and what they do." )
        
  }
        
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Terrains.csv - typical Data input file for \pkg{rSARP}
#' 
#' A dataset containing the terrains and key attributes of each
#' that are relevant to search. The variables are as follows:
#'
#' \itemize{
#'   \item Terrain. Name of the Terrain described.  Usually string descriptions that are easy to remember.
#'   \item Hours. Relative man hours of search required for the terrain. 
#'   \item AMDR. Average maximum detection range that is typical for the Terrain. 
#'   \item Speed.  Expected travel speed through the terrain in miles/hr.
#' }
#'
#' @format A data frame with 7 rows and 4 variables
#' @source John Hutcheson
#' @name Terrains.csv
NULL
        
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Weathers.csv - typical Data input file for \pkg{rSARP}
#' 
#' A dataset containing the Weather conditions that affect search and the speed impacts.
#' The variables are as follows:
#'
#' \itemize{
#'   \item Weather. Name of the Weather described.  Usually string descriptions that are easy to remember and descriptive of conditions.
#'   \item Speed.  Expected travel speed through the Weather in miles/hr.
#' }
#'
#' @format A data frame with 6 rows and 2 variables
#' @source John Hutcheson
#' @name Weathers.csv
NULL
        
