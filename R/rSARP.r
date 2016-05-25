                                        #rSARP
# revised bestsearch code to permit FTt values input by user to be blown into the graphs.  Bug ignored user input.
# added function to report out progress on 525s graphically
# added warning to the bestsearch chart to ignore red line when it crosses the AMDR line
                                        #Copyright 2014 by John F. Hutcheson, all rights reserved.
#vers 20160312jfh find and update vers string below as well
# Revised string placement on Bestsearch graphs so readable.
# Major Rewrite. Returned working directory control to R.  Added feature to track % of plan coverage - if 10 hectares planned but only half covered adjusts the displayed bars and targeted POS. Set up functions to be exported to package.
# fixed bug in bestsearch() that kept special directories from being created
# added .csv to sens file names so these could be easily inspected and set up plot to be visible in R display
# changed bestsearch() algorith to incorporate more R structures and controls
# added function to run FtT repeately based on limited number of subjects called bestsearch()
# fixed bug that would cause program to crash if no sectors were labeled as sectors not planned for search.
# changed searchme inputs to permit easy optimization and created optimization rourtine for pos as function of fixed headcount; changed 0 to -1 inputs for computer calcs so 0 is valid state
# added snow as WX variable in form of 6snow8 for 6 to 8 inches of snow
# revised sorting algorithms for the SearchOut file and added columns to create running sums for heads and POS values, added option for windows7 users to use 32 bit version of R
# fixed bug that kept searchstatus from going to the proper directory and added option to indicate sector was no longer actively being searched through POScum value
# attempt to speed up graphics portion by rolling plot objects into mapply functions and added ability to shut graphs off
# added FtT function and input option to move from Fast to Thorough by adjusting this number
# at the input option.  Also added OPOScum column to simplify the 'processing' of POScum figures
# The user inputs POScum to represent the previous cumulative POS from the last search.  OPOScum becomes the cumulative POS after the plan is executed
# modified program to include POScum calculations, which account for repetitive searches of a sector and the cumulative impact on POS
# added antipod function to calc COV from POD used in calculating POScum
# created function to give status reports
# modified terspd and terheads to be calculated from THours,TSearchers,TSpacing values if given
# simplified code to speed it up
# added POC (ranking from consensus) and calculation showing POS impact by varying searcher count around planned Searchers to each segment
# revised hour estimate to include TOD as factor after Terrain is used if estimated on Terrain alone.  Added scheme to calculate TSearcher if given 0
#revised initial AMDR and Spacing for Terrain based estimates of Urban, Open Area, and other terrain types to bring projected performance to the Rule of thumb line and changed color of the lines on graphs for Phil
# Add initial values for the theoretical AMDR and starting separation distance based on the terrain used as starter values if these fields are set to zero by adding the amdrestimator function and the tspacingestimator function. Also change the THours calculuation to be based on a set speed given the terrain.  This required the addition of a geocons function to calculate a new speed constant for use in the NHRHS function
# Add the following - ability to specify Urban as new terrain, and have the program calculate the time expected if no hours are provided 
# added area indicator to the graphic outputs and spread sheet output
# added option to switch between acres and hectares with default in hectares for area of search
# revised speed spacing graph to display the expected hours (horizontal red line)
# revised models to fit on one page - removing Kent Model from prominence and elevating NASAR models and lower speeds of search based on Isabella search data
# thickended reference lines in all plots which were obscured by grids
# Added panel.curve plots to all I could manage and replaced the xyplots
# Fixed titles in graphs and added grids to key graphs
# Fix bug in GEOM brought in during trellis revisions
# change all graphs over to trellis
# add graph of speed lines (set) as function of spacing and Time using phys modl
# fixed sect error so print sect is correctly reported between POD and Sectors
# added version data printed out into pdf file margins
# This code is the property of John Hutcheson and is not to be sold commercially or used without permission of the Board of Directors
# This code is a beta version of an R program designed to take a CSV file descriping a prospective search by sectors and lead planners and search managers through the process of selecting the proper team makeup and expected outcomes (Search Duration, number of searchers required, spacing, and ultimately POD)
# This program is a model and will not provide answers - only estimates
# The user assumes all responsibilities for the use of this model
# Change log - added ability to rename the output file to any name by passing the searchme function the name
 if(getRversion() >= "2.15.1")  utils::globalVariables("x")

# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @author John F. Hutcheson
##### @aliases terrainnr
##### @param terrain - text string that describes the terrain of the sector considered
##### @param terraindf - dataframe of the terrains and constants associated with those terrains
##### @return number that represents the relative effort to search a sector in man hours
##### @details fucntion passed a string which describes a sector's terrain and returns a numeric value that gauges the effort required to search a sector
##### @title terrainnr function 
##### @description This function takes in the terrain and returns a numeric value based on the rules of thumb to estimate the hours of search effort required
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
terrainnr <- function(terrain, terraindf)
{   ter <- terraindf[terraindf[,1]==terrain,2]
    return(ter)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## # @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title AMDREstimator
##### @aliases amdrestimator AMDRestimator AMDREstimator names
##### @description This function estimates the AMDR in meters based solely on the terrain given to describe the area
##### @details Function takes in the terrain and returns a double
##### @author John F. Hutcheson
##### @param terrain - string description of the terrain.
##### @param terraindf - dataframe of the terrains and constants associated with those terrains
##### @return real number average maximum detection range (AMDR)
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
amdrestimator <- function(terdf, terraindf)
{   tervalues <- (terraindf[terraindf[,1]==terdf,3])
    return(tervalues)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title tspacingestimator
##### @aliases tspacingestimator
##### @description Function takes in the AMDR of a sector and the FtT and returns an estimate for the spacing between searchers
##### @details Function is multiplies the FtT times the AMDR and returns that number rounded to the tenths placef detailed info about function
##### @author John F. Hutcheson
##### @param amdr - amdr of the sector in meters
##### @param ftt - Fast to Thorough parameter - sets the relative hastiness of the search 1 = very fast, 5 = very thorough
##### @return spacingsrchr - spacing between searchers in meters rounded to the tenth of a meter
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
tspacingestimator <- function(amdr,ftt)
  # estimates the TSpacing based on the AMDR given to describe the area and assumes the broadest spacing possible and still maintain 63% POD
  { spacingsrchr <- ftt* amdr
    round(spacingsrchr,digits=1)
    return(spacingsrchr)
  }

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title weathernr
##### @aliases weathernr
##### @description Adjusts the speed of search based on a string value that represents weather
##### @details This function adjusts the speed of search based on the impact of weather.  Returned values from the function which are negative slow the search by a commensurage speed in mph while positive numbers represent average increased speeds in mph
##### @author John F. Hutcheson
##### @param wx - string value that represents weather. Currently this variable for the most part has no effect.
##### @param weatherdf - weather dataframe - contains weathers of interest and associated constants
##### @return wea - Function returns a real value that represents the impact of weather as a change in the average speed in mph
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
weathernr <- function(wx, weatherdf)
{  wea <- weatherdf[weatherdf[,1]==wx,2]
   return(wea)
}  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# impact on hours of searching in daylight or darkness
# for ease of calculation this is just a doubling of daylight Thour estimates but could eventually be calculated another way
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title timeofdaynr
##### @aliases timeofdaynr 
##### @description timeofdaynr returns a factor used to adjust search hours based on the time of day the search is conducted
##### @details timeofdaynr is passed a string that represents the time of day (currently "Day" or "Night") and returns an integer that is used to determine the hours required for the search to be conducted
##### @author John F. Hutcheson
##### @param tod - String that represents the time of day ("Day" or "Night")
##### @return Returns an integer, one or two 
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
timeofdaynr <- function(tod)
  { switch(tod, Day=1, Night=2)
  }

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title thumbcount - calculate search hours using rule of thumb
##### @aliases thumbcount
##### @description this function takes in the area units, area, terrain, and time of day (TOD) and calculates the manhours to search a sector.
##### @details this function takes in the area, area units, terrain, and time of day (TOD) and calculates the searcher*hours based on a rule of thumb. This is used to build a chart relating searchers used to hours to complete the search.
##### @author John F. Hutcheson
##### @param area - sector area in area units
##### @param AUC - area unit factor - a number representing hectares or acres
##### @param terrain - string describing the terrain (ex: "OpenWoods")
##### @param weather - string representing the weather (ex: "Snow6to8in")
##### @param time_of_day - string representing the time of day ("Day" or "Night")
##### @return searchhrs - searcher man hours (headcount * hours) for a search as a double
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
thumbcount <- function (area,AUC, ternr, time_of_day)
{  # calculate the hours based on the rules of thumb
  todnr <- timeofdaynr(time_of_day)
  hrs <- (ternr)*todnr*area/160*32*AUC
  searchhrs <- as.double(hrs)
  return(searchhrs)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# this function calculates the Spacing(meters)*Speed(mi/hr) given the Area, TSearchers and THours for the search
# Divide the SpacingSpeed number either by spacing between searchrs or by speed to get the Speed or spacing
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title GEOM - calculates Spacing*Speed for each sector
##### @description GEOM calculates the Spacing(meters)*Speed(mi/hr) given the Area, TSearchers and THours for the search. Divide the SpacingSpeed number ## either by spacing between searchrs or by speed to get the Speed or spacing
##### @details GEOM calculates the Spacing*Speed number for a sector given the area, area units, searchers, hours and a geo constant.
##### @author John F. Hutcheson
##### @param area - area of a sector
##### @param AUC - the area unit constant which allows use of either acres or hectares
##### @param searchers - head count of the number of searchers planned for the sector
##### @param hours - the duration of the search in hours for that sector
##### @param geoconstant - a factor which translates speed from meters/hour to miles per hour
##### @return spdspace - A number representing the speed (mph) * spacing (m) used to generate graphs of each sector
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
GEOM <- function(area,AUC, searchers, hours=1, geoconstant=.397675164)
  { if (is.na(area) || is.na(searchers) || is.na(hours))
        {stop("Error - some of the data is missing!  Please check the input data")
     }
    # input data checks complete - ready to calculate
     spdspace <- area/geoconstant/hours/searchers*AUC
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title GEOM2 - calculates hrs*speed
##### @description GEOM2 calculates the Hours*Speed for each sector and returns a data frame containing Hours*Speed and the spacing for a sector
##### @details GEOM2 calculates the Hours*Speed number for a sector given the area, area units, searchers, spacing and a geo constant.
##### @author John F. Hutcheson
##### @param area - area of a sector
##### @param AUC - the area unit constant which allows use of either acres or hectares
##### @param searchers - head count of the number of searchers planned for the sector
##### @param spc - the spacing between searchers in meters for that sector
##### @param geoconstant - a factor which translates speed from meters/hour to miles per hour
##### @return hrsdped - A dataframe containg the Hours*Speed in the first column and the Spacing in the second.
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
GEOM2 <- function(area,AUC, searchers, spc, geoconstant=.397675164)
  { if (is.na(area) || is.na(searchers) )
        {stop("Error - some of the data is missing!  Please check the input data")
      }
    # input data checks complete - ready to calculate
     hrsped=data.frame()   # creates a list to store the output
     speod <- data.frame(spc)    # speod is TSpacing in meters
     hrsped<- area/geoconstant/searchers/speod*AUC              # flag
     hrsped[,2] <- data.frame(spc)
     colnames(hrsped) <- c("spdhrs","xspcg")
     return(hrsped)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This function calculates Hrs*Searchers using the geom model and incorporates the TOD affect as a 2x factor
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title GEOM3 - calculates Searchers*Hrs for each sector
##### @description GEOM3 calculates the Searchers*Hours given the Area, Spacing and Speed, Time of Day and a geo constant for the search. 
##### @details GEOM calculates the Spacing*Speed number for a sector given the area, area units, searchers, hours and a geo constant.
##### @author John F. Hutcheson
##### @param area - area of a sector
##### @param AUC - the area unit constant which allows use of either acres or hectares
##### @param searchers - head count of the number of searchers planned for the sector
##### @param hours - the duration of the search in hours for that sector
##### @param geoconstant - a factor which translates speed from meters/hour to miles per hour
##### @return spdspace - A number representing the speed (mph) * spacing (m) used to generate graphs of each sector
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
GEOM3 <- function(area,AUC,spcg,spd,TOD,geoc=.39765164)
  { tdf<-timeofdaynr(TOD)
    srchrhrs<-area/spd/geoc*AUC/spcg*tdf
    return(srchrhrs)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title GEOM4 - calculates speed (mph) for each sector
##### @description GEOM4 calculates the speed given the Area, hours, count of searchers, searcher spacing, and a geo constant for the search. 
##### @details GEOM4 calculates the Speed(mph) for a sector given the area, area units, count of searchers, man hours, and a geo constant.
##### @author John F. Hutcheson
##### @param area - area of a sector
##### @param AUC - the area unit constant which allows use of either acres or hectares
##### @param searchers - head count of the number of searchers planned for the sector
##### @param hours - the duration of the search in hours for that sector
##### @param geoconstant - a factor which translates speed from meters/hour to miles per hour
##### @return spdspace - A number representing the speed (mph) * spacing (m) used to generate graphs of each sector
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
GEOM4 <- function(area,AUC,hrs,srchrs,spcg,geoc=.39765164)
  {  spd<-area/hrs/srchrs/geoc/spcg*AUC
     return(spd)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This function calculates the impact of adding one searcher to planned headcount (TSearchers) to assess the additional time saved
# The function is the first derivative of the geometric equation
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##### @title SAVHRS
##### @aliases SAVHRS
##### @description SAVHRS - Function calculates the savings in hours that results from adding an additional person to the search line
##### @details SAVHRS - Function returns the savings in hours from adding an additional person to the search team and "growing the line".  Passed the area, area units, number of searcherss, spacing(m) and a geoconstant.
##### @author John F. Hutcheson
##### @param area - area of the sector in hectares or acres
##### @param AUC - Area Unit Conversion factor - converts units based on the area unit chosen
##### @param searchers - head count of the number of searchers in the line
##### @param spc - spacing between searchers in meters
##### @param geoconstant - conversion factor units used
##### @return savhrs - hours saved by lengthening the line of search by 1 person and keeping the same spacing
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SAVHRS <- function(area,AUC,searchers,spc,geoconstant=.397675164,speeed)
  {  if (is.na(area) || is.na(searchers) || is.na(speeed))
         { stop("Error - data is missing for the SAVHRS calculation (area/searchers/speed).  Please check input data")
       }
     savhrs<-area/geoconstant/(searchers)**2/speeed*AUC/spc
     return(savhrs)
 }
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
##### this fucnction extends one dataframe with another by dividing the df that is 2x7 and adding a column and exending it by 11 rows for
##### every row in the multiplier column. This function used to create the durhrs variable for plots of hours by spacing for each sector plot
##### @author John F. Hutcheson
##### @aliases XTNDR
##### @param dtfrm - dataframe that contains spdhrs and xspacing for the sector
##### @return ndtfrm - dataframe that contains the dtfrm table expanded to include 5 rows for every original row and spd and durhrs columns
##### @details if dtfrm contains a first row of c1 c2 the first row of the returned df will contain c1 c2 2 c1/2 and the next 4 rows will repeat the first two columns with 1 c1/1 in row 2, .5 c1/.5 in row 3, .29 c1/.29 in row 4 and .2 c1/.2 in row five.  This is repeated for each row in dtfrm.
XTNDR <- function(dtfrm)
  { frmlength <- nrow(dtfrm)    # rows of dataframe - this varies
    ndtfrm <- data.frame()     # creates temp dfs to store stuff in
    ndtfrm<-rbind(ndtfrm,dtfrm)      #  create new df ouf of dtfrm by repeating the rows 5 times
    ndtfrm<-rbind(ndtfrm,ndtfrm)     # now 2x in length of repeating rows
    ndtfrm<-rbind(ndtfrm,ndtfrm)     # now 4x in length of repeating rows
    ndtfrm<-rbind(ndtfrm,dtfrm)      # now 5x long
    spdcol<-rep(c(2,1,.5,.29,.2),each=frmlength)           # add in the speeds
    ndtfrm[3]<-spdcol
    ndtfrm[4] <- ndtfrm[,1]/ndtfrm[,3]   # this is durhrs
    colnames(ndtfrm) <- c("spdhrs","xspcg","spd","durhrs")
    return(ndtfrm)
}

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### this function calculates the Hours*Spacing using the NASAR grid planning model which assumes .29 mph search speed
#### since speed is constant in NASAR model, spacing determines the hrs to search
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title NHRS - NASAR Hours*Spacing 
#### @aliases NHRS
#### @description NHRS estimates the hours required to search using a fixed speed of .29 mph - NASAR's proposed speed if nothing else is known
#### @details NHRS is a function that provides the Hours*Spacing constant for a search sector using NASAR's worst case speed and given the area, area units, number of searchers, and a geoconstant to convert units
#### @author John F. Hutcheson
#### @param area - area of the sector in acres or hectares
#### @param AUC - Area Unit Conversion - factor for converting area units from hectares or acres
#### @param searchers - head count of the number of searchers assigned to the sector
#### @param geoconstant - unit conversion factor
#### @return hrspace - a constant representing the hours * spacing(m) to complete the search
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
NHRS <- function(area, AUC, searchers, geoconstant=2.5146)
  {  hrspace <- geoconstant*area/searchers/.29*AUC
 }

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### this function calculates the Hours*Searchers using the NASAR grid planning model which assumes .29 mph search speed, the given spacing and area of the search
#### since speed is constant in NASAR model, spacing determines the hrs to search
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title NHRSH - NASAR Hours*Searchers 
#### @aliases NHRSH
#### @description NHRSH estimates the hours * searchers required to search using a fixed speed of .29 mph - NASAR's proposed speed if nothing else is known
#### @details NHRSH is a function that provides the Hours*Searcher headcount that is constant for a search sector using NASAR's worst case speed and given the area, area units, spacing(m), and a geoconstant to convert units.  Assumes a speed of .29mph in the search
#### @author John F. Hutcheson
#### @param area - area of the sector in acres or hectares
#### @param AUC - Area Unit Conversion - factor for converting area units from hectares or acres
#### @param spacing - the spacing between searchers in meters
#### @param geoconstant - unit conversion factor
#### @return hrsheads - a constant representing the hours * count of searchers to complete the search
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
NHRSH <- function(area,AUC, spacing, geoconstant=2.5146)
  {  hrsheads <- geoconstant*area/spacing/.29*AUC
  }
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title terspeed - terrain speed
#### @aliases terspeed
#### @description terspeed - caluculates the terrain speed (mph) expected for a team traversing a sector with a given terrain in the described weather
#### @details terspeed - estimates search speed(mph) of a search of a sector with a given terrain or environment in a given weather condition
#### @author John F. Hutcheson
#### @param terrain - string describing the sector to be searched
#### @param terraindf - dataframe describing the terrains and associated constants
#### @return gspeed - ground speed of the search team in mph 
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
terspeed<- function(terrain, terraindf)
  {    #  gspeed is the ground speed in mph estimated based on the terrain
      gspeed <- terraindf[terraindf[,1]==terrain,4]
      return(gspeed)
  }
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title pod - probability of detection
#### @aliases pod
#### @description pod - calculates the probability of detection given the coverage of the search
#### @details pod - calculates the probabilty of dectection given the coverage of the search.
#### @author John F. Hutcheson
#### @param covg - search coverage (ratio of spacing to the AMDR)
#### @return pod - probability of detection as a fraction
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### This function calculates the POD given the coverage
pod <- function(covg)
  { 1-exp(-covg)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title anitpod - calculates Coverage from POD
#### @aliases antipod
#### @description antipod - calculates the coverage of the search given the probability of detection
#### @details pod - calculates the coverage given the probability of detection of the search
#### @author John F. Hutcheson
#### @param pod - probability of detection as a fraction
#### @return covg - search coverage (ratio of spacing to the AMDR)
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### This function calculates the COV  given the pod
antipod <- function(pod)
  {  -log(1-pod)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title rAMDR - calculates average AMDR required to make the a  Fast Search worth doing
#### @aliases rAMDR
#### @description rAMDR - calculates the Average AMDR required to make the a Fast Search equivalent in POS value to a thorough search.
#### @details rAMDR - calculates the Average AMDR needed to yield an equivalent POS to a thorough search. This is equivalent to saying "how much must the ## visually based AMDRs be inflated to make a faster search method provide an equivalent chance of success to a more thorough method? This would result  from subjects being responsive to audio calls, for instance. Based on the maximum POS obtained in thorough searches.
#### @author John F. Hutcheson
#### @param poc - probability of containment as a sum across all segments for the best search run
#### @param pos - probability of success as a running total for this best search run
#### @param spacing - average spacing for the fast search plan
#### @return iAMDR - averaage inflated AMDR needed to reach POS similar to the POS of best search result
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### This function calculates the COV  given the pod
rAMDR <- function(poc,pos,spacing)
  { -1*(log((poc - pos)/poc))*spacing/1.5
  }

# -----------------------------------------------------------------
#### This function translates an FtT number to a parameter the program can use to calculate POD and doesn't allow the paramter to go out of bounds
#### This function translates an FtT of 3 to mean a spacing of 1.5 x AMDR while an FtT of 5 results in a spacing of .66 * AMDR
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title Fasttothorough - speed of search parameter
#### @aliases Fasttothorough
#### @description Fasttothorough - calculates a spacing factor to adjust the speed or thoroughness of search
#### @details Fasttothorough - calculates a spacing factor to adjust the spacing of a search to parametriclly adjust the speed or thoroughness of a search. A setting below 3 has much wider spacing (and is therefore much faster) while a setting above 3 is much slower and more thorough.  The range should be restricted between 1 and 5.
#### @author John F. Hutcheson
#### @param ftt - fast to thorough parameter as a real number
#### @return ftt - fast to thorough paramter as real number adjusted to impact team spacing in a meaningful way
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Fasttothorough<-function(ftt)
  { if (ftt<=1)
        {     #   Go fast as possible
            ftt<-5/ftt       #  results in infinity at low end and 5 at high
        }
    else if(ftt<=2 && ftt>1)
        {
            ftt<-5/ftt            # results in 5 at low end and 2.5 at high end
        }
    else if(ftt<=3 && ftt>2)
        {
            ftt<-4.5/ftt           # results in 1.5 when value is 3 - center normal value for a target .63 POD
        }
    else
        {
            ftt<-4.6/(-3+2*ftt)    # results in 1.5 at low end and .66 at 5
        }
    return(ftt)
}

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title hutchpdf - determines which OS program is being run on and sets up a print device for a pdf file to be created
#### @description hutchpdf sets up the correct printing environment for each OS and sets up a print device for a pdf file
#### @details hutchpdf sets up the correct printing environment for each OS and creates a pdf file when grDevices::dev.off() is encountered after this program is run
#### @author John F. Hutcheson
#### @param platfrm - platform the program is being run on
#### @param filenm - name of the file the plot will be stored as
#### @return intgr - integer indicating which platform the program is being run on and which format is used for storage
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
hutchpdf <- function(platfrm, filenm)
    {  if (is.element(platfrm,c('i386-pc-mingw32')))
           {                # script is running on windows
                   # starts the graphical device driver for windows
               grDevices::pdf(file=filenm,width=8,height=10)
               intgr <- 0
           }
       else if (is.element(platfrm,c('x86_64-w64-mingw32')))               # PC is Windows 7 pc
           {     # set file location for input and output files in the new working directory
               grDevices::pdf(file=filenm,width=8,height=10)
               intgr <- 2
           }
       else if (is.element(platfrm,c('i386-w64-mingw32')))               # PC is Windows 7 pc 32 bit
           {     # set file location for input and output files in the new working directory
                                        # starts the graphical device driver for windows
               grDevices::pdf(file=filenm,width=8,height=10)
               intgr <- 3
           }
       else
           { # works for Mac OS X - not sure about linux / others...
               grDevices::pdf(file=filenm,width= 8,height= 10)
               intgr <- 1
           }
       return(intgr)
   }

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title hutchpng - determines which OS program is being run on and sets up a print device for a png file to be created
#### @description hutchpng sets up the correct printing environment for each OS and sets up a print device for a png file
#### @details hutchpng sets up the correct printing environment for each OS and creates a png file when grDevices::dev.off() is encountered after this program is run
#### @author John F. Hutcheson
#### @param platfrm - platform the program is being run on
#### @param filenm - name of the file the plot will be stored as
#### @return intgr - integer indicating which platform the program is being run on and which format is used for storage
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
hutchpng <- function(platfrm,filenm)
    { if (is.element(platfrm,c('i386-pc-mingw32')))
          {                # script is running on windows
                                        # starts the graphical device driver for windows
              grDevices::png(filename=filenm, units="in", width=8, height=10, bg="white",res=300)
              intgr <- 0
          }
      else if (is.element(platfrm,c('x86_64-w64-mingw32')))               # PC is Windows 7 pc
          {     # set file location for input and output files in the new working directory
              grDevices::png(filename=filenm, units="in", width=8, height=10, bg="white", res=300)
              intgr <- 2
          }
      else if (is.element(platfrm,c('i386-w64-mingw32')))               # PC is Windows 7 pc 32 bit
          {     # set file location for input and output files in the new working directory
                                        # starts the graphical device driver for windows
              grDevices::png(filename=filenm, units="in", width=8, height=10, bg="white",res=300)
              intgr <- 3
          }
      else
          {  # works for Mac OS X - not sure about linux / others...
              grDevices::png(filename=filenm, units= "in", width=8, height=10, bg="white",res=300)
              intgr <- 1
          }
      return(intgr)
  }
        
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title hutchscrn - determines which OS program is being run on and sets up a print to screen
#### @description hutchscrn sets up the correct printing environment for each OS and sets up a print to screen
#### @details hutchscrn sets up the correct printing environment for each OS and creates a screen plot
#### @author John F. Hutcheson
#### @param platfrm - platform the program is being run on
#### @return intgr - integer indicating which platform the program is being run on and which format is used for storage
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
hutchscrn <- function(platfrm)
    { if (is.element(platfrm,c('i386-pc-mingw32')))
          {                # script is running on windows
                   # starts the graphical device driver for windows
              grDevices::pdf(width=8,height=10)
              intgr <- 0
          }
      else if (is.element(platfrm,c('x86_64-w64-mingw32')))               # PC is Windows 7 pc
          {     # set file location for input and output files in the new working directory
              grDevices::pdf(width=8,height=10)
              intgr <- 2
          }
      else if (is.element(platfrm,c('i386-w64-mingw32')))               # PC is Windows 7 pc 32 bit
          {     # set file location for input and output files in the new working directory
                                        # starts the graphical device driver for windows
              grDevices::pdf(width=8,height=10)
              intgr <- 3
          }
      else
          {   # works for Mac OS X - not sure about linux / others...
              grDevices::pdf(width= 6, height= 6, bg= "white")
              intgr <- 1
          }
      return(intgr)
  }


#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### function to plot graphs by sector 
#### The first plot is the hours by heads speed graph showing both the NASAR and Kent models
#### The second plot is the spacing by hours graph
#### The last graph is the POD by COV graph showing the POD for the sector based on the above
#### This function plots out all three graphs that are printed out for each sector
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title plotone - graph plotting function
#### @aliases plotone
#### @description plotone - function that plots sector graphs showing 3 views of the search plan by sector
#### @details plotone - function creates a single pdf page for each sector which graphically depicts a sensitivity analysis for each sector for the relationships between searchers and duration, spacing and duration, and coverage and probability of detection.
#### @author John F. Hutcheson
#### @param versn - version of the script that is being used so that graphs display the version of the software
#### @param platfrm - platform that is used to run the model so that the proper graphics device is called
#### @param sector - name of the sector to be searched as a string - such as "BB"
#### @param area - area of the sector to be searched in acres or hectares as a real number
#### @param terrain - terrain of the sector to be searched as a string - such as "HeavyWoods" or "SteepTerrain"
#### @param tod - time of day as a string - such as "Night"
#### @param Thrs - target duration hours for the search
#### @param Tsrchr - target count of searchers required for the search
#### @param Tspcng - target spacing between searchers in meters
#### @param amdr - average maximum detection range expected in this sector in meters
#### @param auc - area unit conversion - constant to convert from acres or hectares
#### @param NHRSH - hours* heads expected for the search if NASAR's slowest speed used to estimate
#### @param ROT - Rule of thumb hours to search a sector based on the Kent County model
#### @param esw - effective space width - relative amount of spacing that equals coverage of 1 amdr
#### @param cov - coverage - ratio of esw * spacing to amdr
#### @param pood - probability of detection
#### @param atag - string used to label graphs to show area unit used (either "acres" or "hectares")
#### @param Stitle - String denoting the 'name' of the search. used to label graphs with name of the Search.
#### @inheritParams lattice::xyplot
#### @return graphic device outputs as files
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
plotone<-function(versn,platfrm,sector,area,terrain,tod,Thrs,Tsrchr,Tspcng,amdr,auc,NHRSH,ROT,esw,cov,pood,atag, Stitle)
{ #  first set up the graphics device for use by the different operating systems
                # above used to keep package checks from complaining about an undefined global variable in the plotone function
      
    finam<-paste("Sector ",sector,".pdf")
    pdfval <- hutchpdf(platfrm,finam)         # sets up the right graphics environment
    lattice::trellis.par.set(list(par.main.text=list(lineheight=2)))     # sets grpahics title heights
    sect<-paste(sector," : Area= ",area, atag, "; Searchers= ",Tsrchr,"; Hours= ",Thrs,"; Spacing(m)= ",Tspcng)
                                        #   sect is subtitle for first graph
    sect2<-paste(sector," : TOD= ",tod, "; Terrain= ",terrain, "; AMDR= ",amdr)     # subtitle for second graph)
        # adjust the x scale to include the TSearcher value
    srchr <- as.double(c(seq(1,Tsrchr+4,by=1)))
    rhry <- (NHRSH+ROT)/srchr/2       # adjust the plot so that the average y value of both the NASAR
  # and the ROT models are plotted so both show on graph
    pp1<-lattice::xyplot( rhry~srchr, xlab=list(label='Number of Searchers (excluding leaders)[Vertical Red Line is TSearchers]',cex=.6),ylab=list(label='Rule of Thumb Hours [Horizontal Red Line is THours]',cex=.6), type='b',main=list(label=paste(c('Duration F(Head Cnt) :',sect),sep="",collapse=""),cex=.8,just=c("left","top"),x=grid::unit(0.1, "npc")),sub=list(label="Confirm the TSearcher value (Targeted Searcher count) leads to a Reasonable Search Duration",cex=.8,col='blue'),panel=function(...)
        {
                
            lattice::panel.abline(v=as.double(Tsrchr), h=as.double(Thrs), col='red',lwd=1.5)
            
            lattice::panel.text(x=srchr[1],y=rhry[srchr[2]],labels="Brown=Modified NASAR
Blue=ROT",pos=4,cex=.6,crt=180)
            lattice::panel.text(x=3, y=rhry[1.5]*.9, labels= Stitle, pos=4, cex=1.5)
            lattice::panel.grid(h=5,v=-1,col="grey",lwd=.2, lty=3)
            lattice::panel.curve(ROT/x,col="blue")
            lattice::panel.curve(NHRSH/x, col="brown")
        }
                         )
      
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# plot two
    # span the x value from 25% to 125% of Tspacing in 10 equal spaces
    xspcg <- seq(Tspcng*.4,Tspcng*2.5, length.out=7)
    dursa<-data.frame()  # where data is stored
    dursa <- GEOM2(area,auc,Tsrchr,xspcg)
    xdursa <- XTNDR(dursa)
                                        # next plot is the first on the second page
    pp2 <- lattice::xyplot(durhrs~xspcg, groups=xdursa$spd, data=xdursa, xlab=list(label='Spacing of Searchers (meters) [Red lines is TSpacing(m)]',cex=.6),ylab=list(label='Team Hours as a function of Spacing and Speed',cex=.6), main=list(label=paste(c("\n", "Hours f(Area,Searchers,Speed,Spacing)for Sector",sect2),sep="",collapse=""),cex=.8,just=c("left","top"),x=grid::unit(0.1, "npc")),sub=list(label="Select Spacing and Speed to determine Hours Duration for the Search",col="blue"),auto.key=list("Speed (mph)",title="Speeds (mph)",cex.title=.6,x=.9,y=.05,corner=c(0,0)),type='l' ,  panel=function(...)
        {
            lattice::panel.abline(v=as.double(Tspcng),col='red',lwd=1.5)
            lattice::panel.abline(h=as.double(Thrs),col='red',lwd=1.5)
            lattice::panel.text(x=5, y=5, labels=date(), pos=4, cex=.6, crt=180)
            lattice::panel.grid(v=-1,h=-1,col="grey",lty=3, lwd=.2)
            lattice::panel.xyplot(...)
        }
                           )

                                        # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                        # plot three    
                                        # Do the same for an x to use to evaluate POD
    xspcg <- seq(Tspcng*.4,Tspcng*2.5, length.out=7)
    covg <- list()
    covg <- seq(.2,2.0,by=.1)
                                        # now calculate the y values to print
                                        # first the coverage as function of spacing
    ycovg <- list()
    ycovg <- esw/xspcg

                                        #next pod as function of coverage
    ypod <- list()
    ypod <- lapply(covg,pod)
    ypody <- data.frame(cbind(ypod,covg))
    colnames(ypody) <- c("yped","cevg")

                                        # create line to show coverage resulting
                                        # _______________________
                                        # now plot the POD curve and show the Coverage expected
                                        # now plot the COV vs Spacing plot

    ppp <- lattice::xyplot(yped~cevg,data=ypody,xlab=list(label='Coverage of Search (1.5*AMDR/Actual Spacing)',cex=.6),ylab=list(label='Probability of Detection%',cex=.6),type='b',main=list(label=c("  ","Probability of Detection as a function of the Expected Coverage of",sector),cex=.8),sub=list(label="Confirm the Targeted Searcher spacing in meters leads to a Reasonable POD",col="blue"), panel=function(...)
        {
            lattice::panel.abline(v=cov,h=pood,col='red',lwd=1.5)
            lattice::panel.text(labels=versn,x=ypody[[17,2]],y=ypody[[16,1]],cex=.5)
            lattice::panel.grid(v=-1,h=-1,col="grey",lty=3, lwd=.2)
            lattice::panel.curve((1-exp(-x)),col="purple")
        }
                           )
    
    graphics::plot(pp1,split=c(1,1,1,3),more=T)
    graphics::plot(pp2,split=c(1,2,1,3),more=T)
    graphics::plot(ppp,split=c(1,3,1,3),more=F)

    grDevices::dev.off()

    return(1)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Running Total function
#### Calculates running totals for TSearchers, POS, and AreaCov (area covered)
#### The totals are used for the bestsearch chart 
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title rtotal - running totals for critical figures
#### @aliases rtotal
#### @description rtotal - running totals for Searchers, POS and area covered
#### @details rtotal - fuction sums up the running totals for searcheres, Probability of Success, and sector area covered across many sectors. This information used for managing the search as well as running the bestsearch() model
#### @author John F. Hutcheson
#### @param ndf - search output dataframe containing detailed hours, searcher counts, area, area covered, pos, pod and all known stats by sector
#### @return ndf - search output dataframe that now includes running totals for searchers, POS, and Area Covered 
## # @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rtotal<-function(ndf)
  { pos<-ndf$TSearchers
    ndf$RTotlHeads<-cumsum(pos)
    pis<-ndf$POS
    ndf$RTotlPOS <- cumsum(pis)
    tarea<-ndf$AreaCov
    ndf$TotlArea<-cumsum(tarea)
    tspcg <- ndf$AreaCov*ndf$TSpacing   # tracks the spacing area covered for running average spacing
    ndf$AvgSpacing <- cumsum(tspcg)/cumsum(tarea)   # average spacing
    rPOC <- ndf$AreaCoverage*(ndf$POC-ndf$POScum)/100    # tally up the POC available by area covered
    ndf$rPOC <- cumsum(rPOC)                         # this is the area coverage compensated POC available
    avgAMDR <- ndf$AreaCov*ndf$AMDR                  # this is the area averaged AMDR to be summed
    ndf$AvgAMDR <- cumsum(avgAMDR)/cumsum(tarea)      # this is the area averaged AMDR for use by bestsearch
    return(ndf)
}

# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' @title \code{searchme} - Creates or modifies a Search Plan.
#'
#' @description The \code{searchme()} function takes the input csv file SearchInput.csv and returns the SearchOut.csv file which defines a search plan.
#' 
#' @details The \code{searchme()} function takes inputs including Sector,Area, AreaCovereage, Terrain, WX, TOD, THours,TSearchers,TSpacing, AMDR, Rank, and POScum in the form of a CSV file (SearchInput.csv) and returns values to the console and files to the working directory that form the basis of a search plan. At the console, a dataframe with new columns containing estimates for the man hours required, ideal team size, time to clear the sector, POS, POD, cumulative POS, and estimates to range these values between NASAR worst case estimates and Rule of Thumb estimates is returned. Then the function returns a summary of descriptive stats summarizing the plan followed by another display of the dataframe sorted alphabetically by name. The function stores the dataframe in the default SearchOut.csv file, along with a number of graphics displaying the model details by sector. After all sector stats and charts are generated, the fuction calls the \code{searchstatus()} function to create summary graphs for the search plan. See the documentation for \code{searchstatus()} for further information on those features.
#' The \code{searchme()} function is designed to predict optimal values for all key input variables where it finds the value -1.  In other cases, the program accepts the given value and assusmes the planner knows best, but graphs these values to allow planners to easily compare them to an Urban and a NASAR model.  
#' @author John F. Hutcheson
#' @param FtT - Fast to Thorough parameter - sets the relative hastiness of the search 1 = very fast, 5 = very thorough.  This is a parametric form of the team spacing.
#' @param filout - name of the output csv file to store in the workding directory. Defaults to "SearchOut.csv". This output files contains the input and output variables for each sector that comprises the search plan.
#' @param AreaUnits - a variable that toggles between Acres and Hectares area units. Defaults to "Hectares" for sectors. All area units are consistent with this selection within the program.
#' @param graphs - variable that toggles the creation and storage of graphs for each sector which describe the selected strategy for searching each sector. This parameter has the biggest impact on run time.  Defaults to graphs=1 (graphs will be produced and program will be at it's slowest).  Set to graphs = 2 to turn off sector graphs. Set to 3 to turn off all output in file form but still report to the console.
#' @param directory - name of the directory used to store output data from running this function. Defaults to the R working directory. The function will create a new subdirectory if the default value is changed.  Used to segregate output from different runs of the model.
#' @param STitle - string representing the name of the search for use as a title on the graphic output
#' @return df - data frame that includes the input table with all unknown variables defined
#' @examples
#' \dontrun{searchme(FtT=1.5, graphs=2, AreaUnits = "Acres", STitle="Example Loose Grid Search")}
#' \dontrun{searchme(FtT=4, graphs=3, STitle = "Example Tight Grid Search")}
#' @export searchme
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
searchme <- function(FtT=3,filout='SearchOut.csv',AreaUnits="Hectares",graphs=1,directory="Searches",STitle="")
{
  vers <- c("20160312jfh")
 # get rid of any old variables and data files
  oldwd <- getwd()            # store the old working directory in oldwd
 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Easy to reach factors for the models that place the calculated hours between NASAR and Kent Models
      
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  platfrm <- R.version$platform
  # first retrieve the example SearchInput data from the package to help the user create a template if they don't have one already
  SearchInput <- utils::read.csv(system.file("extdata","SearchInput.csv", package = "rSARP"),header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA),sep=",",as.is=c(1))
  
  # now read the data file with input data # as.is columns are text read as text not assume to be factors from the working directory
  if(!(file.exists("SearchInput.csv")))
       {     # no file in place so create that template
           print(paste("You didn't save an Input File in your working Directory where it belongs or it isn't properly named      SearchInput.csv.  If you thought you had one there please check your working directory " , oldwd, "  It could also be that your working directory needs to be changed.  Please check for errors in naming the file. If you would like a starter file placed in your working directory, please answer ", "Y", " at the prompt below. The program will store a demo starter file in your working directory for you. You will then need to revise the file saved there according to your needs before you run the program again. "," If you would rather correct the error on your own, please answer N at the prompt.", " Either way the program has stopped and you will need to run it again after addressing these issues."))
           yorno <- readline(prompt='Y -> place demo SearchInput.csv file in my working directory   N -> I will take care of it')
           if (yorno == "Y" || yorno == 'y')
               {  utils::write.table(SearchInput,"SearchInput.csv",row.names=FALSE, sep=",", col.names=TRUE, quote=TRUE)
                }   # create a new file for the user where it belongs
           stop
       } 

#The SearchInput files in the working directory
  df<-utils::read.csv("SearchInput.csv",header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), sep = ",",as.is =c(1,4,5,6))
 
 # df is the dataframe representing the search sector data
  terraindf <- utils::read.csv(system.file("extdata","Terrains.csv", package = "rSARP"),header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA),sep=",",as.is=c(1))

  # terraindf is the dataframe representing the terrain data
  
  weatherdf <- utils::read.csv(system.file("extdata","Weathers.csv", package = "rSARP"),header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA),sep=",",as.is=c(1))
                       # weatherdf is the dataframe representing the weather data  
  if (directory!="Searches")
      {
          if (!(dir.exists(directory)))
              {  dir.create(directory)
             }
          newdir2 <- paste(oldwd,directory,sep="/")
          setwd(newdir2)
      }
      # THis allows a subdirectory to be created to hold the output files  if directory is changed from "Searches"
      # colClasses may have skipped the column, but still expecting rows down to that level
      # remove columns no longer needed
  df<-df[,1:12]         # only take columns 1 thru 12
  
      # need to remove blank rows since some rows may be blank due to other trash in columns beyond 11
  df<-df[df$Area!="",]
  dfrows<-sum(!is.na(df$Area))

  df<-df[1:dfrows,]
# ------------------------------------------------------------------
    # Check to insure the data cloumns are there and properly named
  if (!is.element('TSearchers',stats::variable.names(df)))
    stop("Missing TSearchers data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('Sector',stats::variable.names(df)))
    stop("Missing Sector Names data or not properly named- please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('Area',stats::variable.names(df)))
    stop("Missing Area data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
    if (!is.element('AreaCoverage',stats::variable.names(df)))
    stop("Missing Area Coverage data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('Terrain',stats::variable.names(df)))
    stop("Missing Terrain data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('TOD',stats::variable.names(df)))
    stop("Missing TOD data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('WX',stats::variable.names(df)))
    stop("Missing WX data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
  if (!is.element('TSpacing',stats::variable.names(df)))
    stop("Missing TSpacing data or not properly named - please provide estimates of the total searchers spacing (meters) used to search the sector")
  if (!is.element('AMDR',stats::variable.names(df)))
    stop("Missing AMDR data column or not properly named - please provide estimates of the Avg Max Detcn Range (meters) used to search the sector")
  if (!is.element('Rank',stats::variable.names(df)))
    stop("Missing Rank data column or not properly named - please provide concensus ranking of each sector for Probability of Success calculations")
  if (!is.element('POScum',stats::variable.names(df)))
    stop("Missing POScum data column or not properly named - please provide concensus ranking of each sector for Probability of Success calculations")
 
# -----------------------------------------------------------------                                                      # Need to provide a conversion factor for Area units
  Auc<-1
  Atag<-"Acres"
  if (AreaUnits=="Hectares")
      { Atag<-"Hect"
        Auc<-2.47105
    }
      # Need to house values for the initial AMDR and Tspacing values if these are given as 0 
# -----------------------------------------------------------------      
      # Need to prepare variables to manage % Area Coverage impacts
  df$fnCovered <-abs(df$AreaCoverage/100)  # fraction of sector to be searched
  df$fnMissed <- 1-df$fnCovered
  df$AreaCov<-df$Area*df$fnCovered  # actual area to be searched excluding areas missed or unable to search (bogs, ponds, etc)
  
# -----------------------------------------------------------------
  fttt <- FtT       # store FtT for later reporting
  FtT=Fasttothorough(FtT)             #   convert the FtT to the correct internal factor - normally 1.5
# -----------------------------------------------------------------
 # now set the order of the databasse into its original order

  df<-df[order(nchar(df$Sector),df$Sector),]

# -----------------------------------------------------------------
  
# Begin cycling thru the input data to make sure there are no missing values and to do initial warnings
  tla<-df[df$Area>100/Auc,]  #  if $Area>100 issue a warning on the console
  if (any(is.na(tla)))
    {
      methods::show("More than 100 acres or 40 hectares in search area - WARNING - Area should be broken down if possible")
    }
          
      # Check for data that is missing

  if (any(is.na(df$Area)))
      {
          stop("Error - some of the Area data is missing!  Please check the input data")
      }
  if (any(is.na(df$AreaCoverage)))
      {
          stop("Error - some of the Area Coverage data is missing!  Please check the input data")
      }
  if (any(is.na(df$WX)))
      {
          stop("Error - some of the WX data is missing!  Please check the input data")
      }
  if (any(is.na(df$TOD)))
      {
          stop("Error - some of the TOD data is missing!  Please check the input data")
      }
  if (any(is.na(df$TSpacing)))
      {
          stop("Error - some of the TSpacing data is missing!  Please check the input data")
      }
  if (any(is.na(df$Sector)))
      {
          stop("Error - some of the Sector data is missing!  Please check the input data")
      }
  if (any(is.na(df$TSearchers)))
      {
          stop("Error - some of the Wx data is missing!  Please check the input data")
      }
  if (any(is.na(df$THours)))
      {
          stop("Error - some of the THours data is missing!  Please check the input data")
      }
  if (any(is.na(df$TSpacing)))
      {
          stop("Error - some of the TSpacing data is missing!  Please check the input data")
      }
  if (any(is.na(df$AMDR)))
      {
          stop("Error - some of the AMDR data is missing!  Please check the input data")
      }
  if (any(is.na(df$Rank)))
      {
          stop("Error - some of the Rank data is missing!  Please check the input data")
      }
  if (any(is.na(df$POScum)))
      {
          stop("Error - some of the POScum data is missing!  Please check the input data")
      }
#  all the data appears to be in order for the first passes to set searcher count and prelim spacing
# create lists to hold data for calculated values
# create list to hold the resulting ESW, Coverage, POD and POS figures
  pod <- list()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now sum up the rankings of the sectors and calculate the POC values
   # adjust the ranks to the POC values and store in the list
  totlpoc<-sum(df$Rank)
  df$POC<-round(df$Rank/totlpoc*100,digits=3)

    if (sum(df$POScum > df$POC)>0)
    {                         # some fool just entered more cumulative POS than was originally given to the sector - a big no-no
      stop ("Error - One of your POScum values is larger than the POC for the sector - Simply Impossible! Please correct the POScum value")
    }
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now zero out the terspd and terheads columns so that afterwards calculated values can be shoved into them
  df$terspd<-c(0)
  df$terheads<-c(0)
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now check the AMDR values and replace if they're <=0 with an estimated value from the Terrain provided
  cntr <- nrow(df)    # number of sectors to be processed
  cntrcy <- cntr/3    # number of graphs
  zeroamdr<-df[df$AMDR<=0,]     # pull out cases where AMDR is 0 or negative
  if (!is.na(zeroamdr[1,1]))
    {                           # have cases where AMDR is zero
        hasmadr<-df[df$AMDR>0,]       # pull out all the other cases to add together later
        zeroamdr$AMDR<- lapply(FUN=amdrestimator, X= zeroamdr$Terrain, terraindf)       #for all 0 AMDR values (colm 9) replace with new value from amdrestimator function
        zeroamdr$AMDR <- as.vector(zeroamdr$AMDR, mode = "numeric")
        df<-rbind(zeroamdr,hasmadr)
        rm(zeroamdr,hasmadr)          # now have cleaned up intermediate dfs
    }
  nospace<-df[df$TSpacing<=0,]             # pull out cases where spacing is zero or negative
  if(!is.na(nospace[1,1]))       # have cases where AMDR isn't specified
    {
      haspace<-df[df$TSpacing>0,]              # pick up rest of the cases
  
      nospace$TSpacing<-mapply(FUN=tspacingestimator, nospace$AMDR, FtT)        # for all zero spacing cases use 1.5 times AMDR to estimate space
      df<-rbind(nospace,haspace)               # put the pieces back together
      rm(nospace,haspace)                      # clean up
    }
# now have the AMDR column filled with estimated values or calculated from Terrain values
# now have the TSpacing column filled with planned spacing or spacing calculated to give 63% POD

   
# -----------------------------------------------------------------      
# Adjust the Area to be covered based on the AreaCoverage values
  # From here forward the Area value needs to be relaced with a adjusted area value
  # From here forward the df$Area will be replaced in calculations
  negAC <- df[df$AreaCoverage < 0,]         # create df of negative AreaCovered rows - these the rows where a fn of the sector is searched for the first time
  posAC <- df[df$AreaCoverage >= 0,]   # rows where AreaCovered is 0 or greater than 0 - these are treated to Bayesian math on subsequent searches
  # set AreaCovered to negative if a portion of the sector is searched for the first time in a follow up search to a portion of the sector missed
  # on first pass.
  if (!is.na(negAC[1,1]))
      # have some negative Area Coverage cases
      {
          negAC$FirstPass <- -1  # searching this portion of sector for the first time no matter if other portions have been hit
      }
    if (!is.na(posAC[1,1]))
        {
         # have some positive AC cases (most common)
            posAC$FirstPass <- 1  # searching this portion over and over again - Bayesian City
        }
  df <- rbind(negAC,posAC)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
                                        # use function thumbcount to calculate hours by rule of thumb
  TerrainHours <- lapply(FUN= terrainnr,  df$Terrain, terraindf)       # calculate the relative man hours associate with this terrain type
  TerrainHours <- as.data.frame(TerrainHours)
  df$ROThrs<- round(mapply(FUN=thumbcount,df$AreaCov,AUC=Auc, TerrainHours, df$TOD),digits=3)
  df$NHRSH<- round(mapply(FUN=NHRSH,area=df$AreaCov,AUC=Auc,spacing=df$TSpacing),digits=3)   
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Adjust any THours values to 0 if TSearcher value is 0

  df$THours[df$TSearchers==0]<-0   # if no searchers assigned, then no hours are possible
   # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # now check to see if TSearchers is set to -1 (user wants program to calculate value)
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ns<-df[df$TSearchers==-1,]   # select the rows without searchers specified
  if (!is.na(ns[1,1]))
    {
        hassearchers<-df[df$TSearchers>=0,]   # select the others to add back later
        spdlst <- as.data.frame(lapply (FUN=terspeed, X = ns$Terrain, terraindf = terraindf))   # new speed based on terrain
        wxspdlst <- as.data.frame(lapply(FUN=weathernr, X = ns$WX, weatherdf= weatherdf))
        spdlst <- spdlst + wxspdlst
        spdlst <- round(spdlst, digits=3)
        spdlst <- t(spdlst)
        dimnames(spdlst) <- NULL
        ns$terspd <- (spdlst)
        ns$terheads<-round(mapply(FUN=GEOM3,area=ns$AreaCov,AUC=Auc,spcg=ns$TSpacing,TOD=ns$TOD,spd=ns$terspd),digits=4)   # calc hrs*heads for ns cases
        rm(spdlst, wxspdlst) 
   # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   # now check for the -1 hour cases within this group and assume for this group of -1 hours cases the searchers can be adjusted
   # but for the cases where planner initially specifies search heads we don't adjust it (they get what they specify)
   # otherwise, there's no way to plan a final case
        nhs<-ns[ns$THours==-1,]             # now have the -1 hours cases in the nhs subset which are the ones we need to calculate
        if(!is.na(nhs[1,1]))
            {
                hshs<-ns[ns$THours>=0,]             # other cases that had no searchers but planned hours

                nhs$TSearchers<-as.integer(round(nhs$terheads/4,digits=0))             # use 4 hours to estimate searchers for the -1 case
            # but if this results in headcount >9 drop to 6 hours
                nhs[nhs$TSearchers>9,8]<-as.integer(round(nhs[nhs$TSearchers>9,18]/6,digits=0))  # this could lead to a loop where planner doesn't recognize need to cut sector
  # or the need to fix the hours or searcher count to avoid having the computer estimate the values
          
                hshs$TSearchers<-as.integer(round(hshs$terheads/hshs$THours,digits=0))      # use normal hours for others
                ns<-rbind(nhs,hshs)                # recombine NS cases
                rm(nhs,hshs)                       # clean house
            }
  
        lttwo<-ns[ns$TSearchers<2,]        # find the cases where searcher count drops below 2
        if (!is.na(lttwo[1,1]))
            {
                mttwo<-ns[ns$TSearchers>1,]        # find the rest
                lttwo$TSearchers<-c(2)             # set the head count to two for the low cases
                ns<-rbind(lttwo,mttwo)             # put the ns set back together
                rm(lttwo,mttwo)                    # clean house
            }
  
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # now find the cases where the head count is too high and hours at 4
        gtnine<-ns[ns$TSearchers>9,]
        if(!is.na(gtnine[1,1]))
            {
                ltten<-ns[ns$TSearchers<10,]      # find the rest of the cases
                gtn4<-gtnine[gtnine$THours==4,]   # find the cases where we set time to 4 hours (most are set by computer)
                if(!is.na(gtn4[1,1]))
                    {
                        notgtn4 <- gtnine[gtnine$THours!=4,]   # find the other cases which may be required by the planner for a special case           
                        gtn4$TSearchers<-round(gtn4$terheads/6,digits=0)   # set the TSearchers using more time
                        gtnine<-rbind(gtn4,notgtn4)       # recombine
                    }
                ns<-rbind(ltten,gtnine)          # recombine the ns set
                rm(ltten,gtnine)
            }
        df<- rbind(ns, hassearchers, deparse.level=1)          # recombine the df set - should be done with 0 searcher subset calcs
        rm(ns,hassearchers)
    }
     # now have the number of searchers set 
  
      # hrspace is the hrs*spacing using a pure physical model and assuming .29 mph speed
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Now check the THours value and replace it if it's -1 with a calculated value based on the Terrain provided
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Now adjust the THours for the values that were initially -1 to the 
  zhr<-df[df$THours==-1,]           #Find all -1 hour rows
  if(!is.na(zhr[1,1]))
      {                              # since we have found some
        tspd <- as.data.frame(lapply (FUN= terspeed, X= zhr$Terrain, terraindf))  # EOet new geoconstant based on terrain
        wxspdlst <- as.data.frame( lapply (FUN= weathernr, X= zhr$WX, weatherdf))  #weather impact on speed
        zspd <- wxspdlst + tspd
        zspd <- round(zspd, digits = 3)
        zspd <- t(zspd)
        dimnames(zspd) <- NULL
        zhr$terspd <- (zspd)
        zhr$terheads<-round(mapply(FUN=GEOM3,area=zhr$AreaCov,AUC=Auc,spcg=zhr$TSpacing,TOD=zhr$TOD,spd=zhr$terspd),digits=4)   # calc hrs*heads for zhr cases
        nzhr<-df[df$THours!=-1,]      # find the rest

        zhr$THours<-round(zhr$terheads/zhr$TSearchers,digits=2)         # calculate the THours values from the TSearchers
        df<-rbind(zhr,nzhr)          # bring the parts back together
        rm(zhr,nzhr,zspd,wxspdlst,tspd)                 # clean house
    }

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # now back calculate the speed and hours*heads figures for the cases where the planner wanted to use values
  # shove these into terspd and terheads values respectively
  gad<-df[df$terheads==0,]      # these will be the cases where the terspd and terheads haven't been calculated yet
  if(!is.na(gad[1,1]))
    {
      # apparently we have a few bonehead planners around yet
      gad$terspd<-round(mapply(FUN=GEOM4,area=gad$AreaCov,AUC=Auc,hrs=gad$THours,srchrs=gad$TSearchers,spcg=gad$TSpacing),digits=3)   # new speed based on figures given for Thours and distance covered
      gad$terheads<-round(gad$TSearchers*gad$THours,digits=4)   # calc hrs*heads for gad cases
      egad<-df[df$terheads!=0,]
      df<-rbind(gad,egad)         # bring the parts back together
      rm(gad,egad)                # clean house
    }
  # now have speeds and hours for the forced values
 #  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
  # Now calculate the impact of varying the manpower on both THours and POS (THours first)
  # Assume the Speed of search doesn't change, just the number of people on team        
  df$SAVHRS<-round(df$terheads/df$TSearchers-df$terheads/(df$TSearchers+1),digits=2) # calculate and store the time savings if an additional member is added

#  
  nosav<-df[df$SAVHRS=="NaN",]   # find the values where we're not planning to search and set these to 0
  savn<-df[df$SAVHRS!="NaN",]
  if(!is.na(nosav[1,1]))
      {
          nosav$SAVHRS<-0
      }        # if there are no values then skip otherwise set to 0
  df<-rbind(nosav,savn)
  rm(nosav,savn)

  
  # now have the hours saved if another member added to the team - need to save this to the dataframe
#  
 #  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This code fragment address searcher spacing, ESW, and POD only
 # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 # Second Pass to create POD values
  df$ESW<-as.double(df$AMDR*1.5)          # calculate coverage by comparing ESW to actual spacing
    # assumes entire sector is covered
  df$COV<-df$ESW/df$TSpacing
  df$POD<-round(1-exp(-df$COV),digits=3)         # calculate max pod from this spacing
  df$POD[df$TSearchers==0]<-0                      # Set pod values to 0 for cases we won't tackle this period
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now calculate the impact of varying the manpower on POD assuming the spacing decreases with addition of another person
    # now create an x list that can be used to push up TSpacing against COV
  df$delspc<-df$TSpacing-df$TSpacing/(df$TSearchers+1)*df$TSearchers      # calculate the change in spacing from an added team member
  df$delspc[df$TSearchers==0]<-0                      # Set delspc values to 0 for cases we won't tackle this period
  df$dPOD<-round((exp(-(df$ESW/(df$TSpacing-.5*df$delspc))))/((df$TSpacing-.5*df$delspc)**2)*df$ESW*df$delspc,digits=3)
  df$dPOD[df$TSearchers==0]<-0                        # Set dpod values to 0 for cases we won't tackle this period
 
# -----------------------------------------------------------------      
# Now need to calculate what the POS would be with 100 % area coverage (no missed areas)
  df$POS<- round(df$POC * df$POD ,digits=3)
# -----------------------------------------------------------------      
#  
  # with version 32 on, subtract from the POS using the AreaCovered fraction
  df$dPOS<- round(df$dPOD * df$POC * df$fnCovered ,digits=3)
  df$POS<- round(df$POC * df$POD * df$fnCovered ,digits=3)

  # now create the final column OPOScum - Overall POS cumulative
     # now find the 0 POScum cases
  hpos<-df[df$POScum!=0,]             # now have the not zero POScum cases in the hpos subset
  npos<-df[df$POScum==0,]         # These are the zero POScum cases
  if (!is.na(npos[1,1]))
    {                   # have some cases where POScum was set to 0 so no prev searches done there
      npos$OPOScum<-npos$POS           # zero POScum cases get POS for OPOScum values
    }
  if(!is.na(hpos[1,1]))               # we have some cases to consider not zero
    {
      pospos<-hpos[hpos$POScum>0,]        # these are the positive POScum cases - so prev searches have been done
      negpos<-hpos[hpos$POScum<0,]        # These are the cases where POScum is <0 so no longer actively searched
      if (!is.na(pospos[1,1]))
          {                             # found some positive cases

      # but now need to check to see if these are re-searches of portions of an already completed sector that hasn't been completed yet (FirstPass=-1)
            
              pospos$POS<-round((pospos$POC-pospos$POScum)*pospos$POD * pospos$fnCovered, digits=3)        # POS adjusted based on remaining POC
          # above is the Bayesian part
              skippos <- pospos[pospos$FirstPass==-1,]  # rows with new area to searchers
              noskipos <- pospos[pospos$FirstPass==1,]  # rows already searched

              if (!is.na(skippos[1,1]))
                  {
                    # found some cases where FirstPass ==-1
                      skippos$POS <- round(skippos$POC * skippos$POD * skippos$fnCovered,digits=3) # undo the bayesian for new areas to searchers
                      pospos <- rbind(skippos,noskipos)   # reunite the dataframe parts
                  }
              rm(skippos,noskipos)   # clean up memory
          
              pospos$OPOScum<-pospos$POS+pospos$POScum         # OPOScum is sum of total POS after search
          # since this comes after the POS has been cleaned up, a new to searchers search of a portion of a sector already searched will still add up correctly
          }
      if (!is.na(negpos[1,1]))
          {                           # found some cases where no longer searching
              negpos$POS<-c(0)          # so set the POS for these to 0 to indicate no longer searching
              negpos$OPOScum<--1*negpos$POScum        # set the OPOScum value to the POScum value but make it positive
              negpos$SAVHRS<-c(0)       # do same for all status tracked variables
              negpos$terheads<-c(0)
          }
      hpos<-rbind(pospos,negpos)      # rebuilding dataset
      # POScum is the cumulative POS before the search - so if it starts off 0 it stays 0 on the output file

  }
  df<-rbind(npos,hpos)
  rm(npos,hpos)
 # now reset the order of the databasse into its original order
 #
# test plots. first create x values beginning with speed, then searchers, then spacing
  spd <- as.double(c(.3,.5,.75,1.0,1.5,2.0,2.5,3.0))
  srchr <- as.double(c(1:10))                # this is the x axis for the first graph
  hrdur <- as.double(c(seq(1,14,by=2)))      # x values for the NASAR graph replacing original spacing values
# produce graphs for each segment based on the 3 models
# begin printing out the charts for setting the searcher count and spacing and speed of search
 #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preliminary Pass

                                        # output in panel of 3 graphs
 # -----------------------------------------------------------------
# -----------------------------------------------------------------
       
  
#This code is the property of John Hutcheson and is not to be sold commercially or used without permission of the Board of Directors
#This code is a beta version of an R program designed to take a CSV file descriping a prospective search by sectors and lead planners and search managers through the process of selecting the proper team makeup and expected outcomes (Search Duration, number of searchers required, spacing, and ultimately POD)

  
  if (graphs==1)
      {
          dfa<-df[df$TSearchers!=0,]
    # ______________________________________
        # now plot the Hrs  vs Spacing vs speed curves plot
          grphs<-NA
          length(grphs)<-cntr      # trick to speed up creating grphs and passing values to it - fix the size
          grphs<-mapply(FUN=plotone,vers,platfrm,dfa$Sector,dfa$AreaCov,dfa$Terrain,dfa$TOD,dfa$THours,dfa$TSearchers,dfa$TSpacing,dfa$AMDR,Auc,dfa$NHRSH,dfa$ROThrs,dfa$ESW, dfa$COV, dfa$POD, Atag, STitle)
          gc()
      }

  # -----------------------------------------------------------------
# Now pass the newly created variables to the output file
  methods::show(paste("FtT for this run was: ",fttt))
  methods::show(paste("Program completed successfully.  Please check the following location for output/original files: ",getwd()))

  maxpospot<-max(df$dPOS)
  posSect<-(df$Sector[df$dPOS==maxpospot])
  maxhrspot<-max(df$SAVHRS)
  hrsSect<-(df$Sector[df$SAVHRS==maxhrspot])
  df<-df[order(df$POS,decreasing=TRUE),]
  df<-rtotal(df)
  df$POSm <- df$fnMissed/(1-df$fnMissed) * df$POS        # should be POS missed
  LostPOS <- sum(df$POSm)
  methods::show(df)
  methods::show(paste(" "))
  methods::show (paste("Total Probability of Success if all sectors searched as planned and all sectors rated correctly =",format(sum(df$POS),width=2),"%"))
  methods::show(paste(" "))
  methods::show(paste(" "))
  methods::show (paste("Impact of missing areas in LOST POS = ",format((LostPOS),width=2),"%"))
  methods::show(paste(" "))
  methods::show(paste(" "))
  methods::show (paste("Total Searchers Required if all sectors searched as planned =",sum(df$TSearchers),"peeps"))
  methods::show(paste(" "))
  methods::show(paste(" "))
  methods::show (paste("Total elapsed Hours to execute this plan if all sectors searched in series =",sum(df$THours),"Hours"))
  methods::show(paste(" "))
  methods::show(paste(" "))
  methods::show (paste("Total Manhours consumed if all sectors searched as planned =",sum(df$THours*df$TSearchers),"Manhrs"))
  methods::show(paste(" "))


  methods::show (paste("Additional hours saved in search if 1 person added to each team and spacing held constant=",format(sum(df$SAVHRS),width=1),"hrs"))
  methods::show(paste(" "))

  methods::show (paste("Increased POS if 1 person added to each team and spacing decreased accordingly=",format(sum(df$dPOS),width=2),"%"))
  methods::show(paste(" "))

  methods::show(paste("First Sector to consider adding in more peeps to improve POS is Sector",posSect))
  methods::show(paste(" "))

  methods::show(paste("First Sector to consider adding in more peeps to shorten the Search hours is Sector",hrsSect))
  methods::show(paste(" "))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   # Now resort and store the data if not called by bestsearch(); if called by bestsearch() then skip this output to keep time down.
  df<-df[order(nchar(df$Sector),df$Sector),]
  df<-rtotal(df)
  dff <- format(df,width=5)
  if (!(graphs ==3))
      {   # called from best search or some one has turned off the output
          utils::write.csv(dff,filout,row.names=FALSE)
          setwd(oldwd)
          searchstatus(filout,directory=directory, STitle)
      }
  setwd(oldwd)                   # return the working directory to the old working directory
  return(df)
}


# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' @title \code{searchstatus()} - provides summary graphs of search.
#' 
#' @description \code{searchstatus()} - this function called by searchme to produce summary graphs of search for management.
#' 
#' @details \code{searchstatus()} summarizes a search in a series of graphs which display search plans. The graphs display all sectors in order of sector ladder importance or in order of POS (probability of success) expected along with other metrics critical to search managment.  The leading chart is a barchart which shows the available POS,the POS already "harvested", and the POS expected in this planning cycle.
#' Pareto charts then summarize the remainting metrics which include POS expected, POS remaining, Searchers required, duration hours planned, sectors with the highest potential to go faster with more manpower, sectors with the best potential to return POS with manpower added, man hours required, and manhours per POS. An additional page summarizes the sectors that have been set aside as "complete" and where no further search effort is planned. This function stores the charts in the R working directory or a subdirectory if one is called out through the \strong{directory} parameter
#' @author John F. Hutcheson
#' @return searchstatus returns a pdf file that contains 2 or 3 pages of graphs summarizing a search.
#' @param filout - string variable. Name of the output csv file generated by the searchme function. This file usually is named SearchOut.csv but can be changed from the default
#' by the user.  If the user renames the output file from searchme, the same name must be used here as this file is located in the working directory and
#' is used as the source of input values.
#' @param directory - string variable.  Name of the directory created and used by the searchme function to store output charts and files under the R working directory. This directory is expected to exist at the time the function is run.
#' @param STitle - string representing the name of the search for use as a title on the graphic output
#' @examples
#' \dontrun{searchstatus(STitle="Alternate Search Plan B")}
#' @export searchstatus
#' 
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# this function pulls in the output file from a previous run and analyzes it and displays several pages of graphs  summarizing the search plan
# graphs include summaries for both active sectors and sectors that will no longer be searched
# see the file "searchstatus.pdf" for examples.
searchstatus<-function(filout="SearchOut.csv",directory="Searches", STitle="")
  {
    # This function does a status report based on the file output found in the default directory
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      versn<-"vers20160312jfh"  
      platfrm <- R.version$platform
             # sets file location for mac, Windows xp or Windows 7
      oldwd <- getwd()
      if (directory!="Searches")
          {
              newdir2<-paste(oldwd,directory,sep="/")
              setwd(newdir2)
          }

      finam <- paste("Sector Status",".pdf")
      pdfval <- hutchpdf(platfrm,finam)                # turn on the PDF graphics devices
    # now read the data file with input data
      df<-utils::read.csv(filout,header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(rep(NA,24)), sep = ",",as.is =c(1,3,4,5))
    
# need to remove blank rows since some rows may be blank due to other trash in columns beyond 11
      df<-df[df$AreaCov!="",]
      dfrows<-sum(!is.na(df$AreaCov))
      datess<-date()
      df<-df[1:dfrows,]
   # should have all the data in for an analysis

  #--------------------------------------------
      graphics::par(mfrow=c(3,1))         # creates plots in 3x1 rxc
# ------------------------------------------------------------------
#   Separate out the completed sectors from the ones pending
      ypdf<-df[df$POScum>=0,]           # ypdf is the dataset of remaining sectors for search
      if (!is.na(ypdf[1,1]))
          {            # have some unfinished sectors to chart out
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing POScum, OPOScum,  and POC sorted by decreasing POC
              pocss<-order(ypdf$POC,decreasing=TRUE)
              ypdf<-ypdf[pocss,]          # sorted by decreasing POS
              mm<-as.data.frame(ypdf$POScum)
              mm$OPOScum<-ypdf$OPOScum
              mm$POC<-ypdf$POC        # now have all the data columns needed, but the dataframe needs to be made a matrix and turned on it's side
              colnames(mm)<-c("Start","Finish","Max POS")
              mm<-as.matrix(mm)
              tm<-t(mm)
              colnames(tm)<-ypdf$Sector
              p2<-graphics::barplot(tm, beside=TRUE, col=c("peru","plum","blue"),main="Starting and Ending Accumulated Prob of Success vs Max Probability of Containment (POC)",axisnames=T,legend.text=T,cex.names=1.2,las=3)
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              graphics::mtext(side=3, STitle, adj=.5, cex=1.1)
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing estimated POS sorted by decreasing POS for this period of the plan
              p3<-pareto.hutch(ypdf$POS,ypdf$Sector,main="Search POS Opportunity for this Cycle of Planning",ylab="Est Probability of Success")
 # ------------------------------------------------------------------
#   Now show a bar chart of the Sectors POS remaining sorted by decreasing POS for this period of the plan
              p14<-pareto.hutch(ypdf$POC-ypdf$POS-ypdf$POScum,ypdf$Sector,main="POS Remaining by Sector for this Cycle of Planning",ylab="Est Probability of Success ")   
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing targeted Searcher count sorted by decreasing POS for this period of the plan
              p12<-pareto.hutch(ypdf$TSearchers,ypdf$Sector,main="Searchers Required by Sector for this Cycle of Planning",ylab="Searcher Count")
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              graphics::mtext(side=3, STitle, adj=.5, cex=1.1)
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing targeted Hours sorted by decreasing POS for this period of the plan
              p13<-pareto.hutch(ypdf$THours,ypdf$Sector,main="Target Hours by Sector for this Cycle of Planning",ylab="Hours Duration of Search")
   
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing SAVHRS sorted by decreasing SAVHRS
    # sorted in order of decreasing manhours Saved if additional persons added
              p4<-pareto.hutch(ypdf$SAVHRS,ypdf$Sector,main="Best potential to Go Faster if People Added",ylab="Est Hours Duration Saved")
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing dPOS sorted by decreasing dPOS
         # sorted in order of decreasing manhours Saved if additional persons added

              p5<-pareto.hutch(ypdf$dPOS,ypdf$Sector,main="Sectors with the best potential to Increase Thoroughness if People Added",ylab="Estimated increased POS")
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              graphics::mtext(side=3, STitle, adj=.5, cex=1.1)
  # ------------------------------------------------------------------
  # sorted in order of decreasing total manhours in sector plan
              p6<-pareto.hutch(ypdf$terheads,ypdf$Sector,main="Sectors by ManHours Planned",ylab="Est ManHours of Search")
  # ------------------------------------------------------------------
  # sorted in order of decreasing total manhours in sector plan
              p7<-pareto.hutch(ypdf$terheads/ypdf$POS,ypdf$Sector,main="ManHours effort per % POS Expected",ylab="Est ManHours of Search/POS")
          }
  # _______________________

# ------------------------------------------------------------------
#   Report out the completed sectors from the ones pending
      npdf<-df[df$POScum<0,]           # npdf is the dataset of remaining sectors for search
      if (!is.na(npdf[1,1]))
          {            # have some finished seectors to chart out
# ------------------------------------------------------------------
#   Now show a bar chart of the Completed Sectors showing POScum, OPOScum,  and POC sorted by decreasing POC
              pocss<-order(npdf$POC,decreasing=TRUE)
              npdf<-npdf[pocss,]          # sorted by decreasing POC
              mm<-as.data.frame(npdf$OPOScum)
              mm$POC<-npdf$POC        # now have all the data columns needed, but the dataframe needs to be made a matrix and turned on it's side
              colnames(mm)<-c("Final","Max POS")
              mm<-as.matrix(mm)
              tm<-t(mm)
              colnames(tm)<-npdf$Sector
              p2<-graphics::barplot(tm, beside=TRUE, col=c("grey","black"),main="Final POS vs Max POS Available for Completed Sectors",axisnames=T,legend.text=T,cex.names=1.2,las=3)
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              graphics::mtext(side=3, STitle, adj=.5, cex=1.1)
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing estimated POS sorted by decreasing POS for this period of the plan
              p3<-pareto.hutch(npdf$OPOScum,npdf$Sector,main="Cumulative POS Realized from Completed Sectors",ylab="Est Overall Probability of Success Cum")
    
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing SAVHRS sorted by decreasing SAVHRS
    # sorted in order of decreasing manhours Saved if additional persons added
              p4<-pareto.hutch(npdf$POC,npdf$Sector,main="Estimated Probability of Containment for Completed Sectors",ylab="Est POC (Max POS)")
# ------------------------------------------------------------------
#   Now show a bar chart of the Sectors showing dPOS sorted by decreasing dPOS
         # sorted in order of decreasing manhours Saved if additional persons added
          }
  # _______________________
      grDevices::dev.off()
      methods::show("Status Report successfully completed.")
    
 #--------------------------------------------
    # now create individual png files for planners to examine more carefully if they choose.
      finam <- "Completed Searches Barchart.png"
    #   Now show a bar chart of the Completed Sectors showing POScum, OPOScum,  and POC sorted by decreasing POC if they exist
      if (!is.na(npdf[1,1]))
          {            # have some finished seectors to chart out
              pngint <- hutchpng(platfrm,finam)                        # turns on PNG graphics for devices
              pocss<-order(npdf$POC,decreasing=TRUE)
              npdf<-npdf[pocss,]          # sorted by decreasing POC
              mm<-as.data.frame(npdf$OPOScum)
              mm$POC<-npdf$POC        # now have all the data columns needed, but the dataframe needs to be made a matrix and turned on it's side
              colnames(mm)<-c("Final","Max POS")
              mm<-as.matrix(mm)
              tm<-t(mm)
              colnames(tm)<-npdf$Sector
              p2<-graphics::barplot(tm, beside=TRUE, col=c("grey","black"),main="Final POS vs Max POS Available for Completed Sectors",axisnames=T,legend.text=T,cex.names=1.2,las=3)    
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              graphics::mtext(side=3, STitle, adj=.5, cex=1.1)
              grDevices::dev.off()
          }
# ------------------------------------------------------------------
      finam <- "Search POS Opportunity.png"
      if (!is.na(ypdf[1,1]))
          {            # have some unfinished sectors to chart out       
              pngint <- hutchpng(platfrm,finam)
#   Now show a bar chart of the Sectors showing estimated POS sorted by decreasing POS for this period of the plan
              p3<-pareto.hutch(ypdf$POS,ypdf$Sector,main=paste(c("Search POS Opportunity for ",STitle," for this Cycle of Planning")),ylab="Est Probability of Success")
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              grDevices::dev.off()
# ------------------------------------------------------------------
              finam <- "Search POS Remaining.png"
              pngint <- hutchpng(platfrm, finam)            
#   Now show a bar chart of the Sectors POS remaining sorted by decreasing POS for this period of the plan
              p14<-pareto.hutch(ypdf$POC-ypdf$POS-ypdf$POScum,ypdf$Sector,main=paste(c("POS Remaining by Sector for ",STitle," after this Cycle of Planning")),ylab="Est Probability of Success ")
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
              grDevices::dev.off()
          }
# ------------------------------------------------------------------
      grDevices::graphics.off()   # should close all graphics devices
      if (!is.na(ypdf[1,1]))
          {            # have some unfinished sectors to chart out
              pocss<-order(ypdf$POC,decreasing=TRUE)
              ypdf<-ypdf[pocss,]          # sorted by decreasing POS
              mm<-as.data.frame(ypdf$POScum)
              mm$OPOScum<-ypdf$OPOScum
              mm$POC<-ypdf$POC        # now have all the data columns needed, but the dataframe needs to be made a matrix and turned on it's side
              colnames(mm)<-c("Start","Finish","Max POS")
              mm<-as.matrix(mm)
              tm<-t(mm)
              colnames(tm)<-ypdf$Sector
              p2<-graphics::barplot(tm, beside=TRUE, col=c("peru","plum","blue"),main="Starting and Ending Accumulated Prob of Success vs Max Probability of Containment (POC)",axisnames=T,legend.text=T,cex.names=1.2,las=3)
              graphics::grid(6,6)
              graphics::mtext(side=3,versn,adj=1,cex=.7)
              graphics::mtext(side=3,date(),adj=0,cex=.7)
          }
      setwd(oldwd)      # now have returned working directory to default
      return(2)
  }


#### this function is a simple pareto chart but with additional labels, titles, legends, and tags that I nearly always want to see
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title pareto.hutch
#### @aliases pareto.hutch
#### @description pareto.hutch - standard pareto chart but with my standard labels, titles, legends, and tags
#### @details this function is a simple pareto chart but with additional labels, titles, legends, and tags that I nearly always want to see
#### @author John F. Hutcheson
#### @param x - x axis data - sector names in this application
#### @param y - y axis data - usually hours or POS or similar here
#### @param ylab - y labels - "frequency" for this application
#### @param ylab2 - secondary y label set to "Cumulative Percentage" here
#### @param xlab - x label
#### @param cumperc - cumulative percent counts
#### @param ylim - y limits
#### @param main - main title of the pareto chart
#### @param col - colors used to highlight height of the bar
#### @param \dots - lots of neat stuff i don't care about
#### @return graphic device
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pareto.hutch<-function (x, y,ylab = "Frequency", ylab2 = "Cumulative Percentage", 
    xlab, cumperc = seq(0, 100, by = 25), ylim, main, col = grDevices::heat.colors(length(x)), 
    ...) 
{   call <- match.call(expand.dots = TRUE)
    varname <- deparse(substitute(x))
    x <- as.table(x)
    rownames(x)<-y
    if (length(dim(x)) > 1) 
        stop("only one-dimensional object (table, vector, etc.) may be provided")
    x <- sort(x, decreasing = TRUE, na.last = TRUE)
    cumsum.x <- cumsum(x)
    cumperc <- cumperc[cumperc >= 0 & cumperc <= 100]
    q <- stats::quantile(seq(0, max(cumsum.x, na.rm = TRUE)), cumperc/100)
    if (missing(xlab)) 
        xlab <- ""
    if (missing(ylim)) 
        ylim <- c(0, max(cumsum.x, na.rm = TRUE) * 1.05)
    if (missing(main)) 
        main <- paste("Pareto Chart for", varname)
    if (missing(col)) 
        col <- grDevices::heat.colors(length(x))
    w <- max(sapply(names(x), nchar))
    if (is.null(call$las)) 
        las <- 3
    else las <- call$las
    if (is.null(call$mar)) {
        if (las == 1) 
            mar <- c(0, 1, 0, 2)
        else mar <- c(log(max(w), 2), 1, 0, 2)
    }
    else mar <- call$mar
    oldpar <- graphics::par(mar = graphics::par("mar") + mar, las = las, cex = qcc::qcc.options("cex"), 
        no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    pc <- graphics::barplot(x, width = 1, space = 0.2, main = main, ylim = ylim, 
        ylab = ylab, xlab = xlab, col = col, ...)
    graphics::abline(h = q, col = "lightgrey", lty = 3)
    graphics::rect(pc - 0.5, rep(0, length(x)), pc + 0.5, x, col = col)
    graphics::lines(pc, cumsum.x, type = "b", cex = 0.7, pch = 19)
    graphics::box()
    graphics::axis(4, at = q, las = 3, labels = paste(cumperc, "%", sep = ""))
    graphics::mtext(ylab2, 4, line = 2.5, las = 3)
    tab <- cbind(x, cumsum.x, x/max(cumsum.x, na.rm = TRUE) * 
        100, cumsum.x/max(cumsum.x, na.rm = TRUE) * 100)
    colnames(tab) <- c("Frequency", "Cum.Freq.", "Percentage", 
        "Cum.Percent.")
    names(dimnames(tab)) <- c("", paste("\nPareto chart analysis for", 
        varname))
    return(as.table(tab))
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=

####  This function takes in a dataframe and a center value and returns only the rows that match the center point +/- 1.
####  These rows are teturned by this function for use in the bestsearch graphs
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title fillsensi
#### @aliases fillsensi
#### @description fillsensi - this fucntion takes in a single, limited search dataframe and center value and returns rows that match
#### @details fillsensi - this function takes in a single, limited search dataframe and returns rows that match a center value within +/- 1 value.  Used to select a row that has a running total of searchers matching a set value for use in the bestsearch function. This function works in conjunction with searchesbycenter to identify all of the matching searches.
#### @author John F. Hutcheson
#### @param dff - a limited search output dataframe containing the sector names, TotlArea, TotlPOS, TotlHeads, targeted head count and FtT values to search
#### @param center - target value to match. In this application it represents a total number of searchers that are available for a search plan.
#### @param ft - fast to thorough value used to generate the original dataframe.  Given to function to document the FtT value used for the best search solution.
#### @return ds - limited data frame that contains the sector name, total area covered, total POS obtained in the search, actual count of searchers used to make the match, and the FtT used to generate the original dataframe.
# @#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
fillsensi <- function(dff,center,ft)
{
    #  This function takes in the dataframe and a center value and returns only the rows that match the center point +/- 1.
    #  This becomes the output file sensi
   dff<-dff[order(dff$POS,decreasing=TRUE),]
   dff<-rtotal(dff)
       # should now have the right totals; subtract available peeps and form new variable for inspection
   ds <- dff
   methods::show(ds)
  
   ds$mat <- match(ds$RTotlHeads,c(center-1,center,center+1))
   ds <- ds[!is.na(ds$mat),c("Sector","TotlArea","RTotlPOS","RTotlHeads","AvgSpacing","rPOC","AvgAMDR","mat")]
   if (nrow(ds)!=0)
       {
           ds$ftt <- ft
           colnames(ds) <- c("Sector","TotlArea","RTotlPOS","RTotlHeads","AvgSpacing","rPOC","AvgAMDR","mat","FtT")
            # now added in the FtT column for this frame which will eventually be sensi
           return(ds)
       }
   else
       {
           return(NULL)
       }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### this function takes in the initial FtT value, the center target and the exisitng output file (sensi) and adds and
#### returns the output file with the appropriate lines from the dff that match
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### @title searchesbycenter
#### @aliases searchesbycenter
#### @description searchesbycenter - this function identifies all of the search ftt values that match a constrained number of searchers and creates a output file comprised of all ftt values matching those criteria.
#### @details searchesbycenter - this function orchestrates the repeated running of the searchme model to identify the optimal solution for a search plan with a constrained number of searchers. For example, if only 40 people are expected for the search, this function identifies the maximum POS and Areas that can be cleared in the search plan with this number of people.  This function identifies all of the search ftt values that match a fixed number of searchers and creates a output file comprised of each "winning" ftt value and all associated stats of interest for use in generating the bestsearch graphs. If no solution can be found for a targeted fttt value, the function returns a dataframe of zeroes to indicate no solution for use in the graphic output of bestsearch.
#### @author John F. Hutcheson
#### @param fttt - the Fast to Thorough parameter that will be searched to find a matching solution
#### @param centre - the number of searchers a solution is sought for described as AvailablePeeps in the bestsearch function.
#### @param sens - a limited search output dataframe containing the sector names, TotlArea, TotlPOS, TotlHeads, targeted head count and FtT value
#### @param dir - the directory where the results from the bestsearch function are to be stored sor storing the winning searchme output files.
#### @param stitle - string representing the name of the search for use as a title on the graphs
#### @param grphs - integer that controls the level of output from the searchme function - enter 1 for everything, 2 to get everything but the sector graphs, and 3 to skip all but the console output
#### @return sens - returns the resulting dataframe solution for the fttt target value. The dataframe may consist of zeroes if no solution is available.
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
searchesbycenter <- function(fttt, centre, sens, dir, stitle, grphs)
{
        # this function takes in the initial FtT value, the center target and the exisitng output file (sensi) and adds and
        # returns the output file with the appropriate lines from the dff that match
    dsens <- NULL     #this will be the data frame that is added to sensi
    fttti <- fttt    # above lines set up the intial values for the function
    # now need to obtain the initial dataframe for the center
    dfff <- searchme(FtT=fttti, graphs= grphs, directory=dir,filout=paste0("sens",fttti,".csv"), STitle= stitle)
    dfff<-dfff[order(dfff$POS,decreasing = TRUE),]  # sort the dataframe so it's in POS decreasing order
    
    dsens <- fillsensi(dfff,centre,fttti)
   
    while (is.null(dsens))
        {
            fttti <- fttti-.05
            dfff <- searchme(FtT=fttti,graphs=grphs,directory=dir,filout=paste0("sens",fttti,".csv"), STitle= stitle)
# creates a new dataframe to evaluate using a new FtT value
            dsens <- fillsensi(dfff,centre,fttti)
            if (fttti <= (fttt-.6))
               {
                   #gone too low and need to stop search
                   break
               }
        }
    if (is.null(dsens))
        {
  # didn't find a solution for this FtT value
            dsens <- data.frame("None",0,0,0,0,0,0,0,fttt)
        }  
    colnames(dsens)<-c("Sector","TotlArea","TotlPOS","TotlHeads","AvgSpacing","rPOC","AvgAMDR","mat","FtT")

    sens <- rbind(sens,dsens)               # fill line with indicator that we bombed out for this FtT value 
    return(sens)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' @title \code{bestsearch} - Optimization of Search for Fixed Number of Searchers
#'
#' @description \code{bestsearch()} This function takes a fixed number of searchers and reports the area and POS that could be covered by them.
#' 
#' @details bestsearch() is an optimization function which is designed to examine the ways that a limited number of searchers can be used to search a given search field. The function repeatedly runs \code{searchme()} and captures the results graphically, displaying the area covered, the maximum POS available, the average AMDR, average team spacing used on the field, and an inflated AMDR value.
#' The inflated AMDR value is the value the measured AMDR would need to climb to in order to return the highest observed POS for the search plan. This becomes a measure of the 'risk' of missing the subject if a faster search method is used. The AMDR, which is a strictly visual measure of optimal team spacing, can be inflated if the subject can be assumed to be responsive or mobile. This function helps the management team identify the best plan to use for a limited number of searchers.  It becomes the discussion vehicle for how fast to search in a search plan. The function creates a graph displaying the expected search area covered and POS returned for searches across a span of searcher spacings and sector headcounts. 
#' @author John F. Hutcheson
#' @param AvailablePeeps - expected headcount of searchers for this operational period
#' @param sdirectory - the sub directory within the search directory where the results will be stored for this 'sensitivity' analysis.  Defaults to the "Sensistivity" subdirectory. The function will create a new subdirectory if one doesn't already exist.
#' @param FTt - vector of Fast to Thorough parameters to search for this solution. FTt is a parametric version of the relative spacing between team members. Defaults to range of integers 1 thru 5. 1 represents a very Loose Grid search. 5 represents a tight grid approaching.
#' @param STitle - string representing the name of the search for use as a title on the graphic output
#' @param graphs - integer that controls the level of output from the searchme function - enter 1 for everything, 2 to get everything but the sector graphs, and 3 to skip all but the console output
#' @inheritParams searchesbycenter
#' @inheritParams ggplot2
#' @inheritParams sensei
#' @export bestsearch
#' @return This function returns a pdf file containing two graphs in the R working directory.
#' The first graph shows the area covered and POS achieved as a function of the FtT used to plan the search.
#' The second graph shows the average spacing for the area covered, the average AMDR used for the plan, and the average inflated AMDR. The average inflated AMDR
#' is the amount the AMDR would have to increase to increase the POS achieved to the highest observed value in the analysis.
#' @examples
#' \dontrun{bestsearch(AvailablePeeps=84, STitle="HighTurnout Alternatives", graphs= 3)}
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    # this function lives to repeatedly run searchme to find best opportunity for limited
    # number of searchers and to become the discussion vehicle for how fast to search in a search plan

bestsearch<-function (AvailablePeeps=25,sdirectory="Sensitivity",FTt=c(1,2,3,4,5), STitle="Search Name", graphs=2)
{
    # this function lives to repeatedly run searchme to find best opportunity for limited
    # number of searchers
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    newwrd<-"Sensitivity"
    versn<-"vers20160312jfh"  
    platfrm <- R.version$platform
    olddir <- getwd()
 #   dir.create(sdirectory)
 #   newdir2<-paste(olddir,sdirectory,sep="/")
 # now have the new subdirectory created below the working directory

# Now run searchme() with constrained FTT and to a specific Output file for this purpose
    
    # now read the data file with input data
    finam <- paste0("Sensitivity for fixed Headcount",".pdf")

    # now run searchme and for each FtT case and label and place the output in the new directory
# ftt are the ftt points we want to examine    
    ftt<-FTt    
    sensi<- data.frame("start",99,99,99,99,99,99,99,99)
    colnames(sensi) <- c("Sector","TotlArea","TotlPOS","TotlHeads","AvgSpacing","rPOC","AvgAMDR","mat","FtT")
 # start looping to find the rows that match for given FtTs
    sensi <- searchesbycenter(fttt=ftt[1],centre=AvailablePeeps,sens=sensi,dir=sdirectory, STitle, grphs = graphs)
    sensi <- searchesbycenter(fttt=ftt[2],centre=AvailablePeeps,sens=sensi,dir=sdirectory, STitle, grphs = graphs)    
    sensi <- searchesbycenter(fttt=ftt[3],centre=AvailablePeeps,sens=sensi,dir=sdirectory, STitle, grphs = graphs)
    sensi <- searchesbycenter(fttt=ftt[4],centre=AvailablePeeps,sens=sensi,dir=sdirectory, STitle, grphs = graphs)
    sensi <- searchesbycenter(fttt=ftt[5],centre=AvailablePeeps,sens=sensi,dir=sdirectory, STitle, grphs = graphs)
    sensi <- sensi[2:nrow(sensi),]          #this drops the first row of sensi
    methods::show(sensi)
    
    methods::show(ftt)
    maxpos <- max(sensi$TotlPOS)
    sensi$InfAMDR <- rAMDR(sensi$rPOC,maxpos,sensi$AvgSpacing)
    nansens <- sensi[sensi$InfAMDR=="NaN",]
    if (!is.na(nansens[1,1]))
        {
            nonansens <- sensi[sensi$InfAMDR!="NaN",]
            nansens$InfAMDR <- 0        # have zeroed out the not a number values
            sensi <- rbind (nonansens,nansens)
        }
    sensi <- sensi[order(sensi$AvgSpacing,decreasing=TRUE),]
 # now have the data files resorted and ready to plot
    # now plot the data
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
    pdfval <- hutchpdf(platfrm,finam)        # sets up the graphic file
    lattice::trellis.par.set(list(par.main.text=list(lineheight=2)))     # sets grpahics title heights    

    tpos<-sensi$TotlPOS
    tar<-sensi$TotlArea

    sensi$HeadFactor <- factor(sensi$TotlHeads)
    ymax <- sensi$TotlArea[1]+7
    ymin <- sensi$TotlPOS[1]-7
    
    g<-ggplot2::ggplot(data= sensi,ggplot2::aes(x=sensi$FtT,y=sensi$TotlArea,colour=sensi$HeadFactor))
    g <- g+ggplot2::geom_line(ggplot2::aes(y=sensi$TotlPOS),color="red")+ggplot2::geom_line(ggplot2::aes(y=sensi$TotlArea),color="blue")

    g <- g+ggplot2::labs(title=paste0("Total Area Covered and Total POS accumulated by ",AvailablePeeps," volunteers"),x="FtT used to achieve goal",y="Total Area covered or Total POS accumulated",legend="Totals",subtitle="Select the FtT Setting that best meets conditions and your resources")
    g <- g+ggplot2::geom_point()+ggplot2::annotate("text",x=2,y=ymin,label="Total POS", family="serif",fontface="italic",colour="darkred",size=4)
    g <- g+ggplot2::annotate("text",x=2,y=ymax,label="Total Area Covered",family="serif",fontface="italic",colour="darkblue",size=4)
    g <- g+ggplot2::annotate("text",x=4.3,y=ymax,label=date(),size=3)+ggplot2::annotate("text",x=4.3,y=ymin,label=versn,size=3) + ggplot2::annotate("text", x=3, y=ymax, label=STitle, size=4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    print(g)
    ymax <- sensi$AvgSpacing[2]+1
    ymin <- sensi$InfAMDR[2]+1
    yminaux <- sensi$AvgAMDR[2]-2
    gg <- ggplot2::ggplot(sensi,ggplot2::aes(x=sensi$FtT,y=sensi$AvgSpacing,color="green"))+ ggplot2::guides(guide_legend=NULL)
    gg <- gg + ggplot2::geom_line(ggplot2::aes(y=sensi$AvgSpacing),color="green")+ ggplot2::geom_line(ggplot2::aes(y=sensi$InfAMDR),color="brown")+ ggplot2::geom_line(ggplot2::aes(y=sensi$AvgAMDR),color="black")
    gg <- gg+ggplot2::labs(title=paste0("Average Spacing and Inflated AMDR for ",AvailablePeeps," volunteers"),x="FtT used to achieve goal",y="Avg Spacing Used or Inflated AMDR Required to Equal Best POS")
    gg <- gg+ggplot2::geom_point()+ggplot2::annotate("text",x=2,y=ymax,label="Average Spacing ", family="serif",fontface="italic",colour="green",size=4)
    gg <- gg+ggplot2::annotate("text",x=2,y=ymin,label="Inflated AMDR",family="serif",fontface="italic",colour="brown",size=4)
    gg <- gg+ggplot2::annotate("text",x=2,y=yminaux,label="Average Planned AMDR",family="serif",fontface="italic",colour="black",size=4)
    gg <- gg+ggplot2::annotate("text",x=4.3,y=ymax,label=date(),size=3)+ggplot2::annotate("text",x=4.3,y=ymin,label=versn,size=3)+ggplot2::annotate("text",x=3,y=yminaux+1,label="Ignore red line to the right of cross over with AMDR line",size=3)+ ggplot2::annotate("text", x=3, y=ymax, label=STitle, size=4)
   
    print(gg)
    grDevices::dev.off()
    grDevices::graphics.off()
    graphics::plot(gg)
    setwd(olddir)
    return(1)
  
}
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' Create, modify, critique, and report against search and rescue plans using \pkg{rSARP}.
#'
#' rSARP allows you to create and manage search and rescue search grid plans using CSV
#' files and the graphic output from R.  The package includes 3 main functions.  The
#' \code{searchme()} function provides modeling by sector and is the core of the package.
#' The \code{searchstatus()} function provides graphic reporting against the core plan and is
#' typically used several times within a planning cycle as plans are modified. The
#' \code{bestsearch()} function is used a the beginning of a planning cycle to
#' determine the best search methods to use for a limited number of searchers. The \code{tracking()}
#' function is used to track task assignments (525s) as they are assigned and completed.
#'
#' The first function you're likely to need from \pkg{rSARP} is
#' \code{\link{searchme}}. Otherwise refer to the package documentation to see
#' how to proceed.
#'
#' @docType package
#' @name rSARP
NULL


# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' SearchInput.csv - typical Data input file for \pkg{rSARP}
#' 
#' A dataset containing the sectors or search areas and key attributes of each
#' that will be searched. This file is stored in the working directory of R and
#' must be named SearchInput.csv. The variables described in the file are as follows:
#'
#' \itemize{
#'   \item Sector. Name of the sector or grid to be searched.  Usually Alpha characters excluding I and O.
#'   \item Area. Area of the sector to be searched in Hectares or Acres.  Defaults to Hectares and requires parametric change in call to \code{searchme()} if Acres is used.
#'   \item AreaCoverage. Percentage of the the sector Area that was or will be covered. Typically used if the search teams are unable to
#' cover the entire sector due to unforeseen hazards in the sector. If the value given is negative, then it signifies that portion of the area is being
#' searched for the first time, bypassing bayesian treatments, even if the sector has a POScum value (indicating the a portion of the sector was
#' previously searched.
#'   \item Terrain. Keyword describing the terrain of the sector which can be used to provide the expected speed of travel and estimated AMDR.
#' Currently limited to use of a set of keywords. Use HeavyWoods, OpenWoods, OpenArea, SteepTerrain, ModerateTerrain, FlatArea, Urban. See guide for further specifics.
#'   \item TOD. Time of Day.  Keyword describing time of search and currently limited to Day or Night.
#'   \item WX. Weather keyword describing conditions during the search.  Currently limited to Normal or Snow6to8in.
#'   \item THours. Target duration Hours.  Hours team will be deployed for the search in the field not counting breaks or transportation time.
#'         If -1 entered for this value program will optimize using internal algorithms.
#'   \item TSearchers. Target Searchers.  Count of searchers, excluding non-searching supervisors, in the field assigned to that sector.
#'         If -1 entered for this value program will optimize using internal algorithms.
#'   \item TSpacing. Target Spacing.  The distance in meters between searchers planned for this sector search.
#'          If -1 entered for this value program will optimize using internal algorithms.
#'   \item AMDR. Average Maximum Detection Range in meters.  Initially estimated, then updated as measured AMDRs are reported in.
#'          If -1 entered for this value program will estimate based on Terrain values
#'   \item Rank. Total rank of the sector from sector ladder ranking exercise.  Use raw score totals - software will normalize and handle appropriately.
#'   \item POScum. Cumulative Probability of Success for the sector in percentage points. Represents the POS that has already been 'harvested'
#' for that area.  If entered as negative number, will revise reports to indicate that no further searching is planned for this sector.
#'   \item TerrainWords. Listing of the key words which can be used in Terrain values.
#'   \item TODWords. Listing of the key words which can be used in TOD values.
#'   \item WXWords.  Listing of the key words which can be used in WX values.
#' }
#'
#' @format A data frame with 32 rows and 15 variables
#' @source John Hutcheson
#' @name SearchInput.csv
NULL
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' SearchOut.csv - typical Data input file for \pkg{rSARP}
#' 
#' A dataset containing the sectors or search segments and key attributes of each
#' that will be searched, including the outputs. The variables are as follows:
#'
#' \itemize{
#'   \item Sector. Name of the sector or grid to be searched.  Usually Alpha characters excluding I and O.
#'   \item Area. Area of the sector to be searched in Hectares or Acres.  Defaults to Hectares and requires parametric change in call to \code{searchme()} if Acres is used.
#'   \item AreaCoverage. Percentage of the the sector Area that was or will be covered. Typically used if the search teams are unable to
#' execute due to unforeseen hazards in the sector. If the value being given is negative, then the area is being searched for
#' the first time and bayesian treatments are skipped.
#'   \item Terrain. Keyword describing the terrain of the sector. Limited to use of a limited set of keywords. See guide for further specifics.
#'   \item TOD. Time of Day.  Keyword describing time of search and currently limited to Day or Night.
#'   \item WX. Weather keyword describing conditions during the search.  Currently limited to Normal or Snow6to8in.
#'   \item THours. Target duration Hours.  Hours team will be deployed for the search in the field not counting breaks or transportation time.
#'         If -1 entered for this value program will optimize using internal algorithms.
#'   \item TSearchers. Target Searchers.  Count of searchers, excluding non-searching supervisors, in the field assigned to that sector.
#'         If -1 entered for this value program will optimize using internal algorithms.
#'   \item TSpacing. Target Spacing.  The distance in meters between searchers planned for this sector search.
#'          If -1 entered for this value program will optimize using internal algorithms.
#'   \item AMDR. Average Maximum Detection Range in meters.  Initially estimated, then updated as measured AMDRs are reported in.
#'          If -1 entered for this value program will estimate based on Terrain values
#'   \item Rank. Total rank of the sector from sector ladder ranking exercise.  Use raw scores - software will normalize and handle appropriately.
#'   \item POScum. Cumulative Probability of Success for the sector in percentage points. Represents the POS that has already been 'harvested'
#' for that area.  If entered as negative number, will revise reports to indicate that no further searching is planned for this sector.
#'   \item fnCovered. Fraction of the planned area covered by the search.
#'   \item fnMissed. Fraction of the planned area missed by the search.
#'   \item AreaCov. Area Covered.  Area of the search seactor actually covered in Hectares or Acres.
#'   \item POC. Probability of Containment.  The normalized sector ranking.
#'   \item terspd. Terrain Speed. The estimated ground speed for the team in miles per hour based on terrain or given by planner constraints.
#'   \item terheads. Terrain Manhours. The estimated man hours to search the sector given spacing and speed constraints.
#'   \item FirstPass. Parameter indicating if the area to be cleared has already been searched or will be searched for the first time.
#'   \item ROThrs. Rule of Thumb hours. Estimated search man hours based on Model used in Kent County, primarily for urban search efforts.
#'   \item NHRSH. NASAR Man hours. Estimated man hours using NASAR rule of thumb.
#'   \item SAVHRS. Duration search hours saved if one additional person added to team to 'lengthen the line'. 
#'   \item ESW. Effective spacing width. Currently 1.5 times the AMDR but technically the length of Coverage in meters. Used to determine COV.
#'   \item COV. Coverage as a fraction. The TSpacing divided by ESW. Used to determine POD.
#'   \item POD. Probability of Detection as a fraction. Likelihood of detecting the object in the sector if the object is present.
#'   \item delspc. The change in spacing between team members if an additional person were added to the line but the overall length kept the same.
#'   \item dPOD. The change in the Probability of Detection that would result if the team spacing were changed by delspc.
#'   \item POS. Probability of Success. The probability that the subject would be found if present in that sector as a percent.
#'   \item dPOS. The change in POS in percent if the spacing between team members is changed by delspc.
#'   \item OPOScum. Overall cumulative Probability of Success. The running tally on POS by sector including bayiesian corrections.
#'   \item RTotlHeads. Running total heads. The number of searchers required to as a running total by sector to execute the plan.
#'   \item RTotlPOS. Running total POS. The percentage of POS accumulated if the plan is executed by sector as a running total.
#'   \item TotlArea. The running total area covered by this search plan in Hectares or Acres.
#'   \item AvgSpacing. The area averaged spacing between team members for all sectors this row and above.
#'   \item rPOC. The running Probability of Containment for all sectors this row and above.
#'   \item AvgAMDR. The running area averaged AMDR for all sectors this row and above. 
#'   \item POSm. THe probability of success that was missed due to the area missed.
#' }
#'
#' @format A data frame with 32 rows and 38 variables
#' @source John Hutcheson
#' @name SearchOut.csv
NULL


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# @#@#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' @title \code{tracking} - Tracking 525s completion within a Search
#'
#' @description \code{tracking()} This function lives to track 525 progress towards completion for a search. Running this function after the tracking.csv
#' file has been updated produces a graph that displays the progress of search by 525. 
#' @details tracking() is a tracking function built to track 525 progress graphically for reports to the SM / PIO. The height of each bar defines the number of searchers
#' working on this task. The location of the triangle relative to the top of the bar indicates the % completion of the task, so if the triangle is half way up the bar, the task is 50% complete.
#' @author John F. Hutcheson
#' @param title - string that represents the Name of the Search. Used to label the graph with the search name.
#' @param directory - a string representing the sub directory within the search directory where the results will be stored for this analysis.  Defaults to the R working directory. The function will create a new directory if the default is not accepted.
#' @examples
#' tracking(title="Midland County Search B")
#' @export tracking
#' @return This function returns a graphic showing a bar chart analysis of 525s within a search by area covered and POS achieved
# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

tracking<-function (title="",directory="Searches")
{
    versn <- c("version 20160312jfh")
 # get rid of any old variables and data files
    oldwd <- getwd()            # store the old working directory in oldwd
 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Easy to reach factors for the models that place the calculated hours between NASAR and Kent Models
      
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    platfrm <- R.version$platform

  # first retrieve the example SearchInput data from the package to help the user create a template if they don't have one already
  trackingtemplate <- utils::read.csv(system.file("extdata","Tracking.csv", package = "rSARP"),header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA,NA),sep=",",as.is=c(1,4,5))
  
  # now read the data file with input data # as.is columns are text read as text not assume to be factors from the working directory
  if(!(file.exists("Tracking.csv")))
       {     # no file in place so create that template
      utils::write.table(trackingtemplate,"Tracking.csv",row.names=FALSE, sep=",", col.names=TRUE, quote=TRUE)   # create a new file for the user where it belongs
      print(paste("You didn't save a tracking File in your working Directory where it belongs, so I saved one there under      Tracking.csv.  If you thought you had one there please check your working directory " , oldwd, ". It could also be that your working directory needs to be changed.  Please check for errors in naming the file. Please revise the file I just saved there accordingly and then run tracking() again."))
      stop
  }
    # now read the data file with input data # as.is columns are text read as text not assume to be factors from the working directory

    df<-utils::read.csv("Tracking.csv",header=T,fill=F,blank.lines.skip=T,flush=T,colClasses=c(NA,NA,NA,NA,NA), sep = ",",as.is =c(1,4,5))
    
    
    if (directory!="Searches")
        {   dir.create(directory)
            newdir2<-paste(oldwd,directory,sep="/")
            setwd(newdir2)
        }
      # THis allows a subdirectory to be created to hold the output files  if directory is changed from "Searches"
      # colClasses may have skipped the column, but still expecting rows down to that level
      # remove columns no longer needed
    df <- df[,1:5]         # only take columns 1 thru 12
  
      # need to remove blank rows since some rows may be blank due to other trash in columns beyond 11
    df <- df[df$Task!="",]
    dfrows <- sum(!is.na(df$Task))

    df <- df[1:dfrows,]
# ------------------------------------------------------------------
    # Check to insure the data cloumns are there and properly named
    if (!is.element('TSearchers',stats::variable.names(df)))
        stop("Missing TSearchers data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
    if (!is.element('Task',stats::variable.names(df)))
        stop("Missing Task data or not properly named- please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
    if (!is.element('PerCentComplete',stats::variable.names(df)))
        stop("Missing PerCentComplete data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
    if (!is.element('Type',stats::variable.names(df)))
        stop("Missing Type data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
    if (!is.element('Comment',stats::variable.names(df)))
        stop("Missing Comment data or not properly named - please provide estimates of the total searchers (excluding supervisors) planned for assignment to the sector")
 
# -----------------------------------------------------------------   
    
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # now read the data file with input data
    finam <- paste0("Tracking ",title,".pdf")

    # now run searchme and for each FtT case and label and place the output in the new directory
# ftt are the ftt points we want to examine    
    maxht <- max(df$TSearchers)
  # will scale the bar chart to the max number of searchers planned
    df$compl<-df$PerCentComplete * df$TSearchers/100
 # this scales the percentcomplete variable to maxht so y scale does both percentcomplete and heads
 
 # now have the data files resorted and ready to plot
    # now plot the data
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
    pdfval <- hutchpdf(platfrm,finam)        # sets up the graphic file
    lattice::trellis.par.set(list(par.main.text=list(lineheight=2)))     # sets grpahics title heights    
    maxrow<-nrow(df)
    ymax <- maxht
    ymin <- 0
    df$ro<-c(1:maxrow)    
    g<-ggplot2::ggplot(df,ggplot2::aes(x=df$ro,y=df$TSearchers,fill=factor(df$Type)))
    g <- g+ggplot2::geom_bar(stat="identity")+ggplot2::geom_point(data=df,mapping=ggplot2::aes(x=df$ro,y=df$compl),shape=6,size=3)

    g <- g+ggplot2::labs(title=paste0("Search Task Progress for ",title," by Task"),x="525's Issued",y="% Complete or Head Count",legend="Search Type",subtitle="Tracking Search Progress")
    g <- g+ggplot2::scale_x_discrete(df$Track)
    g <- g+ggplot2::annotate("text",x=4.3,y=ymax,label=date(),size=3)+ggplot2::annotate("text",x=4.3,y=ymax-.5,label=versn,size=3)
    g <- g+ggplot2::annotate("text",x=df$ro,y=-.5,label=df$Task)
    g <- g+ggplot2::theme_bw()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    print(g)

    grDevices::dev.off()
    grDevices::graphics.off()
    graphics::plot(g)
    setwd(oldwd)
    return(1)
  
}

# @#@#@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Tracking.csv - typical Data input file for \code{tracking()}
#' 
#' A dataset containing the names or assignment numbers of the 525 tasks assigned
#' within a search, the type of task, the number of people assigned, and the %
#' completion of the task.  This file is stored in the working directory of R and
#' must be named Tracking.csv. The variables described in the file are as follows:
#'
#' \itemize{
#'   \item Task. Name or assignment number of the task.  Usually Alpha characters excluding I and O if search sector names are used.
#'   \item PerCentComplete. Relative task completion amount expressed as a percentage. An entry of 25 indicates the task is one quarter complete.
#'   \item TSearchers. Number of persons assigned to this task.
#'   \item Type. String representing the category of task this assignment belongs to.  Generally use Initial, Loose, and Tight types, but the user is free to create sub categories as needed.
#'   \item Comment. Time of Day.  Keyword describing time of search and currently limited to Day or Night.
#' }
#'
#' @format A data frame with 33 rows and 5 variables
#' @source John Hutcheson
#' @name Tracking.csv
NULL
