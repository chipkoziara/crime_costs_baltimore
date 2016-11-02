################
# housekeeping #
################

# load required packages
library(tidyverse)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)
library(tidyr)
library(dplyr)

# function for calculating Purchasing Power of dollars in different years
# based on Purchasing Power section from http://www.bls.gov/cpi/cpimathfs.pdf
cpi.adjust <- function(amount, amount.year, desired_year) {
        
        u <- "http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv"
        
        if(!file.exists(basename(u))) {
        
                download.file(u, file.path("./inputs", basename(u)))
        }
        
        monthly_cpi <- read.csv("./inputs/CPIAUCSL.csv", header = TRUE) 
        
        monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
        
        # averages annual CPI from the monthly CPIs 
        yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))
        
        # which year's dollars do you want? this is the amount.base_year
        yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == desired_year]
        
        # find index for the year you have your dollars in
        year.index <- yearly_cpi$cpi_year[length(yearly_cpi$adj_factor)] - amount.year
        
        # Find the adjustment factor for the year you have your dollars in
        adjustment.factor <- yearly_cpi$adj_factor[length(yearly_cpi$adj_factor)-year.index]
        
        cpi_adjustment <- amount/adjustment.factor
        
        return(cpi_adjustment)
}

# inputs 
setwd("~/Dropbox/All_Teh_Filez/code/R/cost_of_crime/baltimore/")
# directory for crime csv [ADD A TEMPORAL ELEMENT]
csv_location = path.expand("./inputs/baltimore_crime_2015.csv")

# directories for input shapefiles
blockgroups_shp_directory = path.expand("./inputs/Baltimore_Block_Groups/Baltimore_Block_Groups.shp")

# names of input shapefile layers
blockgroups_shp_name = "Baltimore_Block_Groups"

# outputs

# directory for output shapefiles
output_directory = path.expand("./outputs") 

# names of output shapefile layers
blockgroups_output_layer = "Baltimore_Block_Groups"

# read in crime data
crimes_raw <- read_csv(csv_location)

# read in shapefile of blockgroups
blockgroups_shp <- readOGR(blockgroups_shp_directory, blockgroups_shp_name)

# save crime data as a data frame table
crimes <- tbl_df(crimes_raw)

# separate out violent crime
vc <- c('HOMICIDE', 'SHOOTING')
violent_crime <- crimes %>% filter(Description %in% vc)
crimes <- crimes %>% filter(!Description %in% vc) 
# fix times for violent crime
violent_crime$CrimeTime <- paste(violent_crime$CrimeTime, "00")
# merge violent crime back in
crimes <- rbind(crimes, violent_crime)
# create datetime column
crimes$datetime <- paste(crimes$CrimeDate, " ", crimes$CrimeTime)
crimes$datetime <- mdy_hms(crimes$datetime)
# only include 2015 crime
crimes <- filter(crimes, datetime < "2016-01-01")

# reformat crimes to contain datetime, id, xcoord, ycoord, classification
id <- rownames(crimes)
crimes <- cbind(id=id, crimes)
crimes$report_time <- crimes$datetime
crimes$class <- crimes$Description
crimes$`Location 1` <- gsub('\\(', '', crimes$`Location 1`)
crimes$`Location 1` <- gsub('\\)', '', crimes$`Location 1`)
crimes <- crimes %>% separate('Location 1', c("pointy", "pointx"), ", ")
crimes$pointx <- as.numeric(crimes$pointx)
crimes$pointy <- as.numeric(crimes$pointy)
# remove crimes that occur WAY outside of Baltimore
crimes <- crimes %>% filter(pointy < 40)
crimes$datasource <- "data.baltimorecity.gov"
# reformat to only include the columns we want
crimes <- dplyr::select(crimes, id, report_time, class, pointx, pointy, datasource)

######################
# prepare crime data #
######################

# combine classes 
# using http://www.rand.org/jie/centers/quality-policing/cost-of-crime.html

# murder
crimes$class <- gsub(".*HOMICIDE.*", "Murder", crimes$class)

# rape
crimes$class <- gsub("^RAPE$", "Rape", crimes$class)

# robbery
crimes$class <- gsub(".*ROBBERY.*", "Robbery", crimes$class)

# aggravated_assault
crimes$class <- gsub(".*ASSAULT.*", "Aggravated Assault", crimes$class)

# burglary
crimes$class <- gsub("^BURGLARY$", "Burglary", crimes$class)

# larceny 
crimes$class <- gsub("^LARCENY$", "Larceny", crimes$class)

# motor vehicle theft
crimes$class <- gsub("AUTO THEFT", "Motor Vehicle Theft", crimes$class)

# theft from motor vehicle
crimes$class <- gsub("LARCENY FROM AUTO", "Theft From Vehicle", crimes$class)

# arson
crimes$class <- gsub("ARSON", "Arson", crimes$class)

# shooting (group with aggravated assault)
crimes$class <- gsub("SHOOTING", "Aggravated Assault", crimes$class)

# select only the crime classes we want now
crime_type <- c("Murder", "Rape", "Aggravated Assault", "Arson", "Robbery", "Burglary", 
                "Motor Vehicle Theft", "Larceny", "Theft From Vehicle")

crimes <- crimes %>% filter(class %in% crime_type)

# remove rows with no coordinates

# show how many are NA by crime type
crimes.na <- crimes %>% filter(is.na(pointx))

# remove NAs
crimes <- crimes %>% filter(!is.na(pointx))

################
# set weights #
###############

# arson cost from Miller study (1996) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2835847/
# in 1993 dollars
arson.1993 <- 53629
# cpi adjust 
arson.2007 <- cpi.adjust(arson.1993, 1993, 2007)

# create a vector with cost per crime
# using http://www.rand.org/jie/centers/quality-policing/cost-of-crime.html
# Theft from Vehicle is set to same value as "Larceny"

# in order, Murder, Rape, Aggravated Assault, Robbery, Burglary, 
# Motor Vehicle Theft, Larceny, Theft From Vehicle

murder.2007 <- 8649216
rape.2007 <- 217866
aggravated_assault.2007 <- 87238
robbery.2007 <- 67277
burglary.2007 <- 13096
motor_vehicle_theft.2007 <- 9079
larceny.2007 <- 2139
theft_from_vehicle.2007 <- 2139

cost.2007 <- c(murder.2007, rape.2007, aggravated_assault.2007, arson.2007, robbery.2007, 
               burglary.2007, motor_vehicle_theft.2007, larceny.2007, theft_from_vehicle.2007)

# create a table with crime types and cost per crime
cost_of_crime <- data.frame(crime_type, cost.2007)
cost_of_crime <- tbl_df(cost_of_crime)
# order/sort by severity
cost_of_crime <- cost_of_crime[with(cost_of_crime, order(-cost.2007)), ]

# convert cost_of_crime to 2015 dollars
cost_of_crime$cost.2015 <- cpi.adjust(cost_of_crime$cost.2007, 2007, 2015)

#
# MORE DATA PREP
#

# convert 'crimes' data to a spatial points data frame
crimes <- as.data.frame(crimes)
coordinates(crimes)<-~pointx+pointy

# set projection for crime data - make sure this is correct! Assumes crime data is in WGS 84
proj4string(crimes) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# reproject to WGS 84

# 'crimes' is already in WGS 84. Uncomment below if this is not true
# crimes <- spTransform(crimes, CRSobj = CRS(proj4string(blockgroups_shp)))
blockgroups_shp <- spTransform(blockgroups_shp, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# separate out the different crime types
crimes.murder <- subset(crimes, class == "Murder")
crimes.rape <- subset(crimes, class == "Rape")
crimes.aggravated_assault <- subset(crimes, class == "Aggravated Assault")
crimes.arson <- subset(crimes, class == "Arson")
crimes.robbery <- subset(crimes, class == "Robbery")
crimes.burglary <- subset(crimes, class == "Burglary")
crimes.motor_vehicle_theft <- subset(crimes, class == "Motor Vehicle Theft")
crimes.larceny <- subset(crimes, class == "Larceny")
crimes.theft_from_vehicle <- subset(crimes, class == "Theft From Vehicle")

# sets field names for Total Crime Cost Calculation
crime_cost_field_names <- c("murd_cost",
                            "rape_cost",
                            "agg_cost",
                            "ars_cost",
                            "rob_cost",
                            "bur_cost",
                            "mvt_cost",
                            "larc_cost",
                            "tfv_cost")

# sets field names for Percentage of Cost of Crime
perc_field_names <- c("murd_perc",
                      "rape_perc",
                      "agg_perc",
                      "ars_perc",
                      "rob_perc",
                      "bur_perc",
                      "mvt_perc",
                      "larc_perc",
                      "tfv_perc")

# sets crime count field names
crime_count_field_names <- c("murd_num", 
                             "rape_num",
                             "agg_num",
                             "ars_num",
                             "rob_num", 
                             "bur_num", 
                             "mvt_num",
                             "larc_num", 
                             "tfv_num")

# crime type labels
crime_labels <- c("Murder", 
                  "Rape", 
                  "Aggravated Assault",
                  "Arson",
                  "Robbery", 
                  "Burglary", 
                  "Motor Vehicle Theft",
                  "Larceny", 
                  "Theft From Vehicle", 
                  "No Events")

#
#
#
# blockgroups
#
#
#

#################################
# tabulate counts per geography #
#################################

# output shapefile with counts per geographic area

# counts of each crime type per blockgroup

counts.murder <- over(crimes.murder, blockgroups_shp)
counts.murder.na <- crimes.murder[is.na(counts.murder$GEOID),]
counts.murder.table <- as.data.frame(table(counts.murder$GEOID))
colnames(counts.murder.table) <- c("GEOID", "murd_num")

counts.rape <- over(crimes.rape, blockgroups_shp)
counts.rape.na <- crimes.rape[is.na(counts.rape$GEOID),]
counts.rape.table <- as.data.frame(table(counts.rape$GEOID))
colnames(counts.rape.table) <- c("GEOID", "rape_num")

counts.aggravated_assault <- over(crimes.aggravated_assault, blockgroups_shp)
counts.aggravated_assault.na <- crimes.aggravated_assault[is.na(counts.aggravated_assault$GEOID),]
counts.aggravated_assault.table <- as.data.frame(table(counts.aggravated_assault$GEOID))
colnames(counts.aggravated_assault.table) <- c("GEOID", "agg_num")

counts.arson <- over(crimes.arson, blockgroups_shp)
counts.arson.na <- crimes.arson[is.na(counts.arson$GEOID),]
counts.arson.table <- as.data.frame(table(counts.arson$GEOID))
colnames(counts.arson.table) <- c("GEOID", "ars_num")

counts.robbery <- over(crimes.robbery, blockgroups_shp)
counts.robbery.na <- crimes.robbery[is.na(counts.robbery$GEOID),]
counts.robbery.table <- as.data.frame(table(counts.robbery$GEOID))
colnames(counts.robbery.table) <- c("GEOID", "rob_num")

counts.burglary <- over(crimes.burglary, blockgroups_shp)
counts.burglary.na <- crimes.burglary[is.na(counts.burglary$GEOID),]
counts.burglary.table <- as.data.frame(table(counts.burglary$GEOID))
colnames(counts.burglary.table) <- c("GEOID", "bur_num")

counts.motor_vehicle_theft <- over(crimes.motor_vehicle_theft, blockgroups_shp)
counts.motor_vehicle_theft.na <- crimes.motor_vehicle_theft[is.na(counts.motor_vehicle_theft$GEOID),]
counts.motor_vehicle_theft.table <- as.data.frame(table(counts.motor_vehicle_theft$GEOID))
colnames(counts.motor_vehicle_theft.table) <- c("GEOID", "mvt_num")

counts.larceny <- over(crimes.larceny, blockgroups_shp)
counts.larceny.na <- crimes.larceny[is.na(counts.larceny$GEOID),]
counts.larceny.table <- as.data.frame(table(counts.larceny$GEOID))
colnames(counts.larceny.table) <- c("GEOID", "larc_num")

counts.theft_from_vehicle <- over(crimes.theft_from_vehicle, blockgroups_shp)
counts.theft_from_vehicle.na <- crimes.theft_from_vehicle[is.na(counts.theft_from_vehicle$GEOID),]
counts.theft_from_vehicle.table <- as.data.frame(table(counts.theft_from_vehicle$GEOID))
colnames(counts.theft_from_vehicle.table) <- c("GEOID", "tfv_num")

###### INSERTS POINTS TO NEAREST POLYGON 
###### RETURNS TABLE WITH TOTAL COUNTS INCLUDING THESE NEW POINTS

pts.to.nearest.polys.tbl <- function(pts, polys, output.table, crime_type) {
        
        if(nrow(pts) == 0) {
                
                return(output.table)
        } 
        
        else {

        Fdist <- list()
        for(i in 1:dim(pts)[1]) {
                pDist <- vector()
                for(j in 1:dim(polys)[1]) { 
                        pDist <- append(pDist, gDistance(pts[i,],polys[j,])) 
                }
                Fdist[[i]] <- pDist
        } 
        
        # RETURN POLYGON (NUMBER) WITH THE SMALLEST DISTANCE FOR EACH POINT  
        min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1]))  
        
        # RETURN DISTANCE TO NEAREST POLYGON
        PolyDist <- unlist(lapply(Fdist, FUN=function(x) min(x)[1]))  
        
        # CREATE POLYGON-ID AND MINIMUM DISTANCE COLUMNS IN POINT FEATURE CLASS
        pts@data <- data.frame(pts@data, PolyID=min.dist, PDist=PolyDist)
        
        # get the name IDs of the closest polygons
        name_ids <- as.vector(pts$PolyID)
        
        # IMPORTANT needs the right column name
        polygon_names <- as.data.frame(polys$id)
        
        # get NAME and append to shapefile. Do a merge. Sum two columns and delete old column
        name_ids <- as.vector(pts$PolyID)
        
        # IMPORTANT needs the right column name (i.e. $id or $GEOID)
        name_df <- as.data.frame(polys$GEOID)
        
        # saved counts
        pts.table <- as.data.frame(table(pts$PolyID))
        
        # now change var to be the names
        # IMPORTANT needs the right column name
        pts.table$GEOID <- name_df[as.vector(pts.table$Var1),]
        pts.table$near_count <- pts.table$Freq
        
        # now extract just NAME and near_count
        pts.table <- pts.table[,3:4]
        
        # merge with existing counts
        combined_counts <- merge(output.table, pts.table, all.x=TRUE)
        # set NAs to 0
        combined_counts$near_count[is.na(combined_counts$near_count)] <- 0
        
        output <- list()
        # IMPORTANT needs the right column name
        output$GEOID <- output.table$GEOID
        
        # need this to be the right tag
        if(crime_type == "murder") {
                output$murd_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "rape") {
                output$rape_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "aggravated_assault") {
                output$agg_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "arson") {
                output$ars_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "robbery") {
                output$rob_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "burglary") {
                output$bur_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "motor_vehicle_theft") {
                output$mvt_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "larceny") {
                output$larc_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else if(crime_type == "theft_from_vehicle") {
                output$tfv_num <- rowSums(combined_counts[,2:3])
                return(output)
        }
        else {
                break
        }
        }
}

# assign points that don't fall inside polygons to the closest polygon
counts.murder.table <- pts.to.nearest.polys.tbl(counts.murder.na,
                                                blockgroups_shp,
                                                counts.murder.table, 
                                                "murder")

counts.rape.table <- pts.to.nearest.polys.tbl(counts.rape.na,
                                              blockgroups_shp,
                                              counts.rape.table, 
                                              "rape")

counts.aggravated_assault.table <- pts.to.nearest.polys.tbl(counts.aggravated_assault.na,
                                                            blockgroups_shp,
                                                            counts.aggravated_assault.table, 
                                                            "aggravated_assault")

counts.arson.table <- pts.to.nearest.polys.tbl(counts.arson.na,
                                               blockgroups_shp,
                                               counts.arson.table,
                                               "arson")

counts.robbery.table <- pts.to.nearest.polys.tbl(counts.robbery.na,
                                                 blockgroups_shp,
                                                 counts.robbery.table, 
                                                 "robbery")

counts.burglary.table <- pts.to.nearest.polys.tbl(counts.burglary.na,
                                                  blockgroups_shp,
                                                  counts.burglary.table,
                                                  "burglary")

counts.motor_vehicle_theft.table <- pts.to.nearest.polys.tbl(counts.motor_vehicle_theft.na,
                                                             blockgroups_shp,
                                                             counts.motor_vehicle_theft.table, 
                                                             "motor_vehicle_theft")

counts.larceny.table <- pts.to.nearest.polys.tbl(counts.larceny.na, 
                                                 blockgroups_shp, 
                                                 counts.larceny.table, 
                                                 "larceny")

counts.theft_from_vehicle.table <- pts.to.nearest.polys.tbl(counts.theft_from_vehicle.na,
                                                            blockgroups_shp,
                                                            counts.theft_from_vehicle.table, 
                                                            "theft_from_vehicle")


##### END INSERTS POINTS TO NEAREST POLYGON

# add counts of each crime type as a new row in blockgroups_shp

blockgroups_shp <- merge(blockgroups_shp, counts.murder.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.rape.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.aggravated_assault.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.arson.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.robbery.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.burglary.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.motor_vehicle_theft.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.larceny.table, all.x=TRUE)
blockgroups_shp <- merge(blockgroups_shp, counts.theft_from_vehicle.table, all.x=TRUE)

# remove NAs (sets them to 0)
blockgroups_shp$murd_num[is.na(blockgroups_shp$murd_num)] <- 0
blockgroups_shp$rape_num[is.na(blockgroups_shp$rape_num)] <- 0
blockgroups_shp$agg_num[is.na(blockgroups_shp$agg_num)] <- 0
blockgroups_shp$ars_num[is.na(blockgroups_shp$ars_num)] <- 0
blockgroups_shp$rob_num[is.na(blockgroups_shp$rob_num)] <- 0
blockgroups_shp$bur_num[is.na(blockgroups_shp$bur_num)] <- 0
blockgroups_shp$mvt_num[is.na(blockgroups_shp$mvt_num)] <- 0
blockgroups_shp$larc_num[is.na(blockgroups_shp$larc_num)] <- 0
blockgroups_shp$tfv_num[is.na(blockgroups_shp$tfv_num)] <- 0

###############################
# tabulate cost per geography #
###############################

# add total cost of crime for each crime type
blockgroups_shp$murd_cost <- as.numeric(cost_of_crime[1,3]) * as.numeric(blockgroups_shp$murd_num)
blockgroups_shp$rape_cost <- as.numeric(cost_of_crime[2,3]) * as.numeric(blockgroups_shp$rape_num)
blockgroups_shp$agg_cost <- as.numeric(cost_of_crime[3,3]) * as.numeric(blockgroups_shp$agg_num)
blockgroups_shp$ars_cost <- as.numeric(cost_of_crime[3,3]) * as.numeric(blockgroups_shp$ars_num)
blockgroups_shp$rob_cost <- as.numeric(cost_of_crime[4,3]) * as.numeric(blockgroups_shp$rob_num)
blockgroups_shp$bur_cost <- as.numeric(cost_of_crime[5,3]) * as.numeric(blockgroups_shp$bur_num)
blockgroups_shp$mvt_cost <- as.numeric(cost_of_crime[6,3]) * as.numeric(blockgroups_shp$mvt_num)
blockgroups_shp$larc_cost <- as.numeric(cost_of_crime[7,3]) * as.numeric(blockgroups_shp$larc_num)
blockgroups_shp$tfv_cost <- as.numeric(cost_of_crime[8,3]) * as.numeric(blockgroups_shp$tfv_num)

# Total Crime Cost Calculation

# tablulate total cost of crime across all crime types for each row
crime_cost <- rowSums(as.data.frame(blockgroups_shp[,crime_cost_field_names]))
# add field with value for total cost across all crime types
blockgroups_shp$crime_cost <- crime_cost

# adds percentage breakdown
blockgroups_shp$murd_perc <- as.numeric(blockgroups_shp$murd_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$rape_perc <- as.numeric(blockgroups_shp$rape_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$agg_perc <- as.numeric(blockgroups_shp$agg_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$ars_perc <- as.numeric(blockgroups_shp$ars_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$rob_perc <- as.numeric(blockgroups_shp$rob_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$bur_perc <- as.numeric(blockgroups_shp$bur_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$mvt_perc <- as.numeric(blockgroups_shp$mvt_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$larc_perc <- as.numeric(blockgroups_shp$larc_cost) / as.numeric(blockgroups_shp$crime_cost)
blockgroups_shp$tfv_perc <- as.numeric(blockgroups_shp$tfv_cost) / as.numeric(blockgroups_shp$crime_cost)

# removes NaN and sets it equal to NA
blockgroups_shp$murd_perc[is.nan(blockgroups_shp$murd_perc)] <- NA
blockgroups_shp$rape_perc[is.nan(blockgroups_shp$rape_perc)] <- NA
blockgroups_shp$agg_perc[is.nan(blockgroups_shp$agg_perc)] <- NA
blockgroups_shp$ars_perc[is.nan(blockgroups_shp$ars_perc)] <- NA
blockgroups_shp$rob_perc[is.nan(blockgroups_shp$rob_perc)] <- NA
blockgroups_shp$bur_perc[is.nan(blockgroups_shp$bur_perc)] <- NA
blockgroups_shp$mvt_perc[is.nan(blockgroups_shp$mvt_perc)] <- NA
blockgroups_shp$larc_perc[is.nan(blockgroups_shp$larc_perc)] <- NA
blockgroups_shp$tfv_perc[is.nan(blockgroups_shp$tfv_perc)] <- NA

# adds primary crime type
# works for one row at a time, saves the row and a value for what the row is
crime_perc_max <- apply(as.data.frame(blockgroups_shp[,perc_field_names]), 1, which.max)

# append to blockgroups_shp
blockgroups_shp$prim_crime <- crime_perc_max

# coerce value for blockgroups with no crimes to NA
blockgroups_shp$prim_crime <- as.numeric(blockgroups_shp$prim_crime)
# set the NAs to 9
blockgroups_shp$prim_crime[is.na(blockgroups_shp$prim_crime)] <- 9
# assign labels to to numeric values for crime above
blockgroups_shp$prim_crime <- crime_labels[blockgroups_shp$prim_crime]

#
# clean up values (rounding)
#

# crime costs
blockgroups_shp$murd_cost <- round(blockgroups_shp$murd_cost, 2)
blockgroups_shp$rape_cost <- round(blockgroups_shp$rape_cost, 2)
blockgroups_shp$agg_cost <- round(blockgroups_shp$agg_cost, 2)
blockgroups_shp$ars_cost <- round(blockgroups_shp$ars_cost, 2)
blockgroups_shp$rob_cost <- round(blockgroups_shp$rob_cost, 2)
blockgroups_shp$bur_cost <- round(blockgroups_shp$bur_cost, 2)
blockgroups_shp$mvt_cost <- round(blockgroups_shp$mvt_cost, 2)
blockgroups_shp$larc_cost <- round(blockgroups_shp$larc_cost, 2)
blockgroups_shp$tfv_cost <- round(blockgroups_shp$tfv_cost, 2)
blockgroups_shp$crime_cost <- round(blockgroups_shp$crime_cost, 2)

# crime perc
blockgroups_shp$murd_perc <- round(blockgroups_shp$murd_perc, 4)
blockgroups_shp$rape_perc <- round(blockgroups_shp$rape_perc, 4)
blockgroups_shp$agg_perc <- round(blockgroups_shp$agg_perc, 4)
blockgroups_shp$ars_perc <- round(blockgroups_shp$ars_perc, 4)
blockgroups_shp$rob_perc <- round(blockgroups_shp$rob_perc, 4)
blockgroups_shp$bur_perc <- round(blockgroups_shp$bur_perc, 4)
blockgroups_shp$mvt_perc <- round(blockgroups_shp$mvt_perc, 4)
blockgroups_shp$larc_perc <- round(blockgroups_shp$larc_perc, 4)
blockgroups_shp$tfv_perc <- round(blockgroups_shp$tfv_perc, 4)


###############################
# save as separate shapefiles #
###############################

# write out as a new shapefile

# blockgroups
writeOGR(blockgroups_shp, output_directory, layer = blockgroups_output_layer, driver = "ESRI Shapefile", overwrite_layer = T)

