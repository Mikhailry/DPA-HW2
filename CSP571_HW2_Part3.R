# CSP571_HW2_Part3
# Scenario: 
# You have been tasked with analyzing ACME's Temporary Housing spend for senior management. 
# There is a hypothesis that ACME can reduce costs and increase service levels by issuing an RFP.
# ACME currently has three vendors: ‘Sherlock Homes Llc,’ ‘Keepin It Realty Inc,’ and ‘Raynor Shine Llc.’ 
# Prior to issuing an RFP, ACME requested detailed utilization data from all three vendors in Microsoft Excel 
# with the following fields:
# --> Vendor
# --> Type (Condo, Hotel, etc)
# --> # Days
#	--> Current Adjuster
# --> Move-in/Check-In Date
# --> Daily Housing Rate
# --> Claim Number
# --> Move-out/Check-Out Date
# --> Daily Admin Fee
# --> Policyholder Last Name
# --> Occupancy Status
# --> Total Housing Spend
# --> Policyholder City
# --> # ofBedrooms	

# Read in the data from the ACME Corp Spreadsheet

install.packages("readxl")
library(readxl)
acme <- read_excel("ACME_Corp.xlsx", sheet = 1, col_names = T)
summary(acme)

# 15. The three vendors each use a different definition of housing type. However, ACME's official types
# are listed on Sheet2 of the Excel sheet. Create a new column called 'Normalized Housing Type' based on the standardized mapping.
#read lookup data of "ACME_Corp.xlsx" and write it to acme_lookup
acme_lookup <- read_excel("ACME_Corp.xlsx", sheet=2, col_names = T)

#converting lookup table and acme Housing type column to lower case
acme_lookup$`Lookup Value`<-tolower(acme_lookup$`Lookup Value`)
acme_lookup$`Clean Value`<-tolower(acme_lookup$`Clean Value`)
acme$`Housing Type (Condo, Hotel, Apartment, Single Family Home)`<-tolower(acme$`Housing Type (Condo, Hotel, Apartment, Single Family Home)`)

lookup <- function(x){
  acme_lookup$`Clean Value`[match(x, acme_lookup$`Lookup Value`)]
}

#fill in the new column 'Normalized Housing Type' with lookup values
acme$'Normalized Housing Type' <- sapply(acme$`Housing Type (Condo, Hotel, Apartment, Single Family Home)`, lookup)


# 16. Compute the total housing spend by state and the total percentage of spend by Policy holder State
# listed in descending order by cost. Return this as a dataframe.

#calculating total housing spend by state 
totalHousSpend<-tapply(acme$`Total Housing Spend`, acme$`Policy holder State`, sum)
#converting to dataframe
totalHousSpend<-as.data.frame(totalHousSpend)
sumSpend<-sum(totalHousSpend)

totalHousSpend$totalPercSpendState<-sapply(totalHousSpend$totalHousSpend, function(x){x/sumSpend*100})
totalHousSpend[order(totalHousSpend$totalPercSpendState, decreasing = T),]


# 17. Create a table that has Normalized Housing Type (NHT) on the y-axis and vendor on the x-axis. At the 
# intersection, compute the 'Total Housing Spend of spend for that given and vendor and NHT. Compute the 
# row-wise and column-wise margins too.

tbl<-tapply(acme$`Total Housing Spend`, list(acme$`Normalized Housing Type`,acme$Vendor), sum,  na.rm=TRUE)
#substitute 'na' values with '0'
tbl[is.na(tbl)] <- 0
#round values in the table for a better perception
tbl<-round(addmargins(tbl))
tbl


# 18. Obtain top 20 most frequent Policyholder City and Policy holder State combos

library("dplyr")

  tbl2<-acme %>% count(`Policy holder State`, `Policyholder City`, sort = TRUE)
  names(tbl2)<-c('Policy holder State', 'Policyholder City', 'Frequency' )
  topCities<-head(as.data.frame(tbl2), n=20)
  topCities

# 19. Write a function obtains the lat lon for a given city and state
# Note: You'll propsefully need to do some research on how to obtain this. That is
# part of the exercise. Later in the course, we will cover how to leverage
# geolocation data for analysis, which is very important in a variety of disciplines.

#function to obtain geo data for the address
geodata<-function(address){  
  
#use the gecode function to query google servers
geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
#now extract the bits that we need from the returned list
answer <- data.frame(lat=NA, long=NA, formatted_address=NA, status=NA)
answer$status <- geo_reply$status

#if we are over the query limit - want to pause for 30 sec
while(geo_reply$status == "OVER_QUERY_LIMIT"){
  print("OVER QUERY LIMIT - Pausing for 30 sec at:") 
  time <- Sys.time()
  print(as.character(time))
  Sys.sleep(30)
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  answer$status <- geo_reply$status
}

#return Na's if we didn't get a match:
if (geo_reply$status != "OK"){
  return(answer)
}   
#else, extract what we need from the Google server reply into a dataframe:
answer$lat <- geo_reply$results[[1]]$geometry$location$lat
answer$long <- geo_reply$results[[1]]$geometry$location$lng
answer$formatted_address <- geo_reply$results[[1]]$formatted_address

return(answer)
}

#https://www.r-bloggers.com/batch-geocoding-with-r-and-google-maps/
  
# 20. Using the function above, obtain the lat lons for each of the top 20 cities.
#load up the ggmap library

#install.packages('ggmap') #uncomment to install package
library(ggmap)
address <- paste0(topCities$`Policyholder City`, ", ", topCities$`Policy holder State`)
s<-sapply(address, geodata)

topCities$latitude<-s[1,]
topCities$longitude<-s[2,]
topCities
