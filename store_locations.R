# The date is December 1, 2017
# The Network Planning team needs to determine new store locations for 2019
# US Census micro data for 2016 has just been released

# The team must balance current market saturation with projected growth
# Growth comes from 2014 to 2016 ACS data (from US Census Bureau)
# current market saturation will look at locations of existing Target stores (ignore competitors)

# See Locationsv2.R for store location data prep

############
# libraries
###########

library(tidyr)
library(ipumsr)
library(stringr)


############
# data 
###########

# 2014, 2015, 2016 ACS 1-yr samples 
# https://usa.ipums.org/usa/
acs <- read_ipums_micro("NetworkPlanningTest/Census/usa_00002.xml")

############
# transform 
###########

# create StateName and CountyFIPS5 variables
acs <- mutate(acs,
              StateName = as_factor(STATEFIP, levels = "labels"),
              StateFip = str_pad(as.character(as_factor(acs$STATEFIP, levels = "values")), 2, pad = "0"),
              CountyFip = str_pad(as.character(as_factor(acs$COUNTYFIP, levels = "values")), 3, pad = "0"),
              CountyFIPS5 = paste0(StateFip, CountyFip)
)

# 9436102 total obs
# 3731844 obs not identified in both 
sum(acs$CountyFip == "000")
sum(acs$COUNTYFIP == 0)
# 3731844/9436102 = 40 % ?!?!

# Per wikipedia
# As of 2020, there are currently 3,143 counties in the US
# 430/3143 = 13% 
# See resource below for a discussion of this problem 
# https://www.psc.isr.umich.edu/dis/census/Features/puma2cnty/

# remove county == 0 (not identified)
acs2 <- filter(acs, CountyFip != '000')

# roll up from individual to county level
acs_county <- acs2 %>%
  group_by(YEAR, StateName, CountyFIPS5) %>%
  summarise(PopCounts = sum(PERWT))

# create growth variable (2014 vs 2015, 2015 vs 2016, and total 2014 to 2016)
yrdf <- spread(acs_county, YEAR, PopCounts)

yrdf <- rename(yrdf, 
               c2014 = `2014`,
               c2015 = `2015`,
               c2016 = `2016`)

yrdf <- mutate(yrdf, 
               d14v15 = c2015 - c2014, 
               d15v16 = c2016 - c2015,
               yoy = d15v16 +d14v15
)

# Wow, less than 25% of counties grew by more than 10,000 people ?!
quantile(yrdf$yoy)

# Rename for clarity
acs16 <- yrdf

# Get list of stores to merge with ACS

# See ACS_Analysisv2.R for ACS data prep


##############
# libraries
##############


##############
# data
##############

stores <- read.csv("/NetworkPlanningTest/Census/stores.csv",
                   stringsAsFactors = F)


# https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html
codes <- read.csv("/NetworkPlanningTest/Census/geocodes2017.csv",
                  stringsAsFactors = F)

# https://gist.github.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53#file-us-state-ansi-fips-csv
fips <- read.csv("/NetworkPlanningTest/Census/statefips.csv", 
                 stringsAsFactors = F)


##############
# transform
##############


# Only 1 store closed between 2017 and 2019, so okay to keep only "Open"
open <- filter(stores, location_status == "Open")
# Remove stores that open in the "future"
open <- filter(open, as.Date(milestone_grand_open_date) < '2020-01-01')


# remove countyfips = 0
codes <- filter(codes, countyfips != 0)

# merge fips and codes and open 
# Case sensitive ("City" vs "city")
open <- left_join(open, fips, by = c("mail_region" = "abr"))
open$county <- tolower(open$mail_county)
codes$county <- tolower(codes$name)

open <- left_join(open, codes, by = c("fips" = "statefips", "county" = "county"))

# 12 stores show NA
sum(is.na(open$countyfips))
look <- filter(open, is.na(countyfips))

# "chesapeake" VA should be "chesapeake city" 550 
# "anchorage borough" AK should be "anchorage municipality" 20 
# location_id	mail_county	county fips	Notes
# 1076	Alexandria	510	Can't make this one out
# 769	Dona Ana	13	tilde missing over the n
# 929	La Salle	99	LaSalle
# 2527	PA - Pennsylvania	101	Incorrectly listed -- should be Montgomery County
# 3229		61	Incorrectly listed -- should be New York County
# 3269 miami dade 86 -- vs miami-dade
# 3339 prince george 33 -- vs prince george's 

open <- mutate(open, 
               countyfips2 = 
                 case_when(
                   county == "chesapeake" ~ 550,
                   county == "anchorage borough" ~ 20,
                   location_id == 769 ~ 13,
                   location_id == 929 ~ 99,
                   location_id == 2527 ~ 101,
                   location_id == 3229 ~ 61,
                   location_id == 3269 ~ 86,
                   location_id == 3339 ~ 33,
                   location_id == 1076 ~ 510
                 )
)

# number versus integer ??
open <- mutate(open, 
               countyfips_final = 
                 case_when(
                   is.na(countyfips) ~ countyfips2,
                   TRUE ~ as.numeric(countyfips)
                 ))

sum(is.na(open$countyfips_final))

# create 5-digit fips
open <- mutate(open,
StateFip = str_pad(as.character(fips), 2, pad = "0"),
CountyFip = str_pad(as.character(countyfips_final), 3, pad = "0"),
CountyFIPS5 = paste0(StateFip, CountyFip)
)

write.csv(open, "/NetworkPlanningTest/Census/storesFIPS.csv")

# roll up to county level for join to ACS 2016 data 
store_county <- open %>%
  filter(as.Date(milestone_grand_open_date) < '2019-01-01') %>%
  group_by(CountyFIPS5) %>%
  summarise(StoreCounts = n())

# 1837 looks good 
sum(store_county$StoreCounts)


# Store + ACS data prep and analysis

# See Locationsv2.R for store data prep
# See ACS_Analysisv2.R for ACS data prep

acs16 <- left_join(acs16, store_county, by = "CountyFIPS5")

# Create analysis variables

# HasStore Yes/No
# PopStoreRatio (how over or under indexed a county is on Target stores)
# Growth County Yes/No


acs16$StoreCounts[is.na(acs16$StoreCounts)] <- 0

acs16 <- mutate(acs16,
                HasStore = case_when(StoreCounts == 0 ~ 'No', TRUE ~ 'Yes'),
                GrowthCounty = case_when(yoy > 0 ~ 'Yes', TRUE ~ 'No'),
                PopStoreRatio = PopCounts/StoreCounts
                )

acs16$PopStoreRatio[is.infinite(acs16$PopStoreRatio)] <- 0

# can't remove a grouping variable YEAR ?
acs16 <- select(acs16,
                -YEAR,
                -PopCounts,
                -StateName.y
                )


# export to csv for Tableau
write.csv(acs16, "/NetworkPlanningTest/Census/acs_stores2016.csv")



