library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)
#retrieve latest COVID data
US<-read_csv(url("https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv"))%>%
  #remove missing values in longitude
  filter(!is.na(longitude))%>%
  #limit just to the US
  filter(country=="United States")%>%
  #convert to sf object
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326)%>%
  #make the date column date format
  mutate(date=as.Date(date_confirmation,format("%d.%b.%Y")))
acs18_vars<-load_variables(year=2017,dataset="acs5")

poverty_var<-acs18_vars%>%
  filter(grepl("poverty",label,ignore.case=TRUE))

#create a list of census columns to retrieve
to_retrieve_acs5<-c(
  # population in occupied housing units
  "pop_in_units" = "B25008_001",
  # population in owner-occupied units
  "pop_in_owner_units" = "B25008_002",
  # population in renter-occupied units
  "pop_in_renter_units" = "B25008_003",
  # total population
  "total_pop" = "B01001_001",
  # non-hispanic white population
  "white_pop" = "B01001H_001",
  # block population
  "black_pop" = "B01001B_001",
  # american indian / alaska native population
  "native_pop" = "B01001C_001",
  # asian population
  "asian_pop" = "B01001D_001",
  # native hawaiian and other pacific islander pop
  "islander_pop" = "B01001E_001",
  # other race population
  "other_pop" = "B01001F_001",
  # two or more races
  "multi_race_pop" = "B01001G_001",
  # hispanic population
  "hispanic_pop" = "B01001I_001",
  # median household income (in given year's dollars)
  "median_income" = "B19013_001",
  # median age
  "median_age" = "B01002_001",
  # male population
  "male_pop" = "B01001_002",
  # female population
  "female_pop" = "B01001_026",
  # median home value
  "median_home_value" = "B25077_001",
  #pop in poverty
  "poverty_pop" = "B06012_002"
)

#create a function that returns a dataframe of census tract level info for a state
census_function<-function(state){
  get_acs(year=2018,variables=to_retrieve_acs5,geography="tract",state=state,geometry=TRUE)%>%
    select("NAME","variable","estimate")%>%
    #spread the data to make it wide
    spread(key="variable",value="estimate")
  
}


test<-census_function("Washington")
#retrieve nationwide census tract data for these columns
census_info<-lapply(unique(US$province),census_function)%>%
  bind_rows()

save(census_info,file="census_info.Rdata")

  

