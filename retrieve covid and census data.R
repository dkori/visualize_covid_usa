library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(tidycensus)
library(scales)
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
census_sf1_vars<-load_variables(year=2010,dataset="sf1")

rural<-census_sf1_vars%>%
  filter(grepl("rural",concept,ignore.case=TRUE))



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
#create a function that returns percent rural for every census tract
retrieve_rural<-function(state){
  rural_split<-get_decennial(year=2010,
                             state=state,
                             variables=c("total"="P002001","rural"="P002005"),
                             geography="tract",
                             geometry=TRUE)%>%
    select("NAME","variable","value")%>%
    #spread the data to make it wide
    spread(key="variable",value="value")
}


#retrieve nationwide census tract data for these columns
census_info<-do.call(rbind,
                     lapply(unique(US$province),census_function)
                     )
census_rural<-do.call(rbind,
                      lapply(unique(US$province),retrieve_rural))

#save(census_info,file="census_info.Rdata")
load("census_info.Rdata")
#merge census info with US 
with_census<-st_join(US,st_transform(census_info,crs=4326),st_within,left=FALSE)%>%
  st_join(st_transform(census_rural,crs=4326),st_within,left=FALSE)%>%
  #calculate the poverty rate
  mutate(poverty_rate=poverty_pop/total_pop)%>%
  #fix date
  mutate(date=as.Date(date_confirmation,format("%d.%m.%Y")))

# plot median income over time
with_census%>%
  #remove geometry
  as.data.frame%>%
  filter(!is.na(median_income))%>%
  #remove NY
  filter(province=="New York")%>%
  group_by(date,median_income)%>%
  summarise(cases=n())%>%
  ungroup()%>%
  filter(date>=as.Date("02-25-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=cases,fill=median_income))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_viridis_c(labels=dollar,option="inferno")+
  labs(y="daily confirmed COVID cases",
       x="date",
       #fill="Median income of\ncensus tract",
       title="New York confirmed cases by day")+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

test<-US%>%
  #make the date column date format
  #mutate(date=as.Date(date_confirmation,format("%d.%m.%Y")))%>%
  filter(province=="New York")%>%
  group_by(date_confirmation)%>%
  summarise(cases=n())
  
with_census%>%
  #remove geometry
  as.data.frame%>%
  filter(!is.na(rural))%>%
  mutate(pct_rural=rural/total)%>%
  group_by(date,pct_rural)%>%
  summarise(cases=n())%>%
  ungroup()%>%
  filter(date>=as.Date("02-26-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=cases,fill=pct_rural))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_viridis_c(labels=percent)+
  labs(y="percentage of daily confirmed COVID cases",
       x="date",
       fill="Rural percentage\nof census tract")+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

with_census%>%
  #remove geometry
  as.data.frame%>%
  filter(!is.na(white_pop))%>%
  filter(!is.na(age))%>%
  mutate(age=gsub(" ","",age),
         age=gsub("-.*","",age),
         age=as.numeric(age))%>%
  filter(date>=as.Date("02-15-2020",format("%m-%d-%Y")))%>%
  mutate(pct_rural=rural/total)%>%  
  ggplot(aes(x=date,y=pct_rural,color=age))+
  geom_point(alpha=.5)+
  geom_jitter(position=position_jitter(width=NULL,height=.1))+
  scale_color_viridis_c()+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

unique(with_census$age)
last_day<-max(with_census$date,na.rm=TRUE)
last_day_by_state<-with_census%>%
  filter(date==last_day)%>%
  group_by(province)%>%
  summarise(cases=n())%>%
  arrange(desc(cases))


