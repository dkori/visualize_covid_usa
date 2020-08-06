library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(tidycensus)
library(scales)
options(tigris_use_cache = TRUE)
#retrieve latest COVID data
US<-read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))%>%
  #filter out territories
  filter(!state%in%c("Puerto Rico","Virgin Islands","Guam","Northern Mariana Islands"))

acs18_vars<-load_variables(year=2017,dataset="acs5")
census_sf1_vars<-load_variables(year=2010,dataset="sf1")

rural<-census_sf1_vars%>%
  filter(grepl("rural",concept,ignore.case=TRUE))



poverty_var<-acs18_vars%>%
  filter(grepl("poverty",label,ignore.case=TRUE))



#create a function that returns percent rural for every county
retrieve_rural<-function(state){
  rural_split<-get_decennial(year=2010,
                             state=state,
                             variables=c("total"="P002001","rural"="P002005"),
                             geography="county")%>%
    #select("GEOID","variable","value")%>%
    #spread the data to make it wide
    spread(key="variable",value="value")
}

#read in 2016 election results
election16<-read_csv(url("https://raw.githubusercontent.com/john-guerra/US_Elections_Results/master/US%20presidential%20election%20results%20by%20county.csv"))%>%
  #make fips character
  mutate(GEOID= case_when(combined_fips<=9999~paste0("0",combined_fips),
                          TRUE~paste0(combined_fips)))%>%
  #since some states have multiple rows per county, aggregate up to County level
  group_by(GEOID,county_name,state_abbr)%>%
  summarise(votes_total = sum(votes_total),
            votes_dem = sum(votes_dem),
            votes_gop = sum(votes_gop))%>%
  ungroup()%>%
  #recalculate percentages
  mutate(per_dem = votes_dem/votes_total,
         per_gop = votes_gop/votes_total)

#retrieve nationwide census tract data for these columns
census_info<-do.call(rbind,
                     lapply(unique(US$province),census_function)
                     )
census_rural<-do.call(rbind,
                      lapply(unique(US$state),retrieve_rural))

#merge rural with election results
rural_election<-census_rural%>%
  inner_join(election16)%>%
  mutate(trump_margin=per_gop-per_dem)%>%
  mutate(pct_rural=rural/total)

rural_election%>%
  ggplot(aes(x=pct_rural,y=trump_margin,color=trump_margin,size=votes_total))+
  geom_point(alpha=.5)+
  scale_color_gradient(low="blue",high="red")+
  theme_minimal()+
  theme(legend.position="bottom")
test<-US%>%
  filter(state=="New York")
with_census<-US%>%
  inner_join(census_rural,by=c("fips" = "GEOID"))

#merge rural_election with covid data
with_election<-US%>%
  inner_join(rural_election,by=c("fips" = "GEOID"))

#graph covid against trump margin
unique(with_election$state_abbr)
length(unique(with_election$fips))

length(unique(with_election[with_election$state_abbr=="NY",]$fips))
length(unique(election16[election16$state_abbr=="NY",]$combined_fips))
length(unique(US[US$state=="New York",]$fips))
elections_fips_ny<-unique(with_election[with_election$state_abbr=="NY",]$fips)
test<-
with_election%>%
  #filter(state_abbr!="NY")%>%
  group_by(fips)%>%
  arrange(date,.by_group = TRUE)%>%
  mutate(new_cases = cases - lag(cases,default = 0))%>%
  ungroup()%>%
  #remove recovered cases
  mutate(new_cases = case_when(new_cases<0 ~ 0,
                               TRUE ~ new_cases))%>%
  mutate(trump_margin_binned = case_when(trump_margin<(-.2)~(-.2),
                                         trump_margin>(.2)~.2,
                                         TRUE~trump_margin))%>%
  group_by(date,trump_margin_binned)%>%
  summarise(new_cases=sum(new_cases))%>%
  ungroup()%>%
  filter(date>=as.Date("02-26-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=new_cases,fill=trump_margin_binned))+
  geom_bar(stat="identity",position="stack",width=1)+
  scale_fill_gradient2(low="blue",mid="grey",high="red",label=percent)+
  labs(x="date",y="new covid cases",fill="County Trump win margin",
       title = "Daily new covid cases by Trump win margin (excl. NYC and VT)")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_y_continuous(label=comma)

unique(with_election[with_election$state_abbr=="NY",]$county)
test<-election16%>%
  filter(state_abbr=="NY")
test2<-US%>%
  filter(state=="New York")%>%
  select(county,fips)%>%
  unique()
with_election%>%
  #filter(state_abbr!="NY")%>%
  group_by(fips)%>%
  arrange(date,.by_group = TRUE)%>%
  mutate(new_cases = cases - lag(cases,default = 0))%>%
  ungroup()%>%
  #remove recovered cases
  mutate(new_cases = case_when(new_cases<0 ~ 0,
                               TRUE ~ new_cases))%>%
  mutate(trump_margin_binned = case_when(trump_margin<(-.2)~(-.2),
                                         trump_margin>(.2)~.2,
                                         TRUE~trump_margin))%>%
  group_by(date,trump_margin_binned)%>%
  summarise(new_cases=sum(new_cases))%>%
  ungroup()%>%
  filter(date>=as.Date("02-26-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=new_cases,fill=trump_margin_binned))+
  geom_bar(stat="identity",position="stack",width=1)+
  scale_fill_gradient2(low="blue",mid="grey",high="red",label=percent)+
  labs(y="Net increase in COVID cases",fill="County Trump\nvictory margin",
       title = "Daily COVID case increases by 2016 Pres vote\n(excludes NYC)")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_y_continuous(label=comma)

for_rural<-with_census%>%
  filter(!is.na(rural))%>%
  group_by(fips)%>%
  arrange(date,.by_group = TRUE)%>%
  mutate(new_cases = cases - lag(cases,default = 0))%>%
  ungroup()%>%  
  #remove recovered cases
  mutate(new_cases = case_when(new_cases<0 ~ 0,
                             TRUE ~ new_cases))%>%
  #remove recovered cases
  #mutate(new_cases = case_when(new_cases<0 ~ 0,
  #                             TRUE ~ new_cases))%>%
  mutate(pct_rural=rural/total)
median(for_rural$pct_rural)
for_rural%>%
  group_by(date,pct_rural)%>%
  summarise(new_cases=sum(new_cases))%>%
  ungroup()%>%
  filter(date>=as.Date("02-26-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=new_cases,fill=pct_rural))+
  geom_bar(stat="identity",position="fill",width=1)+
  scale_fill_viridis_c(labels=percent)+
  labs(y="Daily confirmed COVID cases",
       x="date",
       fill="Rural percentage\nof county")+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 1))

options(scipen=999)
va<-retrieve_rural("VA")%>%
  mutate(pct_rural = rural/total)
with_census%>%
  filter(!is.na(rural))%>%
  group_by(fips)%>%
  arrange(date,.by_group = TRUE)%>%
  mutate(new_cases = cases - lag(cases,default = 0))%>%
  ungroup()%>%
  mutate(pct_rural=rural/total)%>%
  mutate(rural_quantile = case_when(pct_rural<.25 ~ "Less than 25% Rural",
                                    pct_rural<.95 ~ "10 to 95% Rural",
                                    pct_rural<1 ~ "95 to 100% Rural"))%>%
  group_by(date,rural_quantile)%>%
  summarise(cases=sum(cases))%>%
  ungroup()%>%
  filter(date>=as.Date("02-26-2020",format("%m-%d-%Y")))%>%
  ggplot(aes(x=date,y=cases,color=rural_quantile))+
  geom_line()+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y="Daily confirmed COVID cases",
       x="date",
       color="Rural share\nof county")+
  theme(legend.position="bottom")

with_census%>%
  filter(state=="New York")%>%
  ggplot(aes(x=date,y=cases))+
  geom_line()
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


