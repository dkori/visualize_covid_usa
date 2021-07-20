library(tidycensus)
library(ggplot2)
library(tidyr)
library(dplyr)
library(parallel)
library(jsonlite)

# get pop for each county
vars<-load_variables('2018','acs5')
vars$layers<-sapply(vars$label,
                    function(x)length(strsplit(x,'!!')[[1]]))
county_pop<-get_acs(geography = 'county',variables = c('population' = 'B01001_001'),
                    year = 2018, wide = TRUE)%>%
  select(GEOID, "population" = "estimate")
# read in vaccination data
json_raw = url('https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data')
vax_json<-jsonlite::fromJSON('https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data')['vaccination_county_condensed_data']
vax_df = do.call(rbind,vax_json)
vars = load_variables('2018','acs5/subject')
# insured<-pct_vars%>%
#   filter(grepl('insured',label,ignore.case=TRUE))
real_estate_tax<-vars%>%
  filter(grepl("Real Estate",label,ignore.case=TRUE))%>%
  arrange(layers)
# pull in raw real estate tax
real_estate_tax2<-vars%>%
  filter(grepl("B25103",name))
# dec_vars<-load_variables(year=2010,dataset="sf1")
# suburb_vars<-dec_vars%>%
#   filter(grepl("suburb",label,ignore.case=TRUE))
# read in census rural pct
rural<-get_decennial(year=2010,
                     variables=c("total"="P002001","rural"="P002005"),
                     geography="county")%>%
  #select("GEOID","variable","value")%>%
  #spread the data to make it wide
  spread(key="variable",value="value")%>%
  mutate(pct_rural = rural/total)%>%
  select(GEOID,pct_rural)

# limit to just vars containing a percentage
pct_vars = vars%>%
  filter(grepl("Percent",label,ignore.case=TRUE))%>%
  # also filter out Puerto Rico Vars
  filter(!grepl("Puerto Rico", concept, ignore.case=TRUE))%>%
  # remove variables that end with "total population
  filter(!grepl("Total population$",label))%>%
  # remove the "Estimate!!Total!! from each label
  mutate(label = gsub('Estimate!!','',label))%>%
  # there are some labels repeated, remove them so they'll be unique
  group_by(label)%>%
  slice(1)%>%
  ungroup()

test<- pct_vars%>%filter(grepl('^Percent Insured',label))
var_ids = pct_vars$name

names(var_ids) <-pct_vars$label
# pull all variables at the county level, first by creating a function
get_vars<-function(state){
  return(get_acs(geography = 'county',
                 variables = var_ids[1:10],
                 year = 2018#,
                 #state = state
                 ))
}
# split variables into bunches so we can pick up where things left off
result_list = list()
var_ids_split = split(var_ids, ceiling(seq_along(var_ids)/50))

# for(i in 1:length(var_ids_split)){
#   print('split:')
#   print(i)
#   result_list[[i]]<-get_acs(geography = 'county',
#                             variables = var_ids_split[[i]],
#                             year = 2018)
#   
# }
#save(result_list, file = "county_census_data_list.Rdata")
download.file("https://dataverse.harvard.edu/api/access/datafile/3641280?format=tab&gbrecs=true",'election.csv')
election16<-read_tsv('election.csv')
ak_test<-election16_reshaped%>%
  filter(state_po=='AK')

election16_reshaped<-election16%>%
  filter(year==2016)%>%
  #make fips character
  mutate(GEOID= case_when(FIPS<9999~paste0("0",FIPS),
                          TRUE~paste0(FIPS)))%>%
  select(GEOID,county,state_po,party,candidatevotes)%>%
  spread(key=party,value=candidatevotes)%>%
  mutate(totalvotes = democrat+republican)%>%
  #since some states have multiple rows per county, aggregate up to County level
  group_by(GEOID,county,state_po)%>%
  summarise(votes_total = sum(totalvotes),
            votes_dem = sum(democrat),
            votes_gop = sum(republican))%>%
  ungroup()%>%
  #recalculate percentages
  mutate(per_dem = votes_dem/votes_total,
         per_gop = votes_gop/votes_total,
         trump_margin16 = (per_gop - per_dem)*100)%>%
  select(GEOID,trump16 = per_gop)
# download 2020 election results
election2020<-read_csv(url('https://raw.githubusercontent.com/dkori/Scraped2020_US_election_results/master/pres_county_2020.csv'))
# reshape
election20_reshape<-election2020%>%
  select(fips,last,percent)%>%
  spread(key=last,value=percent)%>%
  select(fips,trump20 = Trump)

load(file = "county_census_data_list.Rdata")
# bind result list into dataframe
census_df = do.call(rbind,result_list)
#eliminate variables that aren't actually percentages
more_than_100<-census_df%>%dplyr::filter(estimate>101)%>%
  select(variable)%>%
  unique()
na_test<-census_df%>%
  filter(is.na(estimate))%>%
  group_by(variable)%>%
  summarise(count = n())%>%
  filter(count>1000)
# get population
# make census df wide
census_wide <- census_df%>%
  # drop pct 
  filter(!variable%in%more_than_100$variable)%>%
  # drop variables where missingness is >1000
  filter(!variable%in%na_test$variable)%>%
  # replace nas in estimate
  replace_na(list('estimate'=0))%>%
  #drop moe
  select(-moe)%>%
  # spread it out
  spread(key = variable, value = estimate)
rm(census_df)
# merge in vax
vax_census <- vax_df%>%
  select(FIPS, Series_Complete_18PlusPop_Pct)%>%
  inner_join(census_wide, by = c("FIPS"="GEOID"))%>%
  filter(!is.na(Series_Complete_18PlusPop_Pct))%>%
  inner_join(county_pop, by = c("FIPS"="GEOID"))%>%
  inner_join(rural,by = c("FIPS" = "GEOID"))%>%
  mutate(pct_rural = pct_rural*100)%>%
  # join in trump pct
  inner_join(election16_reshaped,by = c("FIPS" = "GEOID"))%>%
  inner_join(election20_reshape,by= c("FIPS" = "fips"))%>%
  # calculate difference in trump margin cos who gaf about multicollinearity
  mutate(trump16 = trump16*100,
         trump20 = trump20*100,
    trump_decline = (trump16 - trump20))

#rm(census_wide)
# separate out Y and X variables
y = vax_census$Series_Complete_18PlusPop_Pct
x = vax_census%>%
  select(-FIPS,-Series_Complete_18PlusPop_Pct,-NAME, -population)%>%
  as.matrix()
weights = vax_census$population
library(glmnet)
#train model with lasso
lasso_reg = glmnet(x = x,y = y,weights = weights,alpha=1)
# define an mse function taking yhat and y as arguments
rmse<-function(y_hat,y){ 
  sqrt(mean((y_hat-y)^2))}
#calculate the rmse for each lambda level
test_predict = predict(lasso_reg,x,s = 8)
# find the rmse for each lambda value
# generate vector of predictions
prediction_vec = lapply(lasso_reg$lambda,function(s) predict(lasso_reg,x,s=s))
# calculate the rmse of each prediction
rmse_vec = sapply(prediction_vec,function(yhat)rmse(yhat,y))

lambda_comparison = cbind(lasso_reg$lambda,lasso_reg$df,rmse_vec)
names(lambda_comparison)<-c("lambda","non_zero_vars",rmse)
myCoefs <- coef(lasso_reg, s=3.4397786937);
myCoefs[which(myCoefs != 0 ) ]
library(broom)
summary_table <-tidy(lasso_reg)
summary_table_ranked<-summary_table%>%
  arrange(desc(estimate))
library(ggplot2)
top_vars<-unique(summary_table[1:200,]$term)
top_vars
library(viridis)
library(scales)

vax_census%>%
  mutate(log_pop = log(population),
         trump20 = trump20/100)%>%
  select(
         'vax_pct' = Series_Complete_18PlusPop_Pct,
         'hours_worked' = `Percent!!Population 16 to 64 years!!USUAL HOURS WORKED!!Usually worked 35 or more hours per week!!48 to 49 weeks`,
         'single_men' = `Percent!!Households!!No children under 18 years!!Other family!!Male householder, no wife present`,
         'cheap_real_estate' = `Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!$1,500 or more`,
         'native_insured' = `Percent Uninsured!!Civilian noninstitutionalized population`,
         'pct_insured' = `Percent Uninsured!!Civilian noninstitutionalized population`,
         'kitchen_facilities' = `Percent owner-occupied housing units!!Occupied housing units!!COMPLETE FACILITIES!!With complete kitchen facilities`,
         'no_hs_dip' = `Percent!!Citizens 18 years and over!!EDUCATIONAL ATTAINMENT!!9th to 12th grade, no diploma`,
         'pct_broadband_access' = `Percent!!TYPE OF INTERNET SUBSCRIPTIONS!!With an Internet subscription`,
         pct_rural,
         'log_pop' = log_pop,
         population,trump_decline,
         trump20)%>%
  ggplot(aes(x = pct_insured, y = vax_pct))+
  geom_point(aes(color =trump20,
    size = population),alpha = .3)+
  geom_smooth(method = 'lm', mapping = aes(weight = population))+
  scale_color_viridis(label = percent)+
  #theme(legend.position = 'bottom')+
  labs(y = "County-level 18+ Full Vax Rate",
       #x = "Trump vote share decline: 2016-2020",
       #x = "Percent of 18+ Pop W/O HS Diploma",
       #x = 'pct_broadband_access',
       #x = "Percent of owner occupied housing units with kitchen facilities",
       #color = "Pct of Owner-Occupied Units w/o Mortage\n paying Real Estate Tax < $1500"
       x = "Percent of Pop Insured",
       color = "Trump 2020%"
  )+
  scale_size(guide = 'none')

# plot trump margins in 50% chart, show vax rate in color
vax_census%>%
  mutate(log_pop = log(population),
         vax_pct = Series_Complete_18PlusPop_Pct/100)%>%
  select(
    'vax_pct',
    pct_rural,
    'log_pop' = log_pop,
    population,trump16, trump20, trump_decline)%>%
  ggplot(aes(x=trump20,y=trump16))+
  geom_point(aes(color=vax_pct,size=population),alpha=.3)+
  scale_color_viridis(label=percent,option="inferno")+
  geom_abline(intercept=0,slope=1)
# fit a new glm with just a subset of x of things that qualitatively look important
x_subset<-x[,c(#"pct_rural",
     "Percent!!Citizens 18 years and over!!EDUCATIONAL ATTAINMENT!!9th to 12th grade, no diploma",
     "Percent Uninsured!!Civilian noninstitutionalized population",
     "Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!$1,500 or more"#,
     #"Percent!!TYPE OF INTERNET SUBSCRIPTIONS!!With an Internet subscription"
     #"per_gop"
     #"trump_decline"
     #"trump20"
     )]
colnames(x_subset)<-c(#"pct_rural",
  "pct_no_hs","pct_uninsured",
                      "pct_real_estate_tax_plus_1500"#,
                      #"pct_internet_access"
  #"pct_trump16"
                      #"trump_decline"
  #"trump20"
                      )

# lm_subset<-lm(Series_Complete_18PlusPop_Pct~trump_decline,
#               data = vax_census,
#               weights = weights)
# summary(lm_subset)
#subset_reg<-glmnet(x = x_subset,y = y,weights = weights,alpha=1,lambda = 0)
subset_reg<- lm(y~x_subset
                ,weights=weights
                )
options(scipen=999)
summary(subset_reg)
lm_predicted<-predict.lm(subset_reg,x%>%as.data.frame(),weights = weights)
vax_census%>%
  mutate(log_pop = log(population),
         y_pred = lm_predicted,
         resid = y_pred - Series_Complete_18PlusPop_Pct,
         pct_rural = pct_rural/100,
         trump_decline = trump_decline/100)%>%
  select(resid,
         trump16,
         trump20,
         population,
         y_pred,
         trump_decline,
         pct_rural)%>%
  ggplot(aes(x = trump16, y = resid))+
  geom_point(aes(color =pct_rural,
                 size = population),alpha = .3)+
  scale_color_viridis()+
  #scale_color_gradient2(low="red",mid="grey",high="blue",midpoint = median(vax_census$trump_decline/100),label=percent)+
  labs(y = "Resid from regression",
       x = "Trump 2016",
       #x = 'pct_broadband_access',
       #x = "Percent of owner occupied housing units with kitchen facilities",
       #color = "Pct of Owner-Occupied Units w/o Mortage\n paying Real Estate Tax < $1500"
       #color = "Percentage of County Pop\nin Rural Area"
       color = "Pct Rural"
  )+
  geom_smooth(method = 'lm', mapping = aes(weight = population))

# dive into the upside-down L pattern: among low-rural counties, the ones with a high trump decline seem to be following an entirely different pattern, figure out how to separate them
low_rural_low_decline <-vax_census%>%
  filter(pct_rural<10 & trump_decline<2)%>%
  select(-FIPS,-Series_Complete_18PlusPop_Pct,-NAME, -population)%>%
  summarise_all("mean")%>%
  t()%>%
  as.data.frame()%>%
  add_rownames()%>%
  select(rowname, "low_decline" = "V1")
  

low_rural_high_decline<-vax_census%>%
  filter(pct_rural<10 & trump_decline>2)%>%
  select(-FIPS,-Series_Complete_18PlusPop_Pct,-NAME, -population)%>%
  summarise_all("mean")%>%
  t()%>%
  as.data.frame()%>%
  add_rownames()%>%
  select(rowname, "high_decline" = "V1")

compare_low_high<-low_rural_low_decline%>%
  left_join(low_rural_high_decline)%>%
  mutate(diff = low_decline-high_decline,
         abs_diff = abs(diff))%>%
  arrange(desc(abs_diff))


vax_census%>%
  select('vax_pct' = Series_Complete_18PlusPop_Pct,
         "pct_some_hs" = "Percent!!Citizens 18 years and over!!EDUCATIONAL ATTAINMENT!!9th to 12th grade, no diploma",
         "pct_uninsured" = "Percent Uninsured!!Civilian noninstitutionalized population",
         #"Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!$1,500 or more",
         #"per_gop"
         "trump_decline",
         "pct_rural"
         )%>%
  cor()%>%
  corrplot(method="number")

tidy(subset_reg)
summary(subset_reg)
min_lambda = min(subset_reg$lambda)
summary(subset_reg)
subset_predictions = predict(lasso_reg,x,s = 8)


# add the number of layers to each of the pct vars
pct_vars$layers<-lapply(pct_vars$label,
                        function(x)length(strsplit(x,'!!')[[1]]))

# find correlations between  percent insured and all other variables
insured_correlates<-cor(vax_census$`Percent Uninsured!!Civilian noninstitutionalized population`,
                        vax_census%>%select(-FIPS,-NAME,`Percent Uninsured!!Civilian noninstitutionalized population`))%>%
  t()%>%
  as.data.frame()%>%
  add_rownames("label")%>%
  left_join(pct_vars)%>%
  # create absolute value of correlation
  mutate(abs_cor = abs(V1))%>%
  dplyr::arrange(desc(abs_cor))%>%
  select(label,V1,layers)%>%
  dplyr::filter(!grepl('insured',label,ignore.case=TRUE))
insured_correlates$label<-row.names(insured_correlates)

cheap_correlates<-cor(vax_census$`Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!$1,500 or more`,
                        vax_census%>%select(-FIPS,-NAME,`Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!$1,500 or more`))%>%
  t()%>%
  as.data.frame()%>%
  add_rownames("label")%>%
  left_join(pct_vars)%>%
  # create absolute value of correlation
  mutate(abs_cor = abs(V1))%>%
  dplyr::arrange(desc(abs_cor))
cheap_correlates$layers<-sapply(cheap_correlates$label,
                                function(x)length(strsplit(x,'!!')[[1]]))
cheap_correlates<-cheap_correlates%>%
  select(label,V1,layers)%>%
  dplyr::filter(!grepl('insured',label,ignore.case=TRUE))
#coefficients: intercept included

# census_df<-get_acs(geography = 'county',
#               variables = var_ids,
#               year = 2018)
# save(census_df,file = "county_census_data.Rdata")
# # use a forloop to get verbose output from get_acs
# result_list = list()
# Sys.time()
# for(state in state.name){
#   print(state)
#   temp = 
#   result_list = append(result_list,get_vars(state))
#   Sys.time()
#   # save result list with each iteration in case it crashes part way through
#   save(result_list,file = "census_pulls_list.Rdata")
# }
# vax_census%>%
#   select('cheap_real_estate' = `Percent owner-occupied housing units without a mortgage!!Owner-occupied housing units without a mortgage!!REAL ESTATE TAXES!!Less than $800`,
#          'vax_pct' = Series_Complete_18PlusPop_Pct)%>%
#   ggplot()+
#   geom_point(aes(x =cheap_real_estate, y=vax_pct))

# county_test<-vax_census%>%
#   filter(grepl("(loudon)|(allegheny)|(butler)",NAME,ignore.case=TRUE))%>%
#   select(NAME,pct_rural)
# names(vax_census)[1:20]
