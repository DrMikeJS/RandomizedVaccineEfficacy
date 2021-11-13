library(tidyverse)
library(ggrepel)

#load data
county_vax_rates <- read.csv("J:/OneDrive/Covid/COVID-19_Vaccinations_in_the_United_States_County.csv",sep=";")
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh

county_pop_2019 <- read.csv("J:/OneDrive/Covid/ERS_USDA_People.csv")
#https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/download-the-data/
county_pop_2019 <- dplyr::select(county_pop_2019,ï..FIPS,TotalPopEst2019)

county_covid_rates <- read.csv("J:/OneDrive/Covid/NYT_us-counties.csv")
#https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv

#get start and end points of running totals for period of interest
covid_Oct <- dplyr::select(filter(county_covid_rates, date == "2021-10-31"),c(fips,cases,deaths))
covid_Aug <- dplyr::select(filter(county_covid_rates, date == "2021-07-30"),c(fips,cases,deaths))

#remove data where county is unknown
covid_Aug <- covid_Aug[!is.na(covid_Aug$fips) ,]
covid_Oct <- covid_Oct[!is.na(covid_Oct$fips) ,]

#join start and end points
covid_Aug_Oct <- merge(covid_Aug,covid_Oct, by = "fips")

#calculate percentage change in covid rates over period of interest
covid_Aug_Oct$cases <-  covid_Aug_Oct$cases.y - covid_Aug_Oct$cases.x 
covid_Aug_Oct$deaths <- covid_Aug_Oct$deaths.y - covid_Aug_Oct$deaths.x 

#remove obsolete columns
covid_Aug_Oct <- dplyr::select(covid_Aug_Oct,fips,cases,deaths)

#remove bad (or revised) data, i.e. negative cases or negative deaths
covid_Aug_Oct <- covid_Aug_Oct[covid_Aug_Oct$cases>=0 ,]
covid_Aug_Oct <- covid_Aug_Oct[covid_Aug_Oct$deaths>=0 ,]

#prepare for join to other data
covid_Aug_Oct <- covid_Aug_Oct %>% rename(FIPS = fips)

#calculate percent vaccinated in each county (for simplicity, use mid-point of period of interest)
pct_vaccinated_mid_period <- dplyr::select(filter(county_vax_rates, ï..Date == "09/15/2021"),c(FIPS,Administered_Dose1_Pop_Pct,Series_Complete_Pop_Pct))

#join rates and vaccination data
covid_by_vax_rates <- merge(pct_vaccinated_mid_period, covid_Aug_Oct, by = "FIPS")

#prepare population data for join
county_pop_2019 <- county_pop_2019 %>% rename(FIPS = ï..FIPS)

#join population data to other data
covid_by_vax_rates <- merge(county_pop_2019, covid_by_vax_rates, by = "FIPS")

#calculate covid rate per 1000 population (not necessary if already provided in some datasets)
covid_by_vax_rates$cases_per_1000 = covid_by_vax_rates$cases/covid_by_vax_rates$TotalPopEst2019*1000 #no change in population, so per capita calculation unnecessary
covid_by_vax_rates$deaths_per_1000 = covid_by_vax_rates$deaths/covid_by_vax_rates$TotalPopEst2019*1000

#calculate background rates from counties with zero percent vaccination
Background_Covid_1st_Dose=dplyr::select(filter(covid_by_vax_rates,Administered_Dose1_Pop_Pct ==0),c(cases_per_1000,deaths_per_1000))

Background_Covid_2nd_Dose=dplyr::select(filter(covid_by_vax_rates,Series_Complete_Pop_Pct ==0),c(cases_per_1000,deaths_per_1000))

#remove control group from sample
Covid_by_1st_Dose_by_non_zero_vax_rates = dplyr::select(filter(covid_by_vax_rates,Administered_Dose1_Pop_Pct>0),c(Administered_Dose1_Pop_Pct,cases_per_1000,deaths_per_1000))

Covid_by_2nd_Dose_by_non_zero_vax_rates = dplyr::select(filter(covid_by_vax_rates,Series_Complete_Pop_Pct>0),c(Series_Complete_Pop_Pct,cases_per_1000,deaths_per_1000))

#function to get effect sizes
tabulateEffectSizes <- function(control,UseDeaths,vaccinated_counties){
  
  #any level of vaccination
  
  if(UseDeaths){
    combined_treatment_group=vaccinated_counties$deaths_per_1000 
  } else {
    combined_treatment_group=vaccinated_counties$cases_per_1000 
  }
  
  print("Control group")
  print(summary(control))
  
  n1 = length(control)
  m1 = mean(control)
  s1 = sd(control)
  
  n2 = length(combined_treatment_group)
  m2 = mean(combined_treatment_group)
  s2 = sd(combined_treatment_group)
  
  ci = 1.96*sqrt(s1^2/n1+s2^2/n2)
  
  effectiveness = head(vaccinated_counties,num_bins)#use existing as template
  
  colnames(effectiveness) <- c("effectiveness","lower_95_CI","upper_95_CI","percent_vaccinated")
  
  effectiveness$percent_vaccinated <- factor(effectiveness$percent_vaccinated,levels= c("Any>0",levels(factor(vaccinated_counties$percent_vaccinated))))
  
  effectiveness[1,"percent_vaccinated"] = "Any>0"
  
  effectiveness[1,"effectiveness"] = round((m2-m1)/m1*100)#round for display purposes
    effectiveness[1,"lower_95_CI"] = ((m2-m1)-ci)/m1*100 #divide by m1 and x 100 to give percentage value
  effectiveness[1,"upper_95_CI"] = ((m2-m1)+ci)/m1*100
  
  vaccinated_counties_grouped <- split(vaccinated_counties, f = vaccinated_counties$percent_vaccinated) 
  
  #specific levels of vaccination
  for(i in 1:num_bins){
    treatment = as.data.frame(vaccinated_counties_grouped[i])
    colnames(treatment)<-c("pct_vax","cases_per_1000","deaths_per_1000","percent_vaccinated")
    
    if(UseDeaths){
      treatment_group=treatment$deaths_per_1000 
    } else {
      treatment_group=treatment$cases_per_1000 
    }
    
    print("Treatment group:")
    print(summary(treatment_group))

    n2 = length(treatment_group)
    m2 = mean(treatment_group)
    s2 = sd(treatment_group)
    
    ci = 1.96*sqrt(s1^2/n1+s2^2/n2)
    
    effectiveness[i+1,"percent_vaccinated"] = first(treatment$percent_vaccinated)
    effectiveness[i+1,"effectiveness"] = round((m2-m1)/m1*100)
    effectiveness[i+1,"lower_95_CI"] = ((m2-m1)-ci)/m1*100 
    effectiveness[i+1,"upper_95_CI"] = ((m2-m1)+ci)/m1*100
  }
  print(effectiveness)
  return(effectiveness)
}

#function to graph results
linePlotWithErrorBars <- function(effectiveness){  
  ggplot(subset(effectiveness,percent_vaccinated!="Any>0"),aes(x=percent_vaccinated, y=subset(effectiveness,percent_vaccinated!="Any>0"), label=subset(effectiveness,percent_vaccinated!="Any>0"), group = 1)) +
    geom_line(linetype="dashed", color="blue", size=0.8)+
    geom_hline(linetype="dashed", color="lightblue", yintercept = 0)+#subset(effectiveness$effectiveness,effectiveness$percent_vaccinated=="Any>0"))+
    geom_errorbar(aes(ymin=lower_95_CI, ymax=upper_95_CI), width=.1) +
    geom_point(color=as.integer(factor(subset(effectiveness$upper_95_CI,effectiveness$percent_vaccinated!="Any>0")>0)), size=3)+
    labs(title = heading,
         subtitle ="Change in average incidence per capita from zero-vaccinated counties by percent vaccinated", #subtitle = paste("Percent change of avg. incidence per capita over all levels of vaccination greater than zero: ",subset(effectiveness$effectiveness,effectiveness$percent_vaccinated=="Any>0"),"%"),
         caption = "Data sources: CDC,NYT,ERS",
         y="Percent change in average incidence per capita",
         x="Counties grouped by percent vaccinated in mid-September")+
    geom_label_repel()+
    theme(plot.title = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "white", face = "bold"),
          plot.caption = element_text(color = "white", face = "italic"),
          plot.background = element_rect(fill = "lightblue3"),
          panel.background = element_rect(fill = "lightblue2"),
          panel.grid = element_line(color = "grey", size=0.5))
}

#effect size for cases by first dose
num_bins = 3 #large enough to get a trend, small enough so that confidence intervals are small

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_1st_Dose_percent <- Covid_by_1st_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated = cut_interval(Administered_Dose1_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_1st_Dose_percent)<-c("pct_vaxd","cases_per_1000","deaths_per_1000","percent_vaccinated")

heading  = "Covid cases by US County & percent 1st dose vaccinated (95% confidence): Aug-Oct 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_1st_Dose$cases_per_1000,FALSE,Covid_binned_by_1st_Dose_percent)

linePlotWithErrorBars(effectiveness)


#effect size for deaths by first dose
num_bins = 3 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_1st_Dose_percent <- Covid_by_1st_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated = cut_interval(Administered_Dose1_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_1st_Dose_percent)<-c("pct_vaxd","cases_per_1000","deaths_per_1000","percent_vaccinated")

heading  = "Covid deaths by US County & percent 1st dose vaccinated (95% confidence): Aug-Oct 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_1st_Dose$deaths_per_1000,TRUE,Covid_binned_by_1st_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for cases by second dose
num_bins = 3  #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_2nd_Dose_percent <- Covid_by_2nd_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated =  cut_interval(Series_Complete_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_2nd_Dose_percent)<-c("pct_vaxd","cases_per_1000","deaths_per_1000","percent_vaccinated")

heading  = "Covid cases by US County & percent 2nd dose vaccinated (95% confidence): Aug-Oct 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_2nd_Dose$cases_per_1000,FALSE,Covid_binned_by_2nd_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for deaths by second dose
num_bins = 3 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_2nd_Dose_percent <- Covid_by_2nd_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated =  cut_interval(Series_Complete_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_2nd_Dose_percent)<-c("pct_vaxd","cases_per_1000","deaths_per_1000","percent_vaccinated")

heading  = "Covid deaths by US County & percent 2nd dose vaccinated (95% confidence): Aug-Oct 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_2nd_Dose$deaths_per_1000,TRUE,Covid_binned_by_2nd_Dose_percent)

linePlotWithErrorBars(effectiveness)




