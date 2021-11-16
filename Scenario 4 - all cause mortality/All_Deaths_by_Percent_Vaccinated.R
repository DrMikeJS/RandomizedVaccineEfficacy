library(tidyverse)
library(ggrepel)

#load data
county_vax_rates <- read.csv("J:/OneDrive/Covid/COVID-19_Vaccinations_in_the_United_States_County.csv",sep=";")
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh

county_pop_2019 <- read.csv("J:/OneDrive/Covid/ERS_USDA_People.csv")
#https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/download-the-data/
county_pop_2019 <- select(county_pop_2019,ï..FIPS,TotalPopEst2019)

county_covid_rates_start_of_period <-read.csv ("J:/OneDrive/Covid/Sept_2021_Provisional_COVID-19_Death_Counts_in_the_United_States_by_County.csv")
#https://www.kaggle.com/winternguyen/conditions-contributing-to-deaths-covid19/version/24?select=Provisional_COVID-19_Death_Counts_in_the_United_States_by_County.csv
county_covid_rates_start_of_period <- select(county_covid_rates_start_of_period,c(FIPS.County.Code,Deaths.involving.COVID.19,Deaths.from.All.Causes))

county_covid_rates_end_of_period <-read.csv ("J:/OneDrive/Covid/Provisional_COVID-19_Death_Counts_in_the_United_States_by_County.csv")
#https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy/data
county_covid_rates_end_of_period <- select(county_covid_rates_end_of_period,c(FIPS.County.Code,Deaths.involving.COVID.19,Deaths.from.All.Causes))


#join start and end points
county_covid_rates <- merge(county_covid_rates_start_of_period,county_covid_rates_end_of_period, by = "FIPS.County.Code")

#get totals for period of interest
county_covid_rates$COVID.19.Deaths <-  county_covid_rates$Deaths.involving.COVID.19.y - county_covid_rates$Deaths.involving.COVID.19.x 
county_covid_rates$Total.Deaths <- county_covid_rates$Deaths.from.All.Causes.y - county_covid_rates$Deaths.from.All.Causes.x 
county_covid_rates$Non_Covid_Deaths <- county_covid_rates$Total.Deaths - county_covid_rates$COVID.19.Deaths 

#remove censored values (currently none as start period only reports deaths >10) and unneeded columns
covid_in_period_of_interest <- select(filter(county_covid_rates, !is.na(COVID.19.Deaths)),c(FIPS.County.Code,COVID.19.Deaths,Total.Deaths,Non_Covid_Deaths))

#estimate data where death numbers censored (replace NA with a number between 1 and 9 - say 5 for total, 0.5 for covid as 10% of total https://jamanetwork.com/journals/jama/fullarticle/2778234)
#not useful for current data
#covid_in_period_of_interest <- covid_in_period_of_interest %>% mutate(Total.Deaths = case_when(is.na(Total.Deaths) ~ 5, TRUE ~ as.numeric(Total.Deaths)))
#covid_in_period_of_interest <- covid_in_period_of_interest %>% mutate(COVID.19.Deaths = case_when(is.na(COVID.19.Deaths) ~ 0.5, TRUE ~ as.numeric(Total.Deaths)))

#prepare for join to other data
covid_in_period_of_interest <- covid_in_period_of_interest %>% rename(FIPS = FIPS.County.Code)

#calculate percent vaccinated in each county (for simplicity, use mid-point of period of interest)
pct_vaccinated_mid_period <- select(filter(county_vax_rates, ï..Date == "10/03/2021"),c(FIPS,Administered_Dose1_Pop_Pct,Series_Complete_Pop_Pct))

#join rates and vaccination data
covid_by_vax_rates <- merge(pct_vaccinated_mid_period, covid_in_period_of_interest, by = "FIPS")

#prepare population data for join
county_pop_2019 <- county_pop_2019 %>% rename(FIPS = ï..FIPS)

#join population data to other data
covid_by_vax_rates <- merge(county_pop_2019, covid_by_vax_rates, by = "FIPS")

#calculate covid rate per 1000 population (not necessary if already provided in some datasets)
covid_by_vax_rates$deaths_per_1000 = covid_by_vax_rates$Total.Deaths/covid_by_vax_rates$TotalPopEst2019*1000 
covid_by_vax_rates$covid_deaths_per_1000 = covid_by_vax_rates$COVID.19.Deaths/covid_by_vax_rates$TotalPopEst2019*1000 
covid_by_vax_rates$non_covid_deaths_per_1000 = covid_by_vax_rates$Non_Covid_Deaths/covid_by_vax_rates$TotalPopEst2019*1000 

#calculate background rates from counties with zero percent vaccination
Background_Covid_1st_Dose=select(filter(covid_by_vax_rates,Administered_Dose1_Pop_Pct ==0),c(covid_deaths_per_1000,deaths_per_1000,non_covid_deaths_per_1000))

Background_Covid_2nd_Dose=select(filter(covid_by_vax_rates,Series_Complete_Pop_Pct ==0),c(covid_deaths_per_1000,deaths_per_1000,non_covid_deaths_per_1000))

#remove control group from sample
Covid_by_1st_Dose_by_non_zero_vax_rates = select(filter(covid_by_vax_rates,Administered_Dose1_Pop_Pct>0),c(Administered_Dose1_Pop_Pct,covid_deaths_per_1000,deaths_per_1000,non_covid_deaths_per_1000))

Covid_by_2nd_Dose_by_non_zero_vax_rates = select(filter(covid_by_vax_rates,Series_Complete_Pop_Pct>0),c(Series_Complete_Pop_Pct,covid_deaths_per_1000,deaths_per_1000,non_covid_deaths_per_1000))

#function to get effect sizes
tabulateEffectSizes <- function(control,UseTotalDeaths,UseNonCovidDeaths,vaccinated_counties){
  
  print("Control group")
  print(summary(control))
  
  n1 = length(control)
  m1 = mean(control)
  s1 = sd(control)
  print(n1)
  print(m1)
  print(s1)
  
  effectiveness = head(select(vaccinated_counties,c("covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")),num_bins)#use existing as template
  
  colnames(effectiveness) <- c("effectiveness","lower_95_CI","upper_95_CI","percent_vaccinated")
  
  vaccinated_counties_grouped <- split(vaccinated_counties, f = vaccinated_counties$percent_vaccinated) 
  
  #run for specific levels of vaccination
  for(i in 1:num_bins){
    treatment = as.data.frame(vaccinated_counties_grouped[i])
    colnames(treatment)<-c("pct_vax","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")
  
    if(UseTotalDeaths){
      treatment_group=treatment$deaths_per_1000 
    } else {
      if(UseNonCovidDeaths){
        treatment_group=treatment$non_covid_deaths_per_1000 
      } else {
      treatment_group=treatment$covid_deaths_per_1000 
      }
    }
    
    print("Treatment group:")
    print(summary(treatment_group))

    n2 = length(treatment_group)
    m2 = mean(treatment_group)
    s2 = sd(treatment_group)
    print(n2)
    print(m2)
    print(s2)
    
    ci = 1.96*sqrt(s1^2/n1+s2^2/n2)

    effectiveness[i,"percent_vaccinated"] = first(treatment$percent_vaccinated)
    effectiveness[i,"effectiveness"] = round((m2-m1)/m1*100)
    effectiveness[i,"lower_95_CI"] = ((m2-m1)-ci)/m1*100 
    effectiveness[i,"upper_95_CI"] = ((m2-m1)+ci)/m1*100
  }
  print(effectiveness)

  return(effectiveness)
}

#function to graph results
linePlotWithErrorBars <- function(effectiveness){  
  ggplot(effectiveness,aes(x=percent_vaccinated, y=effectiveness, label=effectiveness, group = 1)) +
    geom_line(linetype="dashed", color="blue", size=0.8)+
    geom_hline(linetype="dashed", color="lightblue", yintercept = 0) +
    geom_errorbar(aes(ymin=lower_95_CI, ymax=upper_95_CI), width=.1) +
    geom_point(aes(x = percent_vaccinated, y = effectiveness, colour =upper_95_CI<0),size=3) +
    scale_colour_manual(name="Significant Reduction?",values = setNames(c('green','red'),c(T, F))) +
    labs(title = heading,
         subtitle ="Change in average incidence per capita from zero-vaccinated counties by % vaccinated (95% conf.int.)",
         caption = "Data sources: CDC,ERS",
         y="Percent change in average incidence per capita",
         x="Percent vaccinated in US Counties by early October")+
    geom_label_repel()+
    theme(plot.title = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "white", face = "bold"),
          plot.caption = element_text(color = "white", face = "italic"),
          plot.background = element_rect(fill = "lightblue3"),
          panel.background = element_rect(fill = "lightblue2"),
          panel.grid = element_line(color = "grey", size=0.5))
}

#effect size for covid deaths by first dose
num_bins = 5 #large enough to get a trend, small enough so that confidence intervals are reasonably small

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_1st_Dose_percent <- Covid_by_1st_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated = cut_number(Administered_Dose1_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_1st_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Covid deaths by US County & percent 1st dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_1st_Dose$covid_deaths_per_1000,FALSE,FALSE,Covid_binned_by_1st_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for total deaths by first dose
num_bins = 5 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_1st_Dose_percent <- Covid_by_1st_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated = cut_number(Administered_Dose1_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_1st_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Total deaths by US County & percent 1st dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_1st_Dose$deaths_per_1000,TRUE,FALSE,Covid_binned_by_1st_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for non-Covid deaths by first dose
num_bins = 5 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_1st_Dose_percent <- Covid_by_1st_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated = cut_number(Administered_Dose1_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_1st_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Non-Covid deaths by US County & percent 1st dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_1st_Dose$non_covid_deaths_per_1000,FALSE,TRUE,Covid_binned_by_1st_Dose_percent)

linePlotWithErrorBars(effectiveness)


#effect size for covid deaths by second dose
num_bins = 5  #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_2nd_Dose_percent <- Covid_by_2nd_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated =  cut_number(Series_Complete_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_2nd_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Covid deaths by US County & percent 2nd dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_2nd_Dose$covid_deaths_per_1000,FALSE,FALSE,Covid_binned_by_2nd_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for total deaths by second dose
num_bins = 5 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_2nd_Dose_percent <- Covid_by_2nd_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated =  cut_number(Series_Complete_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_2nd_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Total deaths by US County & percent 2nd dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_2nd_Dose$deaths_per_1000,TRUE,FALSE,Covid_binned_by_2nd_Dose_percent)

linePlotWithErrorBars(effectiveness)

#effect size for total deaths by second dose
num_bins = 5 #large enough to get a trend, small enough so that treatment groups are big enough

#split treatment sample into equal groups by percent vaccinated
Covid_binned_by_2nd_Dose_percent <- Covid_by_2nd_Dose_by_non_zero_vax_rates %>%
  mutate(
    percent_vaccinated =  cut_number(Series_Complete_Pop_Pct, n = num_bins)
  ) 
colnames(Covid_binned_by_2nd_Dose_percent)<-c("pct_vaxd","covid_deaths_per_1000","deaths_per_1000","non_covid_deaths_per_1000","percent_vaccinated")

heading  = "Non-Covid deaths by US County & percent 2nd dose vaccinated: 1 Sept to 6 Nov 2021"

effectiveness <- tabulateEffectSizes(Background_Covid_2nd_Dose$non_covid_deaths_per_1000,FALSE,TRUE,Covid_binned_by_2nd_Dose_percent)

linePlotWithErrorBars(effectiveness)


