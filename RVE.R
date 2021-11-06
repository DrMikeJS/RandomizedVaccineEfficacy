library("ggplot2")
library("dplyr")   

#load data
county_vax_rates <- read.csv("J:/OneDrive/Covid/COVID-19_Vaccinations_in_the_United_States_County.csv",sep=";")
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh

county_pop_2019 <- read.csv("J:/OneDrive/Covid/ERS_USDA_People.csv")
#https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/download-the-data/
county_pop_2019 <- select(county_pop_2019,ï..FIPS,TotalPopEst2019)

county_covid_rates <- read.csv("J:/OneDrive/Covid/NYT_us-counties.csv")
#https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv

#get start and end points of running totals for period of interest
covid_Oct <- select(filter(county_covid_rates, date == "2021-10-31"),c(fips,cases,deaths))
covid_Aug <- select(filter(county_covid_rates, date == "2021-07-30"),c(fips,cases,deaths))

#remove data where county is unknown
covid_Aug <- covid_Aug[!is.na(covid_Aug$fips) ,]
covid_Oct <- covid_Oct[!is.na(covid_Oct$fips) ,]

#join start and end points
covid_Aug_Oct <- merge(covid_Aug,covid_Oct, by = "fips")

#calculate covid rates over period of interest
covid_Aug_Oct$cases <-  covid_Aug_Oct$cases.y - covid_Aug_Oct$cases.x
covid_Aug_Oct$deaths <- covid_Aug_Oct$deaths.y - covid_Aug_Oct$deaths.x

#remove obsolete columns
covid_Aug_Oct <- select(covid_Aug_Oct,fips,cases,deaths)

#remove bad (or revised) data, i.e. negative cases or negative deaths
covid_Aug_Oct <- covid_Aug_Oct[covid_Aug_Oct$cases>=0 ,]
covid_Aug_Oct <- covid_Aug_Oct[covid_Aug_Oct$deaths>=0 ,]

#prepare for join to other data
covid_Aug_Oct <- covid_Aug_Oct %>% rename(FIPS = fips)

#calculate percent vaccinated in each county (for simplicity, use mid-point of period of interest)
pct_vaccinated_mid_period <- select(filter(county_vax_rates, ï..Date == "09/15/2021"),c(FIPS,Administered_Dose1_Pop_Pct,Series_Complete_Pop_Pct))

#join rates and vaccination data
covid_by_vax_rates <- merge(pct_vaccinated_mid_period, covid_Aug_Oct, by = "FIPS")

#prepare population data for join
county_pop_2019 <- county_pop_2019 %>% rename(FIPS = ï..FIPS)

#join population data to other data
covid_by_vax_rates <- merge(county_pop_2019, covid_by_vax_rates, by = "FIPS")

#calculate covid rate per 1000 population (not necessary if already provided in some datasets)
covid_by_vax_rates$cases_per_1000 = covid_by_vax_rates$cases/covid_by_vax_rates$TotalPopEst2019*1000
covid_by_vax_rates$deaths_per_1000 = covid_by_vax_rates$deaths/covid_by_vax_rates$TotalPopEst2019*1000

#actual cases by first dose
heading  = "Covid cases by US County and percent first dose vaccinated: Aug-Oct 2021"
percent_vaccinated <- covid_by_vax_rates$Administered_Dose1_Pop_Pct
actual_covid_per_1000 <- covid_by_vax_rates$cases_per_1000
current_effect <- round(-cor(actual_covid_per_1000, percent_vaccinated),1)
effectiveness = paste("Actual Cases",current_effect)
covid_per_1000=actual_covid_per_1000
FirstDoseCases <- cbind.data.frame(percent_vaccinated,covid_per_1000,effectiveness)
size = max(actual_covid_per_1000)

#function to simulate target effectiveness based on current distribution
SimCovid <- function(effectiveness,covid_per_1000,event){
  
  current_effect <- -cor(covid_per_1000, percent_vaccinated)
  adj_covid_per_1000 = covid_per_1000
  l<-length(covid_per_1000)
  
  #loop through each county and move its current value towards its randomised ideal value so long as the correlation is improved
  for (iterations in 1:20){
    
    for (i in 1:l){
      
      adj_covid_per_1000[i] = (covid_per_1000[i]+size-size*percent_vaccinated[i]/100*effectiveness*rlnorm(1,1,0.5))/2
      last_effect <- abs(effectiveness-current_effect)
      current_effect <- -cor(adj_covid_per_1000,percent_vaccinated)
      
      print(current_effect)
      print(iterations)
      
      if(abs(effectiveness-current_effect)<last_effect&&adj_covid_per_1000[i]>=0&&adj_covid_per_1000[i]<=size) {
        covid_per_1000[i]=adj_covid_per_1000[i] 
        if(round(current_effect,1)==effectiveness) break #exit if target correlation reached
      }
      
      else {
        adj_covid_per_1000[i]=covid_per_1000[i]
      }
    }
  }
  
  current_effect <- -cor(covid_per_1000, percent_vaccinated)
  
  effectiveness=paste(event,round(current_effect,1))
  return(cbind.data.frame(percent_vaccinated,covid_per_1000,effectiveness))
}

#simulated cases by first dose
FirstDoseCases <- rbind(SimCovid(0.2,actual_covid_per_1000,"Simulated Cases"),FirstDoseCases)
FirstDoseCases <- rbind(SimCovid(0.4,actual_covid_per_1000,"Simulated Cases"),FirstDoseCases)
FirstDoseCases <- rbind(SimCovid(0.6,actual_covid_per_1000,"Simulated Cases"),FirstDoseCases)
FirstDoseCases <- rbind(SimCovid(0.8,actual_covid_per_1000,"Simulated Cases"),FirstDoseCases)

#function to make graphs
scatterplots <- function(inputdata){
  ggplot(inputdata, aes(percent_vaccinated,covid_per_1000, group=effectiveness)) + 
    geom_smooth(method="lm", color="Black") + 
    geom_rug(sides="b") + 
    geom_point(aes(fill=effectiveness), alpha=1/2, shape=21) +
    facet_wrap(~ effectiveness) +
    ggtitle(heading)
}

scatterplots(FirstDoseCases)

#actual deaths by first dose
heading  = "Covid deaths by US County and percent first dose vaccinated: Aug-Oct 2021"
actual_covid_per_1000 <- covid_by_vax_rates$deaths_per_1000
current_effect <- round(-cor(actual_covid_per_1000, percent_vaccinated),1)
effectiveness = paste("Actual Deaths",current_effect)
covid_per_1000=actual_covid_per_1000
FirstDoseDeaths <- cbind.data.frame(percent_vaccinated,covid_per_1000,effectiveness)
size = max(actual_covid_per_1000)

#simulated deaths by first dose
FirstDoseDeaths <- rbind(SimCovid(0.2,actual_covid_per_1000,"Simulated Deaths"),FirstDoseDeaths)
FirstDoseDeaths <- rbind(SimCovid(0.4,actual_covid_per_1000,"Simulated Deaths"),FirstDoseDeaths)
FirstDoseDeaths <- rbind(SimCovid(0.6,actual_covid_per_1000,"Simulated Deaths"),FirstDoseDeaths)
FirstDoseDeaths <- rbind(SimCovid(0.8,actual_covid_per_1000,"Simulated Deaths"),FirstDoseDeaths)
scatterplots(FirstDoseDeaths)

#actual cases by second dose
heading  = "Covid cases by US County and percent second dose vaccinated: Aug-Oct 2021"
percent_vaccinated <- covid_by_vax_rates$Series_Complete_Pop_Pct
actual_covid_per_1000 <- covid_by_vax_rates$cases_per_1000
current_effect <- round(-cor(actual_covid_per_1000, percent_vaccinated),1)
effectiveness = paste("Actual Cases",current_effect)
covid_per_1000=actual_covid_per_1000
SecondDoseCases <- cbind.data.frame(percent_vaccinated,covid_per_1000,effectiveness)
size = max(actual_covid_per_1000)

#simulated cases by second dose
SecondDoseCases <- rbind(SimCovid(0.2,covid_by_vax_rates$cases_per_1000,"Simulated Cases"),SecondDoseCases)
SecondDoseCases <- rbind(SimCovid(0.4,covid_by_vax_rates$cases_per_1000,"Simulated Cases"),SecondDoseCases)
SecondDoseCases <- rbind(SimCovid(0.6,covid_by_vax_rates$cases_per_1000,"Simulated Cases"),SecondDoseCases)
SecondDoseCases <- rbind(SimCovid(0.8,covid_by_vax_rates$cases_per_1000,"Simulated Cases"),SecondDoseCases)
scatterplots(SecondDoseCases)

#actual deaths by second dose
heading  = "Covid deaths by US County and percent second dose vaccinated: Aug-Oct 2021"
actual_covid_per_1000 <- covid_by_vax_rates$deaths_per_1000
current_effect <- round(-cor(actual_covid_per_1000, percent_vaccinated),1)
effectiveness = paste("Actual Deaths",current_effect)
covid_per_1000=actual_covid_per_1000
SecondDoseDeaths <- cbind.data.frame(percent_vaccinated,covid_per_1000,effectiveness)
size = max(actual_covid_per_1000)

#simulated deaths by second dose
SecondDoseDeaths <- rbind(SimCovid(0.2,covid_by_vax_rates$deaths_per_1000,"Simulated Deaths"),SecondDoseDeaths)
SecondDoseDeaths <- rbind(SimCovid(0.4,covid_by_vax_rates$deaths_per_1000,"Simulated Deaths"),SecondDoseDeaths)
SecondDoseDeaths <- rbind(SimCovid(0.6,covid_by_vax_rates$deaths_per_1000,"Simulated Deaths"),SecondDoseDeaths)
SecondDoseDeaths <- rbind(SimCovid(0.8,covid_by_vax_rates$deaths_per_1000,"Simulated Deaths"),SecondDoseDeaths)
scatterplots(SecondDoseDeaths)

  


