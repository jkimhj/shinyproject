library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)
library(DT)
library(googleVis)
library(reshape)
library(reshape2)
library(leaflet)
library(zipcode)
library(data.table)
library(sm)
data("zipcode")

data = read.csv("DOHMH_Childcare_Center_Inspections.csv", stringsAsFactors = FALSE)
data = as.data.frame(data)
names(data) <- tolower(names(data))
data = data %>% group_by(day.care.id) %>% arrange(day.care.id)

center_table = select(data, day.care.id, center.name, building, street, borough, zipcode, phone, age.range, 
                      program.type, child.care.type, regulation.summary)

center_table$address = with(center_table, paste(center_table$building, center_table$street, 
                                                center_table$borough, center_table$zipcode))
center_table$building = NULL
center_table$street = NULL
center_table$borough = NULL
center_table$zipcode = NULL
center_table = center_table[,c("center.name", "address", "phone", "age.range", "program.type", 
                               "child.care.type", "regulation.summary")]

center_table = center_table[!duplicated(center_table[c("center.name")]),]

###Borough Table
df.boro = select(data, day.care.id, borough, violation.rate.percent, public.health.hazard.violation.rate, 
                 critical.violation.rate)
df.boro = df.boro %>% filter(!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
                             !is.na(critical.violation.rate))
df.boro = unique(df.boro)

###Program Table
df.program = select(data, day.care.id, borough, program.type, violation.rate.percent, public.health.hazard.violation.rate, critical.violation.rate)%>% 
  filter (!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
                  !is.na(critical.violation.rate))
df.program$program.type = toupper(df.program$program.type)
df.program = unique(df.program)

###Facility Table
df.facility = select(data, day.care.id, borough, facility.type, violation.rate.percent, public.health.hazard.violation.rate, critical.violation.rate)%>% 
  filter (!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
          !is.na(critical.violation.rate))
df.facility$facility.type = toupper(df.facility$facility.type)
df.facility = unique(df.facility)

###Zipcode Table
df.zipcode = select(data, center.name, day.care.id, building, street, zipcode, violation.rate.percent, 
                    public.health.hazard.violation.rate, critical.violation.rate)%>% 
  filter (!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
          !is.na(critical.violation.rate), !is.na(zipcode))
df.zipcode = unique(df.zipcode)
df.zipcode$zipcode = as.character(df.zipcode$zipcode)
colnames(df.zipcode)[5] = "zip"
df.zipcode = df.zipcode %>% left_join(zipcode, by="zip") 
df.zipcode$address = paste(df.zipcode$building, df.zipcode$street)


centerTooltip = paste(
  "<strong>Name: </strong>", df.zipcode$center.name,
  "<br><strong>Address: </strong>", df.zipcode$address,
  "<br><strong>Violation Rate: </strong>",df.zipcode$violation.rate.percent, 
  "<br><strong>PHH Violation Rate: </strong>",df.zipcode$public.health.hazard.violation.rate,
  "<br><strong>Critical Violation Rate: </strong>",df.zipcode$critical.violation.rate) %>% 
  lapply(htmltools::HTML)

getColor = function(x) {
  sapply(x$violation.rate.percent, function(mag){
    if(mag<30) {
      "green"
    } else if(mag<70) {
      "orange"
    } else{
      "red"
    }
  })
}

icons = awesomeIcons(
  markerColor = getColor(df.zipcode)
)

###Year Table
df.year = select(data, day.care.id, borough, violation.rate.percent, public.health.hazard.violation.rate, 
                 critical.violation.rate, inspection.date) %>%
  filter (!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
          !is.na(critical.violation.rate), !is.na(inspection.date))
df.year = unique(df.year)
df.year = df.year %>% mutate(inspection.date =as.Date(inspection.date, "%m/%d/%Y"))
df.year = df.year %>% mutate(year = as.numeric(format(inspection.date, "%Y")))
year_table = df.year %>% group_by(year, borough) %>% 
  summarise(viol_rate=mean(violation.rate.percent), phh_rate = mean(public.health.hazard.violation.rate), 
            crit_rate=mean(critical.violation.rate))
yt_melted = melt(year_table, id.vars = c('year','borough')) 

