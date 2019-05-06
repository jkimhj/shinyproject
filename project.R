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


data = read.csv("DOHMH_Childcare_Center_Inspections.csv", stringsAsFactors = FALSE)

data = tbl_df(data)
names(data) <- tolower(names(data))
data = data %>% group_by(day.care.id) %>% arrange(day.care.id)



###Delete column Child.Care.Type since it is same as colum Program.Type 
data$child.care.type = NULL
#Delete average columns
data$average.violation.rate.percent = NULL
data$average.public.health.hazard.violation.rate = NULL
data$average.total.educational.workers = NULL
data$average.critical.violation.rate = NULL

###Replace empty value in violation.category to NA
data$violation.category[data$violation.category==""]="NA"


###Inspections by Borough
df.boro = select(data, day.care.id, borough, violation.rate.percent, public.health.hazard.violation.rate, 
                 critical.violation.rate)
df.boro = df.boro %>% filter(!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
                             !is.na(critical.violation.rate))
df.boro = unique(df.boro)
boro_violation_mean = df.boro %>% group_by(borough) %>% summarize(Violation_Rate = mean(violation.rate.percent), 
                                            PHH_Violation_Rate = mean(public.health.hazard.violation.rate), 
                                            Critical_Violation_Rate = mean(critical.violation.rate))

##Violation Rate
ggplot(data=boro_violation_mean, aes(x= borough, y=Violation_Rate, fill=borough)) +
  geom_bar(stat="identity") +
  labs(title='Violations Rate by Borough',
       x='Borough',
       y= 'Rate (%)') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

#Public Health Hazard Violation Rate
ggplot(data=boro_violation_mean, aes(x= borough, y=PHH_Violation_Rate, fill=borough)) +
  geom_bar(stat="identity") +
  labs(title='PHH Violations Rate by Borough',
       x='Borough',
       y= 'Rate (%)') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

#Critical Violation rate
ggplot(data=boro_violation_mean, aes(x= borough, y=Critical_Violation_Rate, fill=borough)) +
  geom_bar(stat="identity") +
  labs(title='Critical Violations Rate by Borough',
       x='Borough',
       y= 'Rate (%)') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

###Inspections by Program.Type(Infant Toddler, Preschool, School Age Camp, All Age Camp)
df.program = select(data, day.care.id, borough, program.type, violation.rate.percent, public.health.hazard.violation.rate, critical.violation.rate) %>% filter (!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), !is.na(critical.violation.rate))

df.program$program.type = toupper(df.program$program.type)
df.program = unique(df.program)
program_table = df.program %>% group_by(program.type) %>% 
  summarise(viol_rate=mean(violation.rate.percent), phh_rate = mean(public.health.hazard.violation.rate), crit_rate=mean(critical.violation.rate))
pt_melted = melt(program_table)

p <- ggplot(pt_melted, aes(x = variable, y = value)) + 
  geom_line(aes(color = program.type, group = program.type))+
  labs(title='Violations Rate by Program Type',
       x='Program Type',
       y= 'Rate (%)') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())

ggplotly(p)


###Inspections by Zipcode
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
  "<br><strong>Critical Violation Rate: </strong>",df.zipcode$critical.violation.rate) %>% lapply()


leaflet(df.zipcode) %>% addTiles() %>%
  addAwesomeMarkers(lng=df.zipcode$longitude, lat=df.zipcode$latitude,
                    label = centerTooltip
                    # clusterOptions=markerClusterOptions() 
                    )


####Inspections by Year
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


yt_melted_1 = yt_melted[yt_melted["variable"]=="viol_rate",]
ggplot(data=yt_melted_1, aes(x=year, y=value, color=borough)) +
  geom_line(aes(color=borough))+
  labs(title='Violations Rate by Year',
       x='Violation Type',
       y= 'Rate (%)') +
  scale_fill_brewer(palette='Set1') +
  theme_bw() 
