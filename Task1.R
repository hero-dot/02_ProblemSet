install.packages("nycflights13")
require(nycflights13)
require(dplyr)
require(ggplot2)
require(magrittr)
str(flights)
str(airlines)
str(planes)

flights = flights
planes = planes

#a.
# Ordnen der Flugzeuge nach dem Jahr der Herstellung
orderedPlanes = planes %>%
  select(year,tailnum)%>%
  arrange(year)

# Auswählen der ältesten Flugzeuge
oldestPlanes = orderedPlanes[1:5,2]
oldestPlanes

# Herausfinden der Entfernungen der ältesten Flugzeuge
flightsOldestPlanes = semi_join(flights, oldestPlanes)

# Vergleich der durchschnittlichen Distanzen zwischen den ältesten Flugzeugen und allen Flügen
diffDist = mean(flightsOldestPlanes$distance) - mean(flights$distance)
diffDist

#b.

# Anzahl der Flugzeuge aus der Planes Tabelle, die von New York losgeflogen sind
numIncludedPlanes = flights %>%
  semi_join(.,planes)%>%
  select(tailnum)%>%
  unique()%>%
  nrow()
numIncludedPlanes

# Anzahl an Flugzeugen ohne Herrstellungsjahr
numMissDate = nrow(filter(planes, is.na(year)))
numMissDate

#c.
# Nicht sicher, ob sich die Aufgabe auf die Flugzeuge aus b bezieht
# Darstellung der Herstellungsdaten für alle Flugzeuge aus der planes Tabelle
g1c = planes%>%
  filter(!is.na(year))%>%
  ggplot(.,aes(x=year))+
  geom_histogram(bins=length(unique(filter(planes, !is.na(year))$year)), fill = "#75bdab", color = "white")
g1c
# Die meisten Flugzeuge in der Planes Tabelle stammen aus den Jahren um 2000
# Es sidn auch noch sehr alte Flugzeuge vorhanden, aber auch aktuelle.

# Manuelles Bestimmen der Anzahl der Flugzeuge
planes%>%
  select(year,tailnum)%>%
  group_by(year)%>%
  summarise(total=length(year))

#d. 
planes[grepl("AIRBUS",planes$manufacturer),"manufacturer"]="AIRBUS"
planes[grepl("MCDONNELL",planes$manufacturer),"manufacturer"]="MCDONNELL"

g1d = planes%>%
  select(manufacturer,year)%>%
  filter(manufacturer %in% c("BOEING","AIRBUS","MCDONNELL","EMBRAER"))%>%
  filter(!is.na(year))%>%
  mutate(decYear = ifelse(year<2013, "before 2013","in 2013"))%>%
  ggplot(.,aes(x=year,fill=manufacturer))+
  geom_bar()+
  facet_grid(.~decYear,scales="free")
g1d

# Die meisten Flugzeuge stammen von Boeing. Der zweit wichtigste Hersteller ist Airbus
# Bis zum Anfang der war McDonnell noch als Hersteller vertreten
# McDonnell verschwindet dann als Hersteller und Embraer wird wichtiger
# Verglichen zum zeitlichen Trend spiegelt das Jahr 2013 den Trend der vergangenen Jahre wider.


#e

TailFlightsDelay = flights%>%
  select(arr_delay,tailnum)%>%
  inner_join(.,planes)%>%
  select(arr_delay,manufacturer)%>%
  filter(manufacturer %in% c("BOEING","AIRBUS","MCDONNELL","EMBRAER"))%>%
  filter(!is.na(arr_delay))%>%
  filter(!(arr_delay>150 | arr_delay < -50))%>%
  ggplot(.,aes(x=arr_delay,fill=manufacturer))+
  geom_bar()
TailFlightsDelay

#Darstellung mit dem Mittelwert der Verspätung
TailFlightsDelay = flights%>%
  select(arr_delay,tailnum)%>%
  inner_join(.,planes)%>%
  select(arr_delay,manufacturer)%>%
  filter(manufacturer %in% c("BOEING","AIRBUS","MCDONNELL","EMBRAER"))%>%
  filter(!is.na(arr_delay))%>%
  group_by(manufacturer)%>%
  summarise(delay=mean(arr_delay))%>%
  ggplot(.,aes(x=manufacturer,y=delay))+
  geom_bar(stat="identity")
TailFlightsDelay

#2
weather = weather

str(weather)
str(weather$wind_speed)
grouped_by_month = group_by(weather,month)
grouped_by_month
meantemp = summarise(grouped_by_month, mean = mean(temp,na.rm = TRUE))
meantemp

#a
min(weather$temp,na.rm=T)
max(weather$temp,na.rm=T)
mean(weather$temp,na.rm=T)

g2a = weather%>%
  select(month,temp)%>%
  group_by(month)%>%
  ggplot(.,aes(x=as.factor(month),y=temp))+
  geom_point(aes(colour=temp))+
  scale_colour_gradient2(low = 'blue', mid = 'green', high = 'red', midpoint = 55)+
  geom_smooth(aes(x=month,y=temp),colour = 'red', size = 1)+
  scale_y_continuous(limits = c(5,105), breaks = seq(5,105,10))+
  ggtitle('Monthly average Temperature')+
  xlab('Month') + ylab('Temperature')
g2a

#b
maxWind = weather%>%
  select(wind_speed)%>%
  filter(!is.na(wind_speed))%>%
  subset(wind_speed>40)%>%
  arrange(desc(wind_speed))
maxWind


minWind = weather%>%
  select(wind_speed)%>%
  filter(!is.na(wind_speed))%>%
  subset(wind_speed<1)%>%
  arrange(-desc(wind_speed))
minWind

#c
g2c = weather%>%
  select(dewp,humid,month)%>%
  group_by(month)%>%
  ggplot(.)+
  geom_smooth(aes(x=humid, y=dewp))+
  ggtitle('Dew point in relation to humidity ')+
  xlab('Humidity')+
  ylab('Dew point')
g2c

# Der Zusammenhang zwischen ist fast für die gesamte Breite
# der Luftfeuchtigkeit linear. Erst ab einer Luftfeuchtigkeit von 
# 75% ändert sich das. Der Taupunkt steigt dann nicht mehr für höhere
# Temperaturen.

g2c1 = weather%>%
  select(dewp,humid,month)%>%
  group_by(month)%>%
  summarise(mean_dewp=mean(dewp),mean_humid=mean(humid))%>%
  ggplot(.)+
  geom_smooth(aes(x=month,y=mean_dewp))+
  geom_smooth(aes(x=month,y=mean_humid))+
  scale_x_continuous(limits = c(1,12),breaks=seq(1,12,1))
g2c1

#d

g2d = weather%>%
  select(precip,visib)%>%
  ggplot(.)+
  geom_smooth(aes(x=visib,y=precip))
g2d

# Es wird intuitiv angenommen, dass ein hoher Niederschlag zu einer schlechten Sicht führt.
# Dies wird auch bestätigt, allerdings ist für eine besonders schlechte Sicht der Niederschlag
# nicht sehr hoch. 

g2d1 = weather%>%
  select(precip,visib,month)%>%
  group_by(month)%>%
  summarise(mean_precip=mean(precip,na.rm = TRUE),mean_visib=mean(visib,na.rm = TRUE))%>%
  ggplot(.)+
  geom_smooth(aes(x=month,y=mean_precip))+
  geom_smooth(aes(x=month,y=mean_visib))
g2d1 
 

#e
dayPrecip = weather%>%
  select(month,day,precip)%>%
  filter(precip>0)%>%
  group_by(month)%>%
  summarise(daysRain = n_distinct(day))
dayPrecip

#f

g2f1 = weather%>%
  select(time_hour,wind_speed,precip)%>%
  inner_join(.,flights)%>%
  select(time_hour,wind_speed,precip,arr_delay)%>%
  ggplot(.,aes(y=arr_delay))+
  geom_smooth(aes(x=precip))
g2f1

# Mit zunehmendem Niederschlag steigt auch die Verspätung
# Für niedrige Niederschläge ist allerdings kein Zusammenhang zu erkennen.
# Erst für ein Niederschlag ab 0.3 steigt die Verspätung steil an.
# Ab einem Wert von 0.75 nimmt die Verspätung wieder ab.

g2f2 = weather%>%
  select(time_hour,wind_speed,precip)%>%
  inner_join(.,flights)%>%
  select(time_hour,wind_speed,precip,arr_delay)%>%
  ggplot(.,aes(y=arr_delay))+
  geom_smooth(aes(x=wind_speed))
g2f2

# Die Windgeschwindigkeit reduziert die Verspätung.
# Die Verspätung wird durch den Einfluss sogar negativ.
# Die Flüge kommen vor der geplanten Ankunftszeit an. 
# Beträgt die Windgeschwindigkeit um 500 ist die Verspätung am niedrigsten.
# Für niedrigere und höhere Windgeschwindigkeiten ist die Verspätung höher.


#3.
library(Lahman)
library(magrittr)
install.packages("Lahman")
salaries = Salaries
str(salaries)
teams = Teams
str(teams)

#a
YearlyPayroll = salaries %>%
  select(yearID,teamID,salary)%>%
  group_by(yearID,teamID)%>%
  summarise(TeamYearPay = sum(as.numeric(salary)))%>%
  ggplot(.,aes(x=yearID,y=TeamYearPay,color=teamID))+
  geom_line(aes(lintetype=teamID))
  
YearlyPayroll
YearlyPayroll1 = salaries %>%
  select(yearID,teamID,salary)%>%
  group_by(yearID,teamID)%>%
  summarise(TeamYearPay = sum(as.numeric(salary)))
YearlyPayroll1

#b
WinsOak = teams %>%
  select(teamID,yearID,W)%>%
  filter(teamID=='OAK')%>%
  inner_join(., YearlyPayroll1,by=c('teamID','yearID'))%>%
  ggplot(.,aes(x=W,y=TeamYearPay))+
  geom_point()+
  geom_smooth()
WinsOak


#c
LeagueAvg = teams %>%
  select(lgID,divID,teamID,yearID,W)%>%
  filter(yearID>=1990 & yearID<2014)%>%
  inner_join(., YearlyPayroll1,by=c('teamID','yearID'))%>%
  mutate(WinPDollar = TeamYearPay/W)%>%
  group_by(yearID)%>%
  summarise(leagAvg=sum(WinPDollar)/37)

WinsDollars = teams %>%
  select(lgID,divID,teamID,yearID,W)%>%
  filter(yearID>=1990 & yearID<2014)%>%
  inner_join(., YearlyPayroll1,by=c('teamID','yearID'))%>%
  mutate(WinPDollar = TeamYearPay/W)%>%
  inner_join(.,LeagueAvg,by=c('yearID'))%>%
  ggplot(.,aes(x=yearID,y=WinPDollar,color=teamID))+
  geom_line(aes(lintetype=teamID))+
  geom_line(aes(x=yearID,y=leagAvg,colour='red'))+
  facet_wrap(lgID~divID)
  
WinsDollars  

#d

Performance = teams %>%
  select(name,teamID,yearID,W)%>%
  filter(yearID>=1990 & yearID<2014)%>%
  filter(grepl('Oakland',.$name)|grepl('Yankees',.$name)|grepl('Red Sox',.$name)|
           grepl('Blue Jays',.$name)|grepl('Kansas',.$name))%>%
  inner_join(., YearlyPayroll1,by=c('teamID','yearID'))%>%
  mutate(WinPDollar = TeamYearPay/W)%>%
  inner_join(.,LeagueAvg,by=c('yearID'))%>%
  ggplot(.)+
  geom_line(aes(x=yearID,y=WinPDollar, color=teamID,lintetype=teamID))+
  geom_line(aes(x=yearID,y=leagAvg, color = 'red',name='Average'))

Performance
#e

MedianYearlyPayroll = salaries %>%
  select(yearID,teamID,salary)%>%
  group_by(yearID,teamID)%>%
  summarise(MedianTeamYearPay = median(salary))

MedianLeagueAvg = MedianYearlyPayroll%>%
  group_by(yearID)%>%
  summarise(MedLeagAvg=sum(MedianTeamYearPay)/37)

Robustness = teams %>%
  select(name,teamID,yearID,R)%>%
  filter(yearID>=1990 & yearID<2014)%>%
  filter(grepl('Oakland',.$name)|grepl('Yankees',.$name)|grepl('Red Sox',.$name)|
           grepl('Blue Jays',.$name)|grepl('Kansas',.$name))%>%
  inner_join(., MedianYearlyPayroll,by=c('teamID','yearID'))%>%
  mutate(RunPMedianDollar = MedianTeamYearPay/R)%>%
  inner_join(.,MedianLeagueAvg,by=c('yearID'))%>%
  mutate(MedLeagAvgPRun = MedLeagAvg/R)%>%
  ggplot(.)+
  geom_line(aes(x=yearID,y=RunPMedianDollar, color=teamID,lintetype=teamID))+
  geom_step(aes(x=yearID,y=MedLeagAvgPRun, color = 'red',name='Average'))

Robustness
