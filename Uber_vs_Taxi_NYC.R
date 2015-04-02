
library(ggplot2)

linesize=1


#Uber pricing structure
#https://www.uber.com/cities/new-york
Uber_base_Fare=3
Uber_min_Fare=8
Uber_Fare_per_Min=0.4
Uber_Fare_per_Mile=2.15

#Taxi metered fare information
#http://www.nyc.gov/html/tlc/html/passenger/taxicab_rate.shtml
Taxi_Base_Fare=2.50
Taxi_Fare_per_mile=0.5*5
Taxi_Fare_per_min=0.5
Taxi_MTA_Surcharge=0.5
Taxi_Improvement_Surchage=0.30
Taxi_Daily_Surcharge=0.50
Taxi_Weekday_Surcharge=1.0

#Average NYC traffic speeds
#http://www.nytimes.com/2010/03/24/nyregion/24traffic.html?_r=0
Fast_mph=12
Av_mph=9.5
Slow_mph=7.5


ggplot(data =  data.frame(x=0:5)
       , aes(x=x)) +
  
  stat_function(fun = function(x) Uber_base_Fare + 
                  (x*Uber_Fare_per_Mile) + 
                  ((x/(Av_mph/60))*Uber_Fare_per_Min)
                , geom='line'
                ,size=linesize
                ,aes(colour ='Uber Av Traffic')
  ) +
  
  stat_function(fun = function(x) Uber_base_Fare + 
                  (x*Uber_Fare_per_Mile) + 
                  (((x/(Slow_mph/60))*Uber_Fare_per_Min)*.5)
                , geom='line'
                ,size=linesize
                ,aes(colour ='Uber Hvy Traffic')
  ) +
  
  stat_function(fun = function(x) Uber_base_Fare + 
                  (x*Uber_Fare_per_Mile) + 
                  ((x/(Fast_mph/60))*Uber_Fare_per_Min)
                , geom='line'
                ,size=linesize
                ,aes(colour ='Uber Lt Traffic')
  ) +
  
  stat_function(fun = function(x) Taxi_Base_Fare + 
                  Taxi_MTA_Surcharge+
                  Taxi_Improvement_Surchage+
                  Taxi_Daily_Surcharge+
                  Taxi_Weekday_Surcharge+
                  (x*Taxi_Fare_per_mile)
                , geom='line'
                ,size=linesize
                ,aes(colour ='Taxi Av/Lt Traffic')
  ) +
  
  
  stat_function(fun = function(x) Taxi_Base_Fare + 
                  Taxi_MTA_Surcharge+
                  Taxi_Improvement_Surchage+
                  Taxi_Daily_Surcharge+
                  Taxi_Weekday_Surcharge+
                  (x*Taxi_Fare_per_mile) + 
                  ((x/(Slow_mph/60))*Taxi_Fare_per_min)
                , geom='line'
                ,size=linesize
                ,aes(colour ='Taxi Hvy Traffic')
  ) +
  
  geom_line(y=Uber_min_Fare,colour="black",size=linesize,linetype="dashed")+
    
  scale_colour_manual("Legend", values = c("yellow","yellow","aquamarine", "aquamarine","aquamarine"))+
  xlab("miles")+
  ylab("$")+
  ggtitle("Uber Vs. Taxi Prices - NYC")





