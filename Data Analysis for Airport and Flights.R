#Kevin Jericho Pranata
#TP054154

#Loading Library
library(ggplot2)
library(dplyr)

#To Read CSV File
assign = read.csv("D:\\4. Hourly weather data.csv")

summary(assign)
#Analysis 1
#Dew Point on Level of Humidity
#The chart below represent how Dew Point resulting the Level of Humidity in 2013 according to weatherspark.com
#ggplot() used to initializes a ggplot object.
ggplot() +
  #geom_point to create a point chart
  geom_point(assign, mapping = aes(x = factor(month), y = dewp, color = 'Dew Point(F)', group = 1), alpha = 0.2)+
  #geom_react to create a rectangle which will indicate the range of something inside the data
  geom_rect(assign, mapping = aes(xmin = min(month), xmax = max(month), ymin = min(dewp), ymax =55,color = 'Dry'),fill = 'transparent')+
  geom_rect(assign, mapping = aes(xmin = min(month), xmax = max(month), ymin = 55, ymax = 60, color = 'Comfortable'),fill = 'transparent')+
  geom_rect(assign, mapping = aes(xmin = min(month), xmax = max(month), ymin = 60, ymax = 70, color = 'Muggy'),fill = 'transparent')+
  geom_rect(assign, mapping = aes(xmin = min(month), xmax = max(month), ymin = 70, ymax = 75, color = 'Humid'),fill = 'transparent')+
  #geom_hline set a creating an horizontal line in the chart
  geom_hline(aes(yintercept = 76, color ="Oppressive"), size = 1)+
  #scale_color_manual used to set the color of the ggplot object
  scale_color_manual(breaks = c("Dew Point(F)","Dry","Comfortable","Muggy","Humid","Oppressive"), 
                     values = c("red","deepskyblue","goldenrod","forestgreen","darkorange","rosybrown"))+
  #used to gives title in the visualization, rename the variables in the graph
  labs(title = 'Level of Humidity in each month in 2013', x = 'Month', y = 'Dew Point (F)', fill = "", color = "")

#Analysis 2
#Temp - Dew Point and Humid
#The chart below represent how Temperature and Dew Point affect Humidity.
#Humidity is calculated by how close Temperature - Dew Point. The closer the gap, the higher humidity.
#Dew Point Temperature is Never greater than the temperature itself. Therefore,if the Temperature of Dew Point and Temperature is Equal, 
#it said to be a Saturation where the moisture inside the air is condensate and coming out in form of snow, fog, frost, clouds.
#The closer the difference between Temperature and Dew point, the Higher the humidity.
#The more moisture, the harder a plane to fly.
ggplot()+
  #geom_tile used to creates a visualization of tiles chart
  geom_tile(assign,mapping = aes(x=temp,y=dewp,fill=humid),width=2,height=2)+
  #scale_fill_gradient is used to edit the gradient color of the tile chart
  scale_fill_gradient(low="cornflowerblue", high="blue4")+
  #ylim used to set the range of y axis
  ylim(-10,100)+
  #geom_abline used to create a staight diagonal line from 0 in the graph
  geom_abline(color = "red", size = 1)+
  labs(title = 'Tile chart of Temperature & Dew Point affect Humidity', x = 'Temperature (F))', y = 'Dew Point(F)', fill ="Humidity")


#Analysis 3
#Wind Speed in 2013
#Actually, Airplanes are able to fly even if the Wind Speed is high. The only thing that caused delay to airplanes is the horizontal winds -
#which also called Cross Winds that exceed 34 mph - 40 mph can cause delay or even flight cancel.
#It is not recommended for an airplanes to take off and landing if the crosswind exceed 34-40 mph.
#The chart below represent the wind speed in each day in 2013.
ggplot()+
  geom_point(assign, mapping = aes(x = factor(day), y = wind_speed, color = "Wind Speed", alpha = 0.2, group = 1))+
  geom_hline(assign, mapping = aes(yintercept = 34, color ="Max Wind Speed Range"), linetype = "dotdash")+
  geom_hline(assign, mapping = aes(yintercept = 40, color ="Max Wind Speed Range"), linetype = "dotdash")+
  scale_color_manual(breaks = c("Max Wind Speed Range","Wind Speed"), values = c("red","darkgoldenrod1","dodgerblue"))+
  labs(title = 'Point Chart of Wind Speed in 2013', x = 'Days', y = 'Wind Speed (mph)', alpha="", color="")+
  #facet_Wrap used to group the visualization, in this case by month
  facet_wrap(~month)

#Analysis 4
#Precipitation Rate in 2013
#Precipitation is any liquid or frozen water that forms in the atmosphere and falls back to the Earth.
#If the precipitation rate in inches is higher or equal to0.4, it can be said as a wet day
#The chart below shows the average precipitation rate in each month in 2013.

#this part is used to manipulate the data, in this case the data willl grouped by origin,year,month,day.
#this also will filtered by the origin
#the data will summarise the mean from the assign data set, in this case precipitate 
mdata4jfk <- assign %>% group_by(origin, year,month,day) %>% filter(origin=="JFK")%>%summarise(dailyprecipitate=mean(precip, na.rm=TRUE))
mdata4lga <- assign %>% group_by(origin, year,month, day) %>% filter(origin=="LGA")%>%summarise(dailyprecipitate=mean(precip,na.rm=TRUE))
ggplot()+
  geom_point(mdata4jfk, mapping = aes(x = factor(month), y = dailyprecipitate, color = "JFK", group = 1))+
  geom_point(mdata4lga, mapping = aes(x = factor(month), y = dailyprecipitate, color = "LGA", group = 1))+
  geom_hline(aes(yintercept = 0.04, color ="Minimal Wet Day"), linetype = "dotdash", size = 1)+
  scale_color_manual(breaks = c("JFK","LGA","Minimal Wet Day"), values = c("blue", "red", "forestgreen"))+
  labs(title = 'Point Chart of Precipitate in 2013', x = 'Month', y = 'Precipitate (Inches)', color = "") 

#Analysis 5
#Pressure x Month
#Air pressure affect the ability of airplanes to fly. Airplanes is designed to make the air move faster over the-
#top of the wing. If the air move faster, the air pressure decreased. Hence, the air pressure on the top of the wing-
#is lower than in the bottom of the wing. The differences between the air pressure creating a force to lifts up airplanes.
#The higher the pressure is, the easier for plane to fly, a lower pressure will cause planes unable to fly
#However, the pressure should not stay too far away from the Optimal Pressure which is the green line in the chart that is around 1013.25.
#Therefore, the pressure of the sea level is recommended not lower than 1010 and 1020 (the red line in the graph)
mdata5jfk <- assign %>% group_by(origin,year,month) %>% filter(origin=="JFK")%>%summarise(monthpressure=mean(pressure,na.rm=TRUE))
mdata5lga <- assign %>% group_by(origin, year,month) %>% filter(origin=="LGA")%>%summarise(monthpressure=mean(pressure,na.rm=TRUE))
ggplot()+
  geom_line(mdata5jfk, mapping = aes(x = factor(month), y = monthpressure, color = "JFK",group = 1))+
  geom_line(mdata5lga, mapping = aes(x = factor(month), y = monthpressure, color = "LGA",group = 1))+
  geom_hline(yintercept = 1013.25, color = "Green", linetype = "dotdash", size = 2)+
  geom_hline(yintercept = 1020, color = "red", linetype = "dotdash", size = 2)+
  geom_hline(yintercept = 1010, color = "red",  linetype = "dotdash", size = 2)+
  scale_color_manual(breaks = c("JFK","LGA"), values = c("blue","red"))+
  labs(title = 'Line Chart of Pressure in 2013', x = 'Month', y = 'Pressure (millibars)', color ="")+
  facet_wrap(~origin)       


#Analysis 6
#Wind Gust and Wind Speed
#Wind Gust is basically a sudden increase of wind speed for a short time.
#If the changes of the wind speed is too significant, a horrible wind can be formed. For instance, wind sheer or even thunder storm.
#According to WWMT, if a wind gust is sudden increased to 58 mph or more, a thunderstorm may happen.
#The chart below show the average increaseof wind gust in each month
mdata6 <- assign %>% group_by(origin,year,month)%>% summarise(dailywindspeed=mean(wind_speed,na.rm=TRUE),
                                                              dailywindgust=mean(wind_gust,na.rm=TRUE))
ggplot()+
  geom_point(mdata6, mapping  = aes(x=factor(month), y = dailywindspeed, color ="Wind Speed", group = 1))+
  geom_line(mdata6, mapping  = aes(x=factor(month), y = dailywindspeed, color ="Wind Speed", group = 1))+
  geom_point(mdata6, mapping  = aes(x=factor(month), y = dailywindgust, color ="Wind Gust", group = 1))+
  geom_line(mdata6, mapping  = aes(x=factor(month), y = dailywindgust, color ="Wind Gust", group = 1))+
  geom_hline(mdata6, mapping = aes(yintercept = 58, color ="Thunderstorm"), linetype = "dotdash", size = 2)+
  scale_color_manual(breaks = c("Wind Speed","Wind Gust","Thunderstorm"), values = c("blue","plum","red"))+
  labs(title = 'Data of Wind Gust and Wind Speed in 2013', x = 'Month', y = 'mph', color = "")+
  facet_wrap(~origin)

#Analysis 7
#Visibility in 2013
#Visibility is an important thing to be concern to fly airplanes.
#The optimal visibility in miles is 10 miles
#However, in aviation, visibility at least 3 miles is already allowed to fly.


#Graph 1, Visibility in Each Day in 2013
ggplot()+
  geom_point(assign, mapping = aes(x = factor(day), y = visib, color = "visibility", group = 1))+
  geom_hline(assign, mapping = aes(yintercept = 3, color ="Minimum Visibility"), size = 1)+
  geom_hline(assign, mapping = aes(yintercept = 10, color ="Excellent Visibility"),size = 1)+
  scale_color_manual(breaks = c("Minimum Visibility","Excellent Visibility", "visibility"), values = c("red","darkgoldenrod1","forestgreen"))+
  labs(title = 'Point Chart of Visibiity Each Month', x = 'Days', y = 'Visibility (miles)', color ="")+
  facet_wrap(~month)

#Graph 2, Average Visibility for each month in 2013
mdata7 <- assign %>% group_by(origin,year,month)%>% summarise(monthvisib=mean(visib,na.rm=TRUE))
ggplot()+
  geom_point(mdata7, mapping = aes(x = factor(month), y = monthvisib, color = "Indicator", group = 1))+
  geom_hline(mdata7, mapping = aes(yintercept = 3, color ="Minimum Visibility"), linetype = "dotdash", size = 2)+
  geom_hline(mdata7, mapping = aes(yintercept = 10, color ="Excellent Visibility"), linetype = "dotdash", size = 2)+
  scale_color_manual(breaks = c("Indicator","Minimum Visibility","Excellent Visibility"), values = c("red","darkgoldenrod1","dodgerblue"))+
  labs(title = 'Point Chart of Visibility in 2013', x = 'Month', y = 'Visibility (miles)', color ="")+
  facet_wrap(~origin)

#Analysis 8
#Temperature and Pressure
#Temperature affect The sea level pressure. The point is, the higher the temperature, the pressure is decreasing, vice versa
#If the affected pressure exceed the minimum and maximum pressure for airplanes to fly, it may cause cancel or delays on the flight itself
#The chart below, shows how the temperature affect the pressure in 2013.
ggplot(assign, mapping = aes(x = temp, y = pressure, color = origin, na.rm = TRUE))+
  geom_point(alpha = 0.2)+
  #stat_smooth is used to create a line that represent the mean form the data
  stat_smooth(method="lm", color = 'darkblue')+
  #xlim is used to set the range of x-axis
  xlim(0,100)+
  ylim(990,1040)+
  labs(title = 'Scatter Plot Temperature affect Sea Level Pressure', x = 'Temperature(F)', y = 'Pressure (millibars)', color ="")+
  facet_wrap(~origin)

#Analysis 9
#Humidity and Precipitation
#Humidity can also affect Precipitation. Basically, humidity is basically the amount of water vapor or moisture in the air.
#If the humidity is high, moisture from the air will fall from the sky to Earth such as snow, rain, etc. This process is precipitation.
#The point is if the humidity is increasing, that means the precipitation is increased.
#The point chart below shows the humidity and precipitation in 2013
ggplot()+
  geom_point(assign, mapping = aes(x = humid, y = precip, color = "Visibility"), alpha = 0.2)+
  geom_hline(assign, mapping = aes(yintercept = 0.04, color ="Wet"), size = 1)+
  scale_colour_manual(breaks = c("Visibility","Wet"), values = c("darkorange","darkgreen"))+
  labs(title = 'Point Chart of Precipitation affect Humidity', x = 'Humidity', y = 'Precipitation (Inches)', alpha = "", color = "")

#Analysis 10
#Best Day to Fly based on Humidity and Visibility
#The graph shows the humidity and visibility in Spring season which is March to May.
#We can see that the visibility in every month is almost reached 10 miles which is excellent to have a flight.
#However, based on the humidity, the optimal humidity should be around 40 to 60.
#we can conclude that, the best month to travel is on April except for 3,4,11,12,16,19,20,23,29
mdata9spring <- assign %>% group_by(year,month,day)%>% filter((month>=3)&(month<=5))
#Humidity
ggplot()+
  geom_point(mdata9spring, mapping = aes(x = factor(day), y = humid, color = "RH"), group = 1, alpha = 0.2)+
  geom_hline(mdata9spring, mapping = aes(yintercept = 40, color ="Optimal Range"))+
  geom_hline(mdata9spring, mapping = aes(yintercept = 60, color ="Optimal Range"))+
  scale_color_manual(breaks = c("RH", "Optimal Range"), values = c("darkorange", "firebrick"))+
  labs(title = 'Point Chart of Humidity of Spring in 2013', x = 'Days', y = 'Humidity')+
  facet_wrap(~month)

#Visibility
ggplot()+
  geom_point(mdata9spring, mapping = aes(x = factor(day), y = visib, color = "Visibility"), group = 1)+
  geom_hline(mdata9spring, mapping = aes(yintercept = 3, color ="Min Visibility"))+
  geom_hline(mdata9spring, mapping = aes(yintercept = 10, color ="Optimal Visibility"))+
  scale_color_manual(breaks = c("Visibility", "Min Visibility", "Optimal Visibility"), values = c("darkviolet", "firebrick", "lightcoral"))+
  labs(title = 'Point Chart of Visibility in Spring 2013', x = 'Days', y = 'Visibility (miles)')+
  facet_wrap(~month)

#Analysis 11
#Wind Speed and Pressure
#Air pressure that is converted into air movement is called wind. The pressure will increase when the wind speed decrease.
#A moving air mass produced kinetic energy will be converted into a static atmospheric pressure along with the decreasing of air mass.
#Hence, the affected pressure if exceed the maximum and minimum pressure, will delay or cancel the flight.
#The graph below shows the wind speed and pressures in 2013
#We can conclude that, in both JFK and LGA the average Pressure is close to standard pressure which is 1013.25 millibars
#However, there is some time when the pressure exceed 1020 millibars
ggplot(assign, mapping = aes(x = wind_speed, y = pressure, color = origin, na.rm = TRUE))+
  geom_point(alpha = 0.2)+
  stat_smooth(method="lm")+
  labs(title = 'Scatter Plot Temperature affect Sea Level Pressure', x = 'Wind Speed (mph)', y = 'Pressure (milibars)', color ="")+
  xlim(0,45)+
  ylim(990,1040)+
  facet_wrap(~origin)

#Analysis 12
#Pressure and Wind Gusts
#On hot day, wind gusts can be generated by rising air currents when the earth is heated.
#This thing can cause a thermal warm air which will result the air is sinking (decreasing pressure) from above to replace the rising thermal.
#This descending air which caused by decreasing pressure can cause wind gusts
#The graph below shows the pressure and wind gusts in 2013

mdata12 <- assign%>% filter(!is.na(wind_gust))
ggplot()+
  geom_tile(mdata12,mapping = aes(x=temp,y=pressure,fill=wind_gust),width=2,height=2)+
  scale_fill_gradient(low="yellow", high="blue")+
  labs(title = 'Tile chart of Temperature, Pressure and Wind Gust', x = 'Temperature (F))', y = 'Pressure (millibars)', 
       fill ="Wind Gust (mph)")

#Analysis 13
#Best time to travel in 2013 based on temperature
#Based on weatherspark.com data, the best time to travel with a good weather which is not too hot or wet is with temperature around 65F to 80F
#Hence, the graph below shows the temperature in 2013
#We can see that the best time to travel is around May to June and August to October
#July is not recommended because it almost reached 100F which is too high.
ggplot()+
  geom_point(assign, mapping = aes(x = factor(month), y = temp, color = "Temperature"), alpha = 0.2)+
  geom_rect(assign, mapping = aes(xmin = min(month), xmax = max(month), ymin = 65, ymax =80), color = 'darkorange',fill = 'transparent')+
  scale_color_manual(breaks = c("Temperature","Freezing"), values = c("blue4","darkorange"))+
  labs(title = 'Point Chart of Temperature in 2013 in 2013', x = 'Month', y = 'Temperature in (F)', color ="")


#Analysis 14
#Best time to visit beaches in summer
#Based on weatherspark.com, visiting beaches is best with hot temperature 
#The temperature range is 75F to 90F
#Hence, the graph is showing the temperature from  June to August.
#The best month is august because the temperature in almost everyday is inside the temperature range.
#However, at 16 and 25 is not recommended because the temperature is much lower than the other day
mdata14summer <- assign %>% group_by(year,month,day)%>% filter((month>=6)&(month<=8))
ggplot()+
  geom_point(mdata14summer, mapping = aes(x = factor(day), y = temp, color = "Temperature"), alpha = 0.2)+
  geom_rect(mdata14summer, mapping = aes(xmin = min(day), xmax = max(day), ymin = 75, ymax = 90),
            color = 'darkorange',fill = 'transparent')+
  scale_color_manual(breaks = c("Temperature","Freezing"), values = c("blue4","darkorange"))+
  labs(title = 'Point Chart of Temperature in Summer 2013', x = 'Month', y = 'Temperature in (F)', color ="")+
  facet_wrap(~month)

#Analysis 15
#Temperature x Seasonal
#According timeanddate.com, there are 4 meteorogical season which is spring, summer, autumn, winter

#Spring
#Stated in climatestotravel.com, Spring temperature is actually unstable
#In March, it snow is still happening in early march because it is transition from winter
#In April, the condition is quite stable 
#In May, the temperature is increasing and since it is the time to do transition to summer
#It said to be a possible in the first afternoon will occur thunderstorm
mdata15spring <- assign %>% group_by(year,month,day)%>% filter((month>=3)&(month<=5))
ggplot()+
  geom_point(mdata15spring, mapping = aes(x = factor(day), y = temp), color = "red", group = 1, alpha = 0.2)+
  geom_hline(mdata15spring, mapping = aes(yintercept = 36, color = "Average Temperature"))+
  geom_hline(mdata15spring, mapping = aes(yintercept = 72, color = "Average Temperature"))+
  scale_color_manual(breaks = c("Average Temperature"), values = c("blue"))+
  labs(title = 'Point Chart of Spring in 2013', x = 'Days', y = 'Temperature (F)', color = "")+
  ylim(25,100)+
  facet_wrap(~month)

#Summer
#The chart below shows the temperature in summer
#according to climatetotravel, July and August is the hottest month
#The peak is in August which reached almost 100F
#Also in sunny day, thunderstorm can occurs in the afternoon.Heat wave is also possible sometimes.
mdata15summer <- assign %>% group_by(year,month,day)%>% filter((month>=6)&(month<=8))
ggplot()+
  geom_point(mdata15summer, mapping = aes(x = factor(day), y = temp), color = "red", group = 1, alpha = 0.2)+
  geom_hline(mdata15summer, mapping = aes(yintercept = 64, color = "Average Temperature"))+
  geom_hline(mdata15summer, mapping = aes(yintercept = 84, color = "Average Temperature"))+
  scale_color_manual(breaks = c("Average Temperature"), values = c("blue"))+
  labs(title = 'Point Chart of Summerin 2013', x = 'Days', y = 'Temperature (mph)', color = "")+
  ylim(50,100)+
  facet_wrap(~month)

#Autumn
#The chart below shows the temperature in Autumn
#According to climatetotravel, in early September, the weather is still characterized by heat and humid
#Afternoon sunny day, Thundersorm is possibl. This only occur until first half October
#After that, the situation is become quite and mild on the second half of October.
#In November, it is already a cold month and Snowfall may occur.
#Less frequency rainfall is also occur in this season.
mdata15autumn <- assign %>% group_by(year,month,day)%>% filter((month>=9)&(month<=11))
ggplot()+
  geom_point(mdata15autumn, mapping = aes(x = factor(day), y = temp), color = "red", group = 1, alpha = 0.2)+
  geom_hline(mdata15autumn, mapping = aes(yintercept = 41, color = "Average Temperature"))+
  geom_hline(mdata15autumn, mapping = aes(yintercept = 75, color = "Average Temperature"))+
  scale_color_manual(breaks = c("Average Temperature"), values = c("blue"))+
  labs(title = 'Point Chart of Autumn in 2013', x = 'Days', y = 'Temperature (mph)', color = "")+
  ylim(25,100)+
  facet_wrap(~month)

#Winter
#From December to February is all cold.
#The average temperature is all freezing (32F)
#The wind is also cold.
mdata15winter <- assign %>% group_by(year,month,day)%>% filter((month==12)|(month<=2))
ggplot()+
  geom_point(mdata15winter, mapping = aes(x = factor(day), y = temp), color = "red", group = 1, alpha = 0.2)+
  geom_hline(mdata15winter, mapping = aes(yintercept = 27, color = "Average Temperature"))+
  geom_hline(mdata15winter, mapping = aes(yintercept = 43, color = "Average Temperature"))+
  scale_color_manual(breaks = c("Average Temperature"), values = c("blue"))+
  labs(title = 'Point Chart of Winter in 2013', x = 'Days', y = 'Temperature (mph)', color = "")+
  ylim(25,70)+
  facet_wrap(~month)

#Extra Feature 1
#Temperature and Dew Point affect Humidity
#In this analysis, a relation between Humidity 
#Analyzing 3 Variables
ggplot()+
  geom_tile(assign,mapping = aes(x=temp,y=dewp,fill=humid),width=2,height=2)+
  scale_fill_gradient(low="yellow", high="orange")+
  ylim(-10,100)+
  geom_abline(color = "red", size = 1)+
  labs(title = 'Tile chart of Temperature & Dew Point affect Humidity', x = 'Temperature (F))', y = 'Dew Point(F)', fill ="Humidity")

#Extra Feature 2
#Wind Direction of JFK and LGA Airport
#Comparing the number of flights in each direction between JFK and LGA airport
ggplot() +
  geom_bar(data = assign, mapping = aes(x = wind_dir, fill = origin)) +
  #coord_polar used to make the geom_bar to a polarized shape like circle
  coord_polar()+
  labs(title = 'Polar Bar Charrt of Wind Direction  in 2013',x = 'Wind Direction', fill = "")
