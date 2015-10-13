PM = read.csv("PM.csv")
day1 = read.csv("day1.csv")
#no recording time and date!
wind = read.csv("WeatherData.csv")

source("libraries.R")

total = merge(PM, wind)
#wind data: 4/9-4/13

plot(day1$DateNum, day1$Difference_in_Small)
plot(day1$DateNum, day1$SW_Small)


windback = subset(wind, wind$Winddirection == "180")
windfor = subset(wind, wind$Winddirection > 300)

#half of time wind is backwards 
#7am - 7pm wind 180
#7pm - 7am wind 315
#back means away from the industrial PM source

back = total[total$Winddirection == "180",]
forw = total[total$Winddirection > 300, ]
plot(forw$DateNum, forw$SW_Small)
plot(back$DateNum, back$SW_Small)

total$retentiontime = 200 / (total$WindCurrent)

x = total[total$retentiontime != Inf,]
y = total[total$retentiontime == Inf,]
y$retentiontime = 200

total = rbind(x,y)

summary(lm(total$Difference_in_Small~ total$retentiontime))

plot(PM$Difference_in_Small)
summary(PM$Difference_in_Small)
plot(total$Difference_in_Small)

