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
#7am - 7pm wind 180 backward
#7pm - 7am wind 315 forward --- should get a large difference
#back means away from the industrial PM source

back = total[total$Winddirection == "180",]
forw = total[total$Winddirection > 300, ]
plot(forw$DateNum, forw$SW_Small)
plot(back$DateNum, back$SW_Small)
plot(back$Difference_in_Small)
summary(back$Difference_in_Small)
summary(forw$Difference_in_Small)
#this is significant!

summary(lm(total$Difference_in_Small ~ total$Winddirection))

#not enough data
#just assume the 7-7 period wind direction is constant for all?

pmfor1 = PM[7:17,]

pmfor1$coverage = 0
summary(pmfor1$Difference_in_Small)
pmfor2 = PM[39:51,]
pmfor2$coverage = 0
summary(pmfor2$Difference_in_Small)
pmfor3 = PM[65:80,]
pmfor3$coverage = 3000
pmfor = rbind(pmfor1,pmfor2,pmfor3)
summary(pmfor3$Difference_in_Small)
pmfor$winddirection = 315

plot(pmfor$coverage, pmfor$Difference_in_Small)
summary(lm(pmfor$Difference_in_Small ~ pmfor$coverage))

pmfor$industryPM = "with industrial PM input"
summary(pmfor$Difference_in_Small)
pmback1 = PM[1:6,]
pmback2 = PM[18:38,]
pmback3 = PM[52:64,]
pmback_control = rbind(pmback1,pmback2,pmback3)
pmback_control$coverage = 0
pmback4 = PM[81:93,]
pmback4$coverage = 3000
pmback = rbind(pmback_control,pmback4)
pmback$industryPM = "without industrial PM input"
pmback$Difference_in_Small = -1 * pmback$Difference_in_Small

plot(pmback$coverage, pmback$Difference_in_Small)
boxplot(pmback$coverage, pmback$Difference_in_Small)
summary(lm(pmback$Difference_in_Small ~ pmback$coverage))



summary(pmback$Difference_in_Small)
pmback$winddirection = 180

pm2 = rbind(pmfor, pmback)

summary(lm(pm2$Difference_in_Small ~ pm2$coverage))

boxplot(pm2$winddirection,pm2$Difference_in_Small, main = "Difference in size 1-5 um PM after passing the farm", ylab = "#PM difference from upwind to downwind")
summary(lm(pm2$Difference_in_Small~pm2$NE_Small))
plot(pm2$Difference_in_Small~pm2$NE_Small)

total$retentiontime = 200 / (total$WindCurrent)
x = total[total$retentiontime != Inf,]
y = total[total$retentiontime == Inf,]
y$retentiontime = 200
total = rbind(x,y)

summary(lm(total$Difference_in_Small~ total$retentiontime))

plot(PM$Difference_in_Small)
summary(PM$Difference_in_Small)
plot(total$Difference_in_Small)


summary(lm(pm2$Difference_in_Small ~ pm2$coverage * pm2$winddirection))
        
lmermodel1 <-lmer(pm2$Difference_in_Small ~ pm2$coverage * (1|pm2$winddirection), data=pm2)
anova(lmermodel1,test="Chisq")
summary(lmermodel1)

