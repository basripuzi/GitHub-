library(dplyr)
library(ggplot2)
library(readr)
library(vcd)
library(car)
library (FSA)
library (multcomp)

#This is the script to obtain result for AAAP abstract.

#statistical summary
#to read data from csv file saved in the folder
data <-read_csv("aaap1.csv")

#to observe data
glimpse(data)
head(data)
tail(data)

#to omit missing values (NA)
lact <- na.omit(data)
glimpse (lact)

#Set categorical variables as factor or numeric
lact$id <- as.factor (lact$id)
lact$origin <- as.factor (lact$origin)
lact$mob <- as.factor (lact$mob)
lact$sire <- as.factor (lact$sire)
lact$dam <- as.factor (lact$dam)
lact$pcv<-as.numeric(lact$pcv)
lact$eo <-as.numeric(lact$eo)
lact$pcr_am <- as.factor (lact$pcr_am)
lact$pcr_bbi <- as.factor (lact$pcr_bbi)
lact$pcr_bbo <- as.factor (lact$pcr_bbo)
lact$pcr_the <- as.factor (lact$pcr_the)
#lact$pcr_try <- as.factor (lact$pcr_try)
lact$bw <-as.numeric(lact$bw)
lact$calvingrate <- as.factor (lact$calvingrate)
lact$lactmnth <- as.factor (lact$lactmnth)
lact$milkyield <-as.numeric(lact$milkyield)
lact$infection <- as.factor (lact$infection)
glimpse (lact)

summary (lact)

#leveling for negative and positive animals
#levels(lact$pcr_am)
#lact$pcr_am <- relevel(lact$pcr_am, ref = "Positive")

#levels(lact$pcr_bbi)
#lact$pcr_bbi <- relevel(lact$pcr_bbi, ref = "Positive")

#levels(lact$pcr_bbo)
#lact$pcr_bbo <- relevel(lact$pcr_bbo, ref = "Positive")

#levels(lact$pcr_the)
#lact$pcr_the <- relevel(lact$pcr_the, ref = "Positive")

#levels(lact$pcr_try)
#lact$pcr_try <- relevel(lact$pcr_try, ref = "Positive")

glimpse (lact)

####################################################################################
#Milkyield of cattle infected with different haemoparasites

##bar chart
ggplot(lact, aes(x = infection, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Groups of infected cattle")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=infection, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       y="Milk yield (kg)", x = "Groups of infected cattle")+
  theme_bw()

##boxplot
milk_plot <- ggplot(lact, aes(x=infection, y=milkyield,
                              fill = infection, group = infection))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       x="Haemoparasite infection", y = "Milk yield (kg)")+
  theme_bw()
milk_plot
  

# ##boxplot coord_flip
# milk_plot <- ggplot(lact, aes(x=milkyield, y=infection,
#                  fill = infection, group = infection))+ 
#   geom_boxplot()+
#   labs(title="Milk yield of cattle infected with different haemoparasite species",
#        x="Milk yield (kg)", y = "Haemoparasite infection")+
#   theme_bw()+
#   coord_flip()
# 
# milk_plot

#Change the legend labels
milk_plot <- milk_plot + scale_fill_discrete(name = "Species",
                        labels = c("A.marginale, B.bigemina, B.bovis", 
                                   "A.marginale, B.bigemina, T.orientalis", 
                                   "A.marginale, B.bigemina", 
                                   "B.bigemina, B.bovis", 
                                   "B.bigemina",
                                   "A.marginale, B.bigemina, B.bovis, T.orientalis"))
milk_plot

#Change the position of the legend
milk_plot <- milk_plot + theme(legend.position="bottom")

milk_plot

# ##boxplot coord_flip
# ggplot(lact, aes(x=milkyield, y=infection))+ 
#   geom_point(shape 18)+
#   labs(title="Milk yield of cattle infected with different haemoparasite species",
#          x="Milk yield (kg)", y = "Groups of infected cattle")+
#   theme_bw()+
#   coord_flip()
#   
# mean_milk <- lact %>%
#   group_by (infection) %>%
#   summarise (mean_milk =mean (milkyield))
# 
# mean_milk
# 
# ggplot(mean_milk, aes(x=mean_milk, y=infection))+ 
#   geom_point(shape 18)+
#   labs(title="Milk yield of cattle infected with different haemoparasite species",
#        x="Milk yield (kg)", y = "Groups of infected cattle")+
#   theme_bw()
# #coord_flip()

# A = Anaplasma, B.bigemina, B.bovis
# B = Anaplasma, B.bigemina, Theileria orientalis
# C/E = Anaplasma, B.bigemina
# D/G = B.bigemina, B.bovis
# E/L = B.bigemina
# F/O = naplasma, B.bigemina, B.bovis, Theileria orientalis

################################################################
#Birth weight of cattle infected with different haemoparasites
### need to replace 0 with NA in the csv file. Save in different csv file

# ##bar chart
# ggplot(lact, aes(x = infection, y = calvingrate))+
#   geom_bar(stat ='identity', position = 'dodge')+
#   xlab ("Groups of infected cattle")+
#   ylab ("Milk yield (kg)")
# 
# ##boxplot
# ggplot(lact, aes(x=infection, y=calvingrate))+ 
#   geom_boxplot()+
#   labs(title="Milk yield of cattle infected with different haemoparasite species",
#        y="Milk yield (kg)", x = "Groups of infected cattle")+
#   theme_bw()

# A = Anaplasma, B.bigemina, B.bovis
# B = Anaplasma, B.bigemina, Theileria orientalis
# C/E = Anaplasma, B.bigemina
# D/G = B.bigemina, B.bovis
# E/L = B.bigemina
# F/O = naplasma, B.bigemina, B.bovis, Theileria orientalis
##########################################################################
#Anaplasma detection and milkyield of infected and non-infected cattle

##bar chart
ggplot(lact, aes(x = pcr_am, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Anaplasma marginale infection")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=pcr_am, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of infected and non-infected cattle of Anaplasma marginale",
       y="Milk yield (kg)", x = "Status of infection")+
  theme_bw()

################################################################
##Babesia bigemina detection and milkyield of infected and non-infected cattle

##bar chart
ggplot(lact, aes(x = pcr_bbi, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Babesia bigemina infection")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=pcr_bbi, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of infected and non-infected cattle of Babesia bigemina",
       y="Milk yield (kg)", x = "Status of infection")+
  theme_bw()

########################################################################################
##Babesia bovis detection and milkyield of infected and non-infected cattle

##bar chart
ggplot(lact, aes(x = pcr_bbo, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Babesia bovis infection")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=pcr_bbo, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of infected and non-infected cattle of Babesia bovis",
       y="Milk yield (kg)", x = "Status of infection")+
  theme_bw()
################################################################
##Theileria detection and milkyield of infected and non-infected cattle

##bar chart
ggplot(lact, aes(x = pcr_the, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Theileria orientalis infection")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=pcr_the, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of infected and non-infected cattle of Theileria orientalis",
       y="Milk yield (kg)", x = "Status of infection")+
  theme_bw()
#################################################################
##Trypanosoma detection and milkyield of infected and non-infected cattle

##bar chart
ggplot(lact, aes(x = pcr_try, y = milkyield))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Trypanosoma evansi infection")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=pcr_try, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of infected and non-infected cattle of Trypanosoma evansi",
       y="Milk yield (kg)", x = "Status of infection")+
  theme_bw()

############################################################################

glimpse (lact)

#One-way ANOVA -> Milk yield & infection
milk <- lm (milkyield ~ infection, data = lact)
anova(milk)
summary (milk)

milk_aov <- aov(milkyield ~ infection,data = lact)

#summary(milk_aov)

# # Tukey HSD test:
# post_test <- glht(milk_aov,
#                   linfct = mcp(infection = "Tukey"))
# 
# 
# summary(post_test)

# Dunnett's test:
post_test <- glht(milk_aov,
                  linfct = mcp(infection = "Dunnett"))


summary(post_test)


#We want to know if there is any significant difference between the average milkyield of each group
kruskal.test(milkyield ~ infection, data = lact)

#we know that there is a significant difference between groups, 
#but we don't know which pairs of groups are different.
#It's possible to use the function pairwise.wilcox.test() 
#to calculate pairwise comparisons between group levels with corrections for multiple testing.

#A Kruskal-Wallis test is used to determine whether or not there is a statistically significant difference between the medians of three or more independent groups. 
#It is considered to be the non-parametric equivalent of the One-Way ANOVA.
#If the results of a Kruskal-Wallis test are statistically significant, 
#then it's appropriate to conduct Dunn's Test to determine exactly which groups are different.
#Note that we chose to use a Bonferroni correction for the p-values of the multiple comparisons
#At ?? = .05, drugs A and C are the only two drugs that are statistically significantly different
# from each other (adjusted p-value = .003768).
dunnTest(milkyield ~ infection,
        data=lact,
        method="bonferroni")
#there were no significant difference betweem milkyield of each groups. 
#https://www.statology.org/dunns-test-in-r/#:~:text=A%20Kruskal%2DWallis%20test%20is,of%20the%20One%2DWay%20ANOVA.

#t-test between group E & F
t.test (milkyield ~ infection, data = lact)

#Check the assumptions of normality
qqnorm(lact$milkyield, pch = 1, frame = FALSE)
qqline(lact$milkyield, col = "steelblue", lwd = 2)
qqPlot(lact$milkyield)
#QQ plot (or quantile-quantile plot) draws the correlation between 
#a given sample and the normal distribution. 
#A 45-degree reference line is also plotted. 
#QQ plots are used to visually check the normality of the data.
#As all the points fall approximately along this reference line, we can assume normality.

##boxplot
ggplot(lact, aes(x=infection, y=milkyield))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       y="Milk yield (kg)", x = "Groups of infected cattle")+
  theme_bw()

##boxplot
milk_plot <- ggplot(lact, aes(x=infection, y=milkyield,
                              fill = infection, group = infection))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       x="Haemoparasite infection", y = "Milk yield (kg)")+
  theme_bw()

#Change the legend labels
milk_plot <- milk_plot + scale_fill_discrete(name = "Species",
                                             labels = c("A.marginale, B.bigemina, B.bovis", 
                                                        "A.marginale, B.bigemina, T.orientalis", 
                                                        "A.marginale, B.bigemina", 
                                                        "B.bigemina, B.bovis", 
                                                        "B.bigemina",
                                                        "A.marginale, B.bigemina, B.bovis, T.orientalis"))

#Change the position of the legend
milk_plot <- milk_plot + theme(legend.position="bottom")

milk_plot
##########################################################################################
##boxplot for milk yield, infection & cattle origin
milk_origin <- ggplot(lact, aes(x=origin, y=milkyield, fill = infection))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species from Johor and Sabah",
       y="Milk yield (kg)", x = "Origin of cattle")+
  theme_bw()

#Change the legend labels
milk_origin <- milk_origin + scale_fill_discrete(name = "Haemoparasite species",
                                             labels = c("A.marginale, B.bigemina, B.bovis", 
                                                        "A.marginale, B.bigemina, T.orientalis", 
                                                        "A.marginale, B.bigemina", 
                                                        "B.bigemina, B.bovis", 
                                                        "B.bigemina",
                                                        "A.marginale, B.bigemina, B.bovis, T.orientalis"))

#Change the position of the legend
milk_origin <- milk_origin + theme(legend.position="bottom")
milk_origin

#TWO-way ANOVA -> Milk yield ~ infection, origin
milk_origin <- lm (milkyield ~ infection + origin, data = lact)
anova(milk_origin)
summary (milk_origin)

milk_origin <- aov(milkyield ~ infection + origin, data = lact)
summary (milk_origin)

#Post-hoc test (multiple pairwise comparison)
summary(glht(milk_origin, linfct = mcp(infection = "Tukey")))
summary(glht(milk_origin, linfct = mcp(infection = "Dunnett")))
summary(glht(milk_origin, linfct = mcp(origin = "Tukey")))
summary(glht(milk_origin, linfct = mcp(origin = "Dunnett")))
#summary(glht(milk_origin, linfct = mcp(lactmnth = "Tukey")))
#http://www.bryanburnham.net/wp-content/uploads/2014/01/Reporting-Statistics-in-APA-Format.pdf

library (AICcmodavg)

#multiple linear regression model
milk1 <- lm (milkyield ~ infection * origin * lactmnth, data = lact)
anova(milk1)
summary (milk1)

milk2 <- lm (milkyield ~ infection * origin + lactmnth, data = lact)
anova(milk2)
summary (milk2)

milk3 <- lm (milkyield ~ infection + origin + lactmnth, data = lact)
anova(milk3)
summary (milk3)
shapiro.test(milk3$residuals)

# ###AIC calculation : https://www.statology.org/aic-in-r/
# #define list of models
# models <- list(milk1, milk2, milk3)
# 
# #specify model names
# mod.names <- c('Model1', 'Model2', 'Model3')
# 
# #calculate AIC of each model
# aictab(cand.set = models, modnames = mod.names)
# 
# #best model R-squared
# milk3 <- lm (milkyield ~ infection + origin + lactmnth, data = lact)
# anova(milk3)
# summary (milk3)
# shapiro.test(milk3$residuals)
# 
# summary(glht(milk3, linfct = mcp(infection = "Tukey")))
# summary(glht(milk3, linfct = mcp(origin = "Tukey")))
# summary(glht(milk3, linfct = mcp(lactmnth = "Dunnett")))

#http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# group_by(lact, infection, origin) %>%
#   summarise(
#     count = n(),
#     mean = mean(milkyield, na.rm = TRUE),
#     sd = sd(milkyield, na.rm = TRUE))
# 
# TukeyHSD(res.aov3, which = "dose")
# 
# 
# 
# milk_origin <-aov(milkyield ~ infection + origin, data = lact )
# TukeyHSD(milk_origin, which = "origin")
# TukeyHSD(milk_origin, which = "infection")
# 
# 
# 
# #Tukey multiple comparisons of means
# #95% family-wise confidence level
# Fit: aov(formula = StressReduction ~ Treatment + Age, data = data)
# 
# 
# 
# 
# #Calculate mean & sd of milkyield for all combinations of infections and origin
# milk_origin <- lact %>%
#   group_by (infection, origin) %>%
#   summarise (meanMilk = mean(milkyield), 
#              seMilk =sd(milkyield)/sqrt(n()))
# glimpse(milk_origin)
# 
# 
# 
# 
# ggplot (milk_origin, aes (x = infection, y = meanMilk,
#                           colour = origin, group = origin))+
#   geom_point()+
#   geom_errorbar(aes(ymin = meanMilk - seMilk,
#                     ymax = meanMilk + seMilk), width = 0.1)+
#   theme_bw()
# 
# 
# 
# 
# 
# milk_origin_aov <- aov(milkyield ~ infection * origin,data = lact)
# 
# # Dunnett's test:
post_test <- glht(milk_origin_aov,
                   linfct = mcp(infection = "Dunnett"))
# 
# 
# summary(post_test)
# 
# glimpse(lact)

#######################################################################
glimpse(lact)

ggplot(lact, aes(x = lactmnth, y = milkyield,
                 fill = origin))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Theileria orientalis infection")+
  ylab ("Milk yield (kg)")

##boxplot for milk yield 
ggplot(lact, aes(x=infection, y=milkyield, fill = origin))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       y="Milk yield (kg)", x = "Groups of infected cattle")+
  theme_bw()

##boxplot
milk_plot <- ggplot(lact, aes(x=lactmnth, y=milkyield,
                              fill = lactmnth, group = lactmnth))+ 
  geom_boxplot()+
  labs(title="Milk yield of cattle infected with different haemoparasite species",
       x="Haemoparasite infection", y = "Milk yield (kg)")+
  theme_bw()

milk_plot


#Change the legend labels
milk_plot <- milk_plot + scale_fill_discrete(name = "Species",
                                             labels = c("A.marginale, B.bigemina, B.bovis", 
                                                        "A.marginale, B.bigemina, T.orientalis", 
                                                        "A.marginale, B.bigemina", 
                                                        "B.bigemina, B.bovis", 
                                                        "B.bigemina",
                                                        "A.marginale, B.bigemina, B.bovis, T.orientalis"))

#Change the position of the legend
milk_plot <- milk_plot + theme(legend.position="bottom")

milk_plot

#One-way ANOVA -> Milk yield & lactation month
milk_lactm<- lm (milkyield ~ lactmnth, data = lact)
anova(milk_lactm)
summary (milk_lactm)

milk_lactm_aov <- aov(milkyield ~ lactmnth,data = lact)

summary(milk_lactm_aov)


# Dunnett's test:
post_test <- glht(milk_lactm_aov,
                  linfct = mcp(lactmnth = "Dunnett"))


summary(post_test)

# # Tukey HSD test:
# post_test <- glht(milk_aov,
#                   linfct = mcp(infection = "Tukey"))
# 
# 
# summary(post_test)

# Dunnett's test:
post_test <- glht(milk_aov,
                  linfct = mcp(infection = "Dunnett"))


summary(post_test)

#######################################################################
#One-way ANOVA -> PCV
pcv <- lm (pcv ~ infection, data = lact)
anova(pcv)
summary (pcv)

#Check the assumptions of normality
qqnorm(lact$pcv, pch = 1, frame = FALSE)
qqline(lact$pcv, col = "steelblue", lwd = 2)
qqPlot(lact$pcv)

##bar chart
ggplot(lact, aes(x = infection, y = pcv))+
  geom_bar(stat ='identity', position = 'dodge')+
  xlab ("Groups of infected cattle")+
  ylab ("Milk yield (kg)")

##boxplot
ggplot(lact, aes(x=infection, y=pcv))+ 
  geom_boxplot()+
  geom_point(size=3, colour = 'lightgrey')+
  labs(title="Packed cell volume of cattle infected with different haemoparasite species",
       y="Packed cell volume (%)", x = "Haemoparasite infection")+
  theme_bw()

##boxplot
pcv_plot <- ggplot(lact, aes(x=infection, y=pcv,
                              fill = infection, group = infection))+ 
  geom_boxplot()+
  geom_point(size=1, colour = 'black')+
  labs(title="Packed cell volume of cattle infected with different haemoparasite species",
       x="Packed cell volume (%)", y = "Haemoparasite infection")+
  theme_bw()

pcv_plot

#Change the legend labels
pcv_plot <-pcv_plot + scale_fill_discrete(name = "Species",
                                             labels = c("A.marginale, B.bigemina, B.bovis", 
                                                        "A.marginale, B.bigemina, T.orientalis", 
                                                        "A.marginale, B.bigemina", 
                                                        "B.bigemina, B.bovis", 
                                                        "B.bigemina",
                                                        "A.marginale, B.bigemina, B.bovis, T.orientalis"))

#Change the position of the legend
pcv_plot <- pcv_plot + theme(legend.position="bottom")

pcv_plot

#One way ANOVA -> Eosinophils
eo <- lm (eo ~ infection, data = lact)
anova(eo)
summary (eo)

#Check the assumptions of normality
qqnorm(lact$eo, pch = 1, frame = FALSE)
qqline(lact$eo, col = "steelblue", lwd = 2)
qqPlot(lact$eo)

shapiro.test(lact$eo)


milk <- glm (milkyield ~ infection*pcv*eo, family = poisson, data = lact)
anova(milk)
summary (milk)

milk_ef<- wilcox.test(milkyield ~ infection,
                   data = lact,
                   exact = FALSE)
milk_ef

#TWO-WAY ANOVA

#1
milk <- lm(milkyield ~ infection*pcv + infection*eo + pcv*eo, data = lact)
anova(milk)
summary (milk)

#2 (the lowest p-value, the most significant)
milk <- lm(milkyield ~ infection*pcv + infection*eo + pcv + eo, data = lact)
anova(milk)
summary (milk)

#3
milk <- lm(milkyield ~ infection*eo + pcv + eo, data = lact)
anova(milk)
summary (milk)

#4
milk <- lm(milkyield ~ infection*eo, data = lact)
anova(milk)
summary (milk)

#5
milk <- lm(milkyield ~ infection:eo + eo, data = lact)
anova(milk)
summary (milk)

#6
milk <- lm(milkyield ~ infection + eo, data = lact)
anova(milk)
summary (milk)

#7
milk <- lm(milkyield ~ infection, data = lact)
anova(milk)
summary (milk)
summary (milk)$coef

###############################################################################
bw <- lm(bw ~ infection, data = lact)
anova(bw)
summary (bw)

#FEC
milk <- glm(milkyield ~ pcr_am + pcr_bbo + pcr_the, family = poisson, data = lact)
anova(milk)
summary (milk)


#PCV
pcv <- lm(milkyield ~ infection*pcv + infection*eo + eo, data = lact)
anova(pcv)
summary (pcv)

#EOSINOPHIL 
eo <- lm(eo ~ Treatment*group, data = data2)
anova(eo)
summary (eo)


#autoplot(fec, smooth.colour =NA)

# box & whiskers plot
# ggplot(fyp, aes(x = group, y = fec.post))+
#   geom_bar(stat ='identity', position = 'dodge')
# 
# #Post-tx FEC vs PCV
# #ggplot (fyp,aes (x=fec.post,y=pcv.post, colour = group))+
# #  geom_point()
# 
# ggplot (fyp,aes (x=eo.pre,y=fec.pre))+
#   geom_point()
# 
# ggplot (fyp,aes (x=eo.post,y=fec.post))+
#   geom_point()
# 
# ggplot (fyp,aes (x=fec.pre,y=pcv.pre))+
#   geom_point()
# 
# ggplot (fyp,aes (x=fec.post,y=pcv.post))+
#   geom_point()
# 
# 
# ggplot (fyp,aes (x=eo.pre,y=pcv.pre))+
#   geom_point()
# 
# ggplot (fyp,aes (x=eo.post,y=pcv.post))+
#   geom_point() 
