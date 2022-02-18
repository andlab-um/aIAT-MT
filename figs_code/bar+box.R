#install.packages('yarrr')
#install.packages('dplyr')
setwd("~/Desktop/projects/IAT/IATcode-Heliyon/statistics_revision/figs")
library(openxlsx)
library(ggplot2)
library(yarrr)
library(dplyr)
source('ggplot_setup.R')

data<-read.xlsx('iat1.xlsx')
data<-read.xlsx('iat2.xlsx')
data<-read.xlsx('merge.xlsx')
data1<-data[which(data$subj_idx != 106),]
data1<-data[which(data$subj_idx != 152),]
data1<-data1[which(data1$rt < 5000),]
data1<-data1[which(data1$error == 0),]
data1<-data1[which((data1$block == 4)|(data1$block == 7)),]
#excel文件已经做了block4和7的筛选


rt_means <- data1 %>% group_by(condition1, subj_idx) %>% 
  summarise(rt_mean=mean(rt)) %>%
  arrange(condition1, subj_idx) %>%
  ungroup()



MAD_means <- data1 %>% group_by(condition1,subj_idx) %>% 
  summarise(MAD_mean=mean(MAD)) %>%
  arrange(condition1,subj_idx) %>%
  ungroup() 
  


AD_means <- data1 %>% group_by(condition1,subj_idx) %>% 
  summarise(AD_mean=mean(AD)) %>%
  arrange(condition1,subj_idx) %>%
  ungroup()



AUC_means <- data1 %>% group_by(condition1,subj_idx) %>% 
  summarise(AUC_mean=mean(AUC)) %>%
  arrange(condition1,subj_idx) %>%
  ungroup()
# get unique variable code for each evidence combination
ev_codes <- rt_means %>% select(condition1) %>% 
  distinct(condition1) %>%
  mutate(code=as.factor(c(1,3,2,4)))
         
ev_codes <- MAD_means %>% select(condition1) %>% 
  distinct(condition1) %>%
  mutate(code=as.factor(c(1,3,2,4)))

ev_codes <- AD_means %>% select(condition1) %>% 
  distinct(condition1) %>%
  mutate(code=as.factor(c(1,3,2,4)))

# add code column to scenario means
rt <- rt_means %>% merge(ev_codes) %>% arrange(condition1, subj_idx)
MAD <- MAD_means %>% merge(ev_codes) %>% arrange(condition1, subj_idx)
AD <- AD_means %>% merge(ev_codes) %>% arrange(condition1, subj_idx)
AUC <- AUC_means %>% merge(ev_codes) %>% arrange(condition1)
############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects

pirateplot(rt_mean~code,
           data = rt,
           xlab = "",
           ylab = "",
           inf.f.col = c("dark blue", "light blue"), 
           inf.b.col = "black",
           gl.col = 0, # Add light gray background
           cex.axis = 1.2,
           cex.lab =  0.0001,
           cex.names = 0.0001,
           point.cex = 1.2,
           point.o = .4,
           bar.f.o = .5, # Bar
           bar.f.col = c(gray(.4),gray(.6),gray(.8)), # bar filling color
           bean.f.o = 0,
           bean.b.o = 0,
           ylim=c(600,2600))

pirateplot(MAD_mean~code,
           data = MAD,
           xlab = "",
           ylab = "",
           inf.f.col = c("dark green", "light green"), 
           inf.b.col = "black",
           gl.col = 0, # Add light gray background
           cex.axis = 1.5,
           cex.lab =  0.0001,
           cex.names = 0.0001,
           point.cex = 1.2,
           point.o = .4,
           bar.f.o = .5, # Bar
           bar.f.col = c(gray(.4),gray(.6),gray(.8)), # bar filling color
           bean.f.o = 0,
           bean.b.o = 0,
           ylim=c(-0.2,1.6))
pirateplot(AD_mean~code,
           data = AD,
           xlab = "",
           ylab = "",
           inf.f.col = c("dark red", "pink"), 
           inf.b.col = "black",
           gl.col = 0, # Add light gray background
           cex.axis = 1.5,
           cex.lab =  0.0001,
           cex.names = 0.0001,
           point.cex = 1.2,
           point.o = .4,
           bar.f.o = .5, # Bar
           bar.f.col = c(gray(.4),gray(.6),gray(.8)), # bar filling color
           bean.f.o = 0,
           bean.b.o = 0,
           ylim=c(-0.1,0.6)) 

pirateplot(AUC_mean~code,
           data = AUC,
           xlab = "",
           ylab = "",
           inf.f.col = c("yellow", "light yellow"), 
           inf.b.col = "black",
           gl.col = 0, # Add light gray background
           cex.axis = 1.5,
           cex.lab =  0.0001,
           cex.names = 0.0001,
           point.cex = 1.2,
           point.o = .4,
           bar.f.o = .5, # Bar
           bar.f.col = c(gray(.4),gray(.6),gray(.8)), # bar filling color
           bean.f.o = 0,
           bean.b.o = 0) 
#Green '#336600','#339900','#66CC00','#33CC00' 
#Blue  '#003366','#003399','#336699','#0066CC'
#purple '#660066','#990099','#993366','#CC6699'
#orange  '#FF3300', '#FF6633', '#FF6600', '#FF9966'