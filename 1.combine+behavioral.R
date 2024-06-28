# Preparations
## Load libraries
packages = c("data.table", "lme4", "Matrix", "stargazer","coxme","survival",
             "bdsmatrix","ez","ggplot2","lmerTest","dplyr",'yarrr','gridExtra',
             'ggsignif','ggpubr','patchwork','readxl','reticulate','Hmisc','ggnewscale',
             'ggcorrplot2','RColorBrewer', 'igraph', 'car', 'beeswarm', 'plotrix','rockchalk',
             'plotly','hrbrthemes','readxl','mousetrap','readbulk')
newpackages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
loaded = lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
cat(paste('## ', sum(unlist(loaded)), ' out of ', length(unlist(loaded)) ,' packages loaded\n', sep = ''))

rm(list = ls())

## IAT1
# read data
setwd("/Users/orlacamus/Desktop/projects/IAT240615/IAT1_raw")

# read the data
iat4 <-  read_bulk("iat4", fun=read_mt, extension=".mt")
iat7 <- read_bulk("iat7", fun=read_mt, extension=".mt")

# preprocesing
iat4 <- mt_import_wide(iat4)
mt_4 <- mt_remap_symmetric(iat4)
mt_4 <- mt_align_start_end(mt_4)
mt_4 <- mt_time_normalize(mt_4)
mt_4 <- mt_measures(mt_4)
mt_4 <- mt_angles(mt_4,use='tn_trajectories')
mt_4 <- mt_derivatives(mt_4,use='tn_trajectories')
iat1_4=cbind(mt_4$data,mt_4$measures,mt_4$tn_trajectories)
iat1_4$group='seq'
iat1_4$run='congruent'

iat7 <- mt_import_wide(iat7)
mt_7 <- mt_remap_symmetric(iat7)
mt_7 <- mt_align_start_end(mt_7)
mt_7 <- mt_time_normalize(mt_7)
mt_7 <- mt_measures(mt_7)
mt_7 <- mt_angles(mt_7,use='tn_trajectories')
mt_7 <- mt_derivatives(mt_7,use='tn_trajectories')
iat1_7=cbind(mt_7$data,mt_7$measures,mt_7$tn_trajectories)
iat1_7$group='seq'
iat1_7$run='incongruent'


## IAT2
# read data
setwd("/Users/orlacamus/Desktop/projects/IAT240615/IAT2_raw")

# read the data
iat4 <-  read_bulk("iat7", fun=read_mt, extension=".mt")
iat7 <- read_bulk("iat4", fun=read_mt, extension=".mt")

# preprocesing
iat4 <- mt_import_wide(iat4)
mt_4 <- mt_remap_symmetric(iat4)
mt_4 <- mt_align_start_end(mt_4)
mt_4 <- mt_time_normalize(mt_4)
mt_4 <- mt_measures(mt_4)
mt_4 <- mt_angles(mt_4,use='tn_trajectories')
mt_4 <- mt_derivatives(mt_4,use='tn_trajectories')
iat2_4=cbind(mt_4$data,mt_4$measures,mt_4$tn_trajectories)
iat2_4$group='reversed'
iat2_4$run='congruent'

iat7 <- mt_import_wide(iat7)
mt_7 <- mt_remap_symmetric(iat7)
mt_7 <- mt_align_start_end(mt_7)
mt_7 <- mt_time_normalize(mt_7)
mt_7 <- mt_measures(mt_7)
mt_7 <- mt_angles(mt_7,use='tn_trajectories')
mt_7 <- mt_derivatives(mt_7,use='tn_trajectories')
iat2_7=cbind(mt_7$data,mt_7$measures,mt_7$tn_trajectories)
iat2_7$group='reversed'
iat2_7$run='incongruent'

# merge and save all
alldata=rbind(iat1_4,iat1_7,iat2_4,iat2_7)
write.csv(alldata,'/Users/orlacamus/Desktop/projects/IAT240615/merged_data.csv')

# check for every subject: revise manually
alldata=alldata[,!duplicated(colnames(alldata))]

number=alldata %>% group_by(subjID,run) %>% summarise(n = n(),)
number[number$n==400,] #sub108-inc-109; sub132-con-133; sub154-inc-keep first 200
ses=number %>% group_by(subjID) %>% summarise(n = n(),)
ses[ses$n==1,] #sub109; sub133

## check for 400-trial subject
temp=alldata[(alldata$subjID==154) & (alldata$run=='incongruent'),]
temp1=temp[1:200,]
temp2=temp[201:400,]
mean(temp1$error)
mean(temp2$error)
mean(temp1$RT)
mean(temp2$RT)



# filter: error trials +600ms, trials RT<300ms, subj with >10% trials RT>10s
setwd("/Users/orlacamus/Desktop/projects/IAT240615")
alldata=read.csv('/Users/orlacamus/Desktop/projects/IAT240615/merged_data.csv')
alldata=alldata[alldata$subjID!=159,]# no questionnaire
subList=unique(alldata$subjID)

temp=alldata[alldata$RT<=300,] %>% group_by(subjID) %>% summarise(n = n(),)
data=alldata[alldata$RT<10000,]
temp=data %>% group_by(subjID) %>% summarise(acc = 1-mean(error),)
data[data$error==1,]$RT=data[data$error==1,]$RT+600
data$event='zero'
for (i in c(1:length(data$order))){
  if ((data[i,'condition']=='real')|(data[i,'condition']=='unreal')){
    data[i,'event']=data[i,'condition']
  }
  if (grepl('ev1',data[i,'condition'],  fixed = TRUE)){
    data[i,'event']='CR'
  }
  if (grepl('ev2',data[i,'condition'],  fixed = TRUE)){
    data[i,'event']='CI'
  }
}

data=data[data$error==0,]

# IATscore
mean=data %>% group_by(subjID,run) %>% summarise(RT = mean(RT),)
std=data %>% group_by(subjID) %>% summarise(RT = sd(RT),)
IATscore=data.frame((mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT)
colnames(IATscore)=c('IATscore')
IATscore$subjID=unique(data$subjID)
IATscore$group='reversed'
IATscore[IATscore$subjID<=133,]$group='seq'
summary(aov(data=IATscore,IATscore~group))

# MT indices
MTdata=data[data$error==0,]
mean=MTdata %>% group_by(subjID,run) %>% dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))
t.test(mean[mean$run=='incongruent',]$AUC,mean[mean$run=='congruent',]$AUC,paired = TRUE)
std=MTdata %>% group_by(subjID) %>% summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
MTscore=std
MTscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
MTscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
MTscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
MTscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC

# load questionnaire and corr matrix
ques=read_excel('questionnaire_IAT/ques.xlsx')

temp=cbind(ques,IATscore,MTscore)
temp <- temp[,-which(names(temp) %in% c('subj','subjID','group'))]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#3d6e85', "white", '#3d6e85'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)


cor.test(IATscore$IATscore,MTscore$MD_above)
cor.test(IATscore$IATscore,MTscore$AD)
cor.test(IATscore$IATscore,MTscore$AUC)
cor.test(IATscore$IATscore,MTscore$MAD)

t.test(IATscore$IATscore,MTscore$MD_above,paired = TRUE)
t.test(IATscore$IATscore,MTscore$AD,paired = TRUE)
t.test(IATscore$IATscore,MTscore$AUC,paired = TRUE)
t.test(IATscore$IATscore,MTscore$MAD,paired = TRUE)

t.test(IATscore$IATscore,mu=0)
t.test(MTscore$AD,mu=0)
t.test(MTscore$AUC,mu=0)
t.test(MTscore$MAD,mu=0)
t.test(MTscore$MD_above,mu=0)

temp <- MTscore[,-which(names(MTscore) %in% c('subjID'))]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#004ba8', "white", '#004ba8'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=4, height=4)


# bar plot total IAT effect
MTscore$RT=IATscore$IATscore
colnames(MTscore)[colnames(MTscore)=='MD_above']='MD'
MTscore_long=pivot_longer(MTscore,cols = c("RT","MAD","AD","MD","AUC"))

agg <- MTscore_long %>% 
  group_by(name) %>% 
  dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se = sd / sqrt(n)) 


f = ggplot(data = agg, aes(x = name, y=mean)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8,fill='#d6e3f8')  + 
  geom_point(data = MTscore_long, aes(x = name, y=value),size=3,position = position_jitter(seed = 1, width = 0),color='#004ba875') +
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mean-se, ymax=mean+se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='metrics') + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='IAT effect') +
  coord_cartesian(ylim = c(0,0.5))+
  geom_hline(yintercept=0, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/IATeffect_merge.svg'), plot=f, width=6, height=4)

t.test(MTscore$RT,MTscore$MAD,paired = TRUE)
t.test(MTscore$RT,MTscore$AD,paired = TRUE)
t.test(MTscore$RT,MTscore$MD,paired = TRUE)
t.test(MTscore$RT,MTscore$AUC,paired = TRUE)


# correlation with merge IAT effect
plot(MTscore$MD,ques$aq)
plot(MTscore$AUC,ques$mmq_so)
plot(MTscore$MAD,ques$past_pos)
plot(MTscore$MD,ques$past_pos)
plot(MTscore$AUC,ques$pre_hed)

cor.test(MTscore$MD,ques$aq)
cor.test(MTscore$AUC,ques$mmq_so)
cor.test(MTscore$MAD,ques$past_pos)
cor.test(MTscore$MD,ques$past_pos)
cor.test(MTscore$AUC,ques$pre_hed)


# organize for bar plot for CR &CI
MTdata=data[data$error==0,]
mean=MTdata[(MTdata$event=='CR')|(MTdata$event=='real'),] %>% group_by(subjID,run) %>% dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))
write.csv(mean,'MT_CR_trial.csv')
std=MTdata[(MTdata$event=='CR')|(MTdata$event=='real'),] %>% group_by(subjID) %>% dplyr::summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
CRscore=std
CRscore$RT=(mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT
CRscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
CRscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
CRscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
CRscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC
CRscore$event='real'

mean=MTdata[(MTdata$event=='CI')|(MTdata$event=='unreal'),] %>% group_by(subjID,run) %>%dplyr::summarise(RT=mean(RT),MAD = mean(MAD),AD = mean(AD),MD_above = mean(MD_above),AUC = mean(AUC))
write.csv(mean,'MT_CI_trial.csv')
std=MTdata[(MTdata$event=='CI')|(MTdata$event=='unreal'),] %>% group_by(subjID) %>% dplyr::summarise(RT=sd(RT),MAD = sd(MAD),AD = sd(AD),MD_above = sd(MD_above),AUC = sd(AUC))
CIscore=std
CIscore$RT=(mean[mean$run=='incongruent',]$RT-mean[mean$run=='congruent',]$RT)/std$RT
CIscore$MAD=(mean[mean$run=='incongruent',]$MAD-mean[mean$run=='congruent',]$MAD)/std$MAD
CIscore$AD=(mean[mean$run=='incongruent',]$AD-mean[mean$run=='congruent',]$AD)/std$AD
CIscore$MD_above=(mean[mean$run=='incongruent',]$MD_above-mean[mean$run=='congruent',]$MD_above)/std$MD_above
CIscore$AUC=(mean[mean$run=='incongruent',]$AUC-mean[mean$run=='congruent',]$AUC)/std$AUC
CIscore$event='unreal'

allscore=rbind(CRscore,CIscore)
colnames(allscore)[colnames(allscore)=='MD_above']='MD'
allscore_long=pivot_longer(allscore,cols = c("RT","MAD","AD","MD","AUC"))


# bar plot for CI & CR

ev_colors=c('#ffbf00','#9ccc00')
ev_colors=c('#41337A','#41337A50')
ev_colors=c('#004ba8','#004ba850')

agg <- allscore_long %>% 
  group_by(event,name) %>% 
  dplyr::summarise(mean = mean(value),sd=sd(value),n = n(),se = sd / sqrt(n)) 


f = ggplot(data = agg, aes(x = name, y=mean,fill=event)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  scale_fill_manual(values=ev_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mean-se, ymax=mean+se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='metrics') + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='IAT effect') +
  coord_cartesian(ylim = c(0,0.4))+
  geom_hline(yintercept=0, linetype="dashed",color = "gray", size=2)+
  #ggtitle("") +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title=element_text(size=25),
        text = element_text(size=25,family ='Helvetica'),
        axis.ticks.length=unit(0.25, "cm"),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=25,hjust = 0.5),
        legend.position='none',
        legend.text = element_text(family ='Helvetica'),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/IATeffect.svg'), plot=f, width=6, height=4)


t.test(allscore[allscore$event=='real',]$RT,allscore[allscore$event=='unreal',]$RT,paired = TRUE) #x
t.test(allscore[allscore$event=='real',]$MAD,allscore[allscore$event=='unreal',]$MAD,paired = TRUE)
t.test(allscore[allscore$event=='real',]$AD,allscore[allscore$event=='unreal',]$AD,paired = TRUE) #x
t.test(allscore[allscore$event=='real',]$AUC,allscore[allscore$event=='unreal',]$AUC,paired = TRUE)
t.test(allscore[allscore$event=='real',]$MD,allscore[allscore$event=='unreal',]$MD,paired = TRUE)


# correlation with ques
ques=read_excel('questionnaire_IAT/ques.xlsx')

## CR
temp=cbind(ques,CRscore)
temp <- temp[,-which(names(temp) %in% c('subj','subjID','event'))]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#3d6e85', "white", '#3d6e85'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)

cor.test(temp$past_neg,temp$MD_above)
cor.test(temp$past_neg,temp$MAD)
plot(temp$past_neg,temp$MD)

## plot CR~ques
cor.test(temp$past_neg,temp$MAD)
cor.test(temp$past_neg,temp$RT)
cor.test(temp$past_neg,temp$MD_above)

pred <- predict(lm(MD_above ~ past_neg, temp), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = temp, aes(x = past_neg, y=MD_above)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(data = temp, aes(x = past_neg, y=MD_above),size=5, shape=16,color='#ffbf0080') +
  geom_smooth(data = temp, aes(x = past_neg, y=MD_above), col='#ffbf00', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='ZTPI: Past Negative') + # have tick marks for each session
  scale_y_continuous(name='IAT effect using MD\n(true+CR)') + 
  geom_line(aes(x = past_neg, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = past_neg, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/MD.svg'), plot=f, width=5, height=4)



cor.test(temp$past_neg,temp$RT)

pred <- predict(lm(RT ~ past_neg, temp), 
                se.fit = TRUE, interval = "confidence")
limits <- as.data.frame(pred$fit)


f=ggplot(data = temp, aes(x = past_neg, y=RT)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(data = temp, aes(x = past_neg, y=RT),size=5, shape=16,color='gray80') +
  geom_smooth(data = temp, aes(x = past_neg, y=RT), col='gray50', method = 'lm',se=F,size=2)+
  
  scale_x_continuous(name='ZTPI: Past Negative') + # have tick marks for each session
  scale_y_continuous(name='IAT effect using RT\n(true+CR)') + 
  geom_line(aes(x = past_neg, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = past_neg, y = limits$upr), 
            linetype = 2) +
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/RT.svg'), plot=f, width=5, height=4)



cor.test(temp$past_neg,temp$RT)



## CI
temp=cbind(ques,CIscore)
temp <- temp[,-which(names(temp) %in% c('subj','subjID','event'))]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#3d6e85', "white", '#3d6e85'))
f

cor.test(temp$aq,temp$AUC)
cor.test(temp$mmq,temp$AUC)
cor.test(temp$mmq_ss,temp$AUC)
cor.test(temp$mmq_so,temp$AUC)
cor.test(temp$pre_hed,temp$AUC)
cor.test(temp$fantasy,temp$AUC)
cor.test(temp$personal_distress,temp$MAD)



plot(temp$aq,temp$AUC)
plot(temp$mmq,temp$AUC)
plot(temp$mmq_ss,temp$AUC)
plot(temp$mmq_so,temp$AUC)
plot(temp$pre_hed,temp$AUC)
plot(temp$fantasy,temp$AUC)
plot(temp$personal_distress,temp$MAD)



## CR-CI
temp=cbind(ques,CRscore)
temp <- temp[,-which(names(temp) %in% c('subj','subjID','event'))]
temp$RT=CRscore$RT-CIscore$RT
temp$MAD=CRscore$MAD-CIscore$MAD
temp$AD=CRscore$AD-CIscore$AD
temp$MD=CRscore$MD_above-CIscore$MD_above
temp$AUC=CRscore$AUC-CIscore$AUC
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#3d6e85', "white", '#3d6e85'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)

cor.test(temp$past_neg,temp$MD)
plot(temp$past_neg,temp$MD)

