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


# read data
setwd("/Users/orlacamus/Desktop/projects/IAT240615/")
RTs=read_excel('3.simulated_RT/RTs.xlsx')
IAT=read_excel('3.simulated_RT/IAT.xlsx')

RTs_PN=read_excel('3.simulated_RT/RTs_PN.xlsx')
IAT_PN=read_excel('3.simulated_RT/IAT_PN.xlsx')

RTs_AUC=read_excel('3.simulated_RT/RTs_AUC.xlsx')
IAT_AUC=read_excel('3.simulated_RT/IAT_AUC.xlsx')

RTs_PN_AUC=read_excel('3.simulated_RT/RTs_PN_AUC.xlsx')
IAT_PN_AUC=read_excel('3.simulated_RT/IAT_PN_AUC.xlsx')

# test
targetIAT=IAT_PN_AUC


# overall IAT effect: empirical~simulated
cor.test(targetIAT$merge,IATscore$IATscore)
t.test(targetIAT$merge,IATscore$IATscore,paired = TRUE)
temp=targetIAT
temp$empscore=IATscore$IATscore

f=ggplot(data = temp, aes(x = empscore, y=merge)) + 
  
  #geom_point(data = agg, aes(x = factor(run), y=mean),size=5, shape=16,alpha=1,color='#7209b7') +
  #geom_errorbar(width=0.2, alpha=0.8,size=1,data = agg,aes(ymin=mean-se, ymax=mean+se,),position = position_nudge(x = 0),color='#7209b7') +
  #geom_smooth(data = data, aes(x = as.numeric(run), y = similarity), col="#3a0ca3", method = 'lm',se=F)+
  
  
  geom_point(data = temp, aes(x = empscore, y=merge),size=5, shape=16,color='#004ba850') +
  geom_smooth(data = temp, aes(x = empscore, y=merge), col='#004ba8', method = 'lm',se=F,size=2)+
  ggtitle('overall IAT effect')+
  scale_x_continuous(name='empirical data') + # have tick marks for each session
  scale_y_continuous(name='simulated data') + 
  theme(axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.title=element_text(size=25),
        plot.title = element_text(size=25,hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/simRT_corr.svg'), plot=f, width=5, height=4)


# simulated overall/CR/CI IAT~empirical IATs

temp=targetIAT[,c('CR')]
temp=cbind(temp,CRscore)
temp <- temp[,-which(names(temp) %in% c('subjID','event'))]
temp=temp[temp$CR!="NaN",]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#ffbf00', "white", '#ffbf00'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)
cor.test(temp$CR,temp$MAD)
cor.test(temp$CR,temp$AD)
cor.test(temp$CR,temp$AUC)
cor.test(temp$CR,temp$MD)
cor.test(temp$CR,temp$RT)




temp=targetIAT[,c('CI')]
temp=cbind(temp,CIscore)
temp <- temp[,-which(names(temp) %in% c('subjID','event'))]
temp=temp[temp$CI!="NaN",]
corsca <- rcorr(as.matrix(temp), type="pearson")

pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#9ccc00', "white", '#9ccc00'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)


cor.test(temp$CI,temp$MAD)
cor.test(temp$CI,temp$AD)
cor.test(temp$CI,temp$AUC)
cor.test(temp$CI,temp$MD)
cor.test(temp$CI,temp$RT)





temp=targetIAT[,c('merge')]
temp=cbind(temp,MTscore)
temp <- temp[,-which(names(temp) %in% c('subjID','event'))]
col_order <- c("merge","RT","MAD","AD","MD","AUC"     )
temp <- temp[, col_order]
temp=temp[temp$merge!="NaN",]
corsca <- rcorr(as.matrix(temp), type="pearson")
pmatrix=corsca$P
f=ggcorrplot(corsca$r, method = "ellipse", p.mat = pmatrix,insig = "blank",type = "upper",col = c('#004ba8', "white", '#004ba8'))
f
ggsave(file=paste0('/Users/orlacamus/Downloads/matrix.svg'), plot=f, width=6, height=6)

cor.test(temp$merge,temp$MAD)
cor.test(temp$merge,temp$AD)
cor.test(temp$merge,temp$AUC)
cor.test(temp$merge,temp$MD)
cor.test(temp$merge,temp$RT)



# CR/CI IAT comparison
targetIAT=IAT_PN_AUC
temp=allscore[,c('subjID','RT','event')]
temp$datatype='empirical'
colnames(targetIAT)=c('merge','real','unreal')
targetIAT$subjID=subList
targetIAT$datatype='simulated'
targetIAT_long=pivot_longer(targetIAT[,c('subjID','real','unreal','datatype')],cols = c("real","unreal"))
colnames(targetIAT_long)=c("subjID","datatype","event","RT")
targetIAT_long=targetIAT_long[,c("subjID","RT","event","datatype")]
temp=rbind(temp,targetIAT_long)
temp=temp[temp$RT!='NaN',]
temp$RT=format(as.numeric(temp$RT), scientific = FALSE)
agg_sim <- temp %>% 
  group_by(event,datatype) %>% 
  dplyr::summarise(mean = mean(as.numeric(RT)),sd=sd(as.numeric(RT)),n = n(),se = sd / sqrt(n)) 

f = ggplot(data = agg_sim, aes(x = datatype, y=mean,fill=event)) + 
  geom_bar(position = position_dodge(0.8),stat = "identity",width=0.8)  + 
  #geom_point(data = MTscore_long, aes(x = name, y=value),size=3,position = position_jitter(seed = 1, width = 0),color='#004ba875') +
  scale_fill_manual(values=ev_colors)+
  geom_errorbar(width=0.2, size=1,
                aes(ymin=mean-se, ymax=mean+se),
                position=position_dodge(0.8)) +
  scale_x_discrete(name='metrics') + # have tick marks for each session
  scale_y_continuous(expand=c(0,0),name='IAT effect') +
  #coord_cartesian(ylim = c(0,0.5))+
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
ggsave(file=paste0('/Users/orlacamus/Downloads/IATeffect_merge.svg'), plot=f, width=4, height=4)




t.test(as.numeric(targetIAT$real),as.numeric(targetIAT$unreal),paired = TRUE)
