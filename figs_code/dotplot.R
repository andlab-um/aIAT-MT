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
data1$stim5=0
data1[data1$condition=='compatible' & data1$stim4=='word',]$stim5='con_word'
data1[data1$condition=='compatible' & data1$stim4=='imag',]$stim5='con_image'
data1[data1$condition=='incompatible' & data1$stim4=='word',]$stim5='incon_word'
data1[data1$condition=='incompatible' & data1$stim4=='imag',]$stim5='incon_image'
 


rt_ses <- data1 %>% group_by(stim5) %>% 
                    summarise(rt_se=2*sd(rt)/sqrt(length(rt))) %>%
                    arrange(stim5) %>%
                    ungroup()

rt_means <- data1 %>% group_by(stim5) %>% 
                      summarise(rt_mean=mean(rt)) %>%
                      arrange(stim5) %>%
                      ungroup()%>% 
                      group_by(stim5) %>% 
                      summarise(rt_mean=mean(rt_mean)) %>%
                      arrange(stim5) %>%
                      ungroup() 

MAD_ses <- data1 %>% group_by(stim5) %>% 
  summarise(MAD_se=2*sd(MAD)/sqrt(length(MAD))) %>%
  arrange(stim5) %>%
  ungroup()

MAD_means <- data1 %>% group_by(stim5,subj_idx) %>% 
  summarise(MAD_mean=mean(MAD)) %>%
  arrange(stim5) %>%
  ungroup() %>% 
  group_by(stim5) %>% 
  summarise(MAD_mean=mean(MAD_mean)) %>%
  arrange(stim5) %>%
  ungroup() 



AD_ses <- data1 %>% group_by(stim5) %>% 
  summarise(AD_se=2*sd(AD)/sqrt(length(AD))) %>%
  arrange(stim5) %>%
  ungroup()

AD_means <- data1 %>% group_by(stim5) %>% 
  summarise(AD_mean=mean(AD)) %>%
  arrange(stim5) %>%
  ungroup()%>% 
  group_by(stim5) %>% 
  summarise(AD_mean=mean(AD_mean)) %>%
  arrange(stim5) %>%
  ungroup() 

AUC_ses <- data1 %>% group_by(stim5) %>% 
  summarise(AUC_se=2*sd(AUC)/sqrt(length(AUC))) %>%
  arrange(stim5) %>%
  ungroup()

AUC_means <- data1 %>% group_by(stim5) %>% 
  summarise(AUC_mean=mean(AUC)) %>%
  arrange(stim5) %>%
  ungroup()%>% 
  group_by(stim5) %>% 
  summarise(AUC_mean=mean(AUC_mean)) %>%
  arrange(stim5) %>%
  ungroup() 

# get unique variable code for each evidence combination
ev_codes <- rt_means %>% select(stim5) %>% 
  distinct(stim5) %>%
  mutate(code=as.factor(row_number()))
ev_codes <- MAD_means %>% select(stim5) %>% 
  distinct(stim5) %>%
  mutate(code=as.factor(row_number()))

ev_codes <- AD_means %>% select(stim5) %>% 
  distinct(stim5) %>%
  mutate(code=as.factor(row_number()))

# add code column to scenario means
rt <- rt_means %>% merge(rt_ses) %>% merge(ev_codes) %>% arrange(stim5)
MAD <- MAD_means %>% merge(MAD_ses) %>% merge(ev_codes) %>% arrange(stim5)
AD <- AD_means %>% merge(AD_ses) %>% merge(ev_codes) %>% arrange(stim5)
AUC <- AUC_means %>% merge(AUC_ses) %>% merge(ev_codes) %>% arrange(stim5)
############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects

plt_1 <- ggplot(data=rt) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=code, y=rt_mean, ymin=rt_mean-rt_se, ymax=rt_mean+rt_se,color=code),size=1.,
                  position=position_dodge(width = 0.75)) + 
  #scale_color_brewer(palette = "Blues",direction = -1)+
  scale_color_manual(values = c('#336600','#339900','#66CC00','#33CC00'))+
  #group_color_scale +
  coord_cartesian(ylim=c(1200,1700)) +
  labs(title="") +
  ylab("RT") +
  
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )

plt_1 <- ggplot(data=MAD) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=code, y=MAD_mean, ymin=MAD_mean-MAD_se, ymax=MAD_mean+MAD_se,color=code),size=1.,
                  position=position_dodge(width = 0.75)) + 
  #scale_color_brewer(palette = "Blues",direction = -1)+
  scale_color_manual(values = c('#003366','#003399','#336699','#0066CC'))+
  #group_color_scale +
  coord_cartesian(ylim=c(0.35,0.55)) +
  labs(title="") +
  ylab("MAD") +
  
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )

plt_1 <- ggplot(data=AD) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=code, y=AD_mean, ymin=AD_mean-AD_se, ymax=AD_mean+AD_se,color=code),size=1.,
                  position=position_dodge(width = 0.75)) + 
  #scale_color_brewer(palette = "Blues",direction = -1)+
  scale_color_manual(values = c('#660066','#990099','#993366','#CC6699'))+
  #group_color_scale +
  coord_cartesian(ylim=c(0.1,0.15)) +
  labs(title="") +
  ylab("AD") +
  
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )

plt_1 <- ggplot(data=AUC) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=code, y=AUC_mean, ymin=AUC_mean-AUC_se, ymax=AUC_mean+AUC_se,color=code),size=1.,
                  position=position_dodge(width = 0.75)) + 
  #scale_color_brewer(palette = "Blues",direction = -1)+
  scale_color_manual(values = c('#FF3300', '#FF6633', '#FF6600', '#FF9966'))+
  #group_color_scale +
  coord_cartesian(ylim=c(0.20,0.35)) +
  labs(title="") +
  ylab("AUC") +
  
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )



plt_1 <- ggplot_gtable(ggplot_build(plt_1))
plt_1$layout$clip[plt_1$layout$name == "panel"] <- "off"
grid::grid.draw(plt_1)

#Green '#336600','#339900','#66CC00','#33CC00' 
#Blue  '#003366','#003399','#336699','#0066CC'
#purple '#660066','#990099','#993366','#CC6699'
#orange  '#FF3300', '#FF6633', '#FF6600', '#FF9966'