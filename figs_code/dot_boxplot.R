library(openxlsx)
library(ggplot2)
source('ggplot_setup.R')
data1<-read.xlsx('merge.xlsx')

####################

# calculate mean rating for session and evidence combinations
rt_means <- data1 %>% group_by(condition1, subj_idx) %>% 
                    summarise(rt=mean(rt)) %>%
                    arrange(condition1, subj_idx) %>%
                    ungroup()

# get unique variable code for each evidence combination
ev_codes <- rt_means %>% select(condition1) %>% 
                         distinct(condition1) %>%
                         mutate(code=as.factor(row_number()))

# add code column to scenario means
rt_means <- rt_means %>% merge(ev_codes) %>% arrange(condition1,subj_idx)
#code_means <- rt_means %>% group_by(condition1) %>% 
#  summarise(rt=mean(rt)) %>%
#  ungroup() %>%
#  mutate_at(c("condition1"), funs(substr(as.character(.), 1, 1))) %>%
#  mutate_at(c("subj_idx"), funs(sapply(strsplit(as.character(.), " "), `[`, 1)))


t.test(x=data1[data1$stim5=='incon_word',]$rt,y=data1[data1$stim5=='incon_image',]$rt)
plt_2a <- ggplot(rt_means) +
     geom_boxplot(data=rt_means,aes(x=code, y=rt), outlier.size=0, outlier.stroke=0,fatten=NULL) +
     geom_jitter(data=rt_means,aes(code, rt), width=0.2, alpha=0.5, color=color_genpop) +
     stat_summary(fun = mean, aes(code, rt),geom = "point",shape=5,size=4,color='red',stroke=2)+
     #geom_point(data=rt_means,aes(x=code, y=rt), color="red", shape=5, size=5, stroke=2) +
     #ylab('RT') +
     #coord_cartesian(ylim = c(600,2600), expand = FALSE) +
     scale_y_continuous(limits=c(600, 2600),breaks = seq(600,2600, 200))+
     th +
     theme(
       plot.margin=unit(c(10, 10, 10, 10), "points"),
       axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
       axis.text.y = element_text(size = 20),
       axis.title.x = element_blank(),
       panel.border = element_blank()
     )
plt_2a

plt_2 <- ggplot_gtable(ggplot_build(plt_2a))
plt_2$layout$clip[plt_2$layout$name == "panel"] <- "off"
grid::grid.draw(plt_2)
####################



