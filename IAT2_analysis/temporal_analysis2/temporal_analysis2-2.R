##############################################################done import data again
condition1 <- rep(1,400)
condition1[1:200] <- 'incongruent'
condition1[201:400] <- 'congruent'
condition1 <- rep(condition1,26)


mt_data[['data']]<-cbind(condition1,mt_data[['data']])
mt_data[['measures']]<-cbind(condition1,mt_data[['measures']])

mt_data <- mt_subset(mt_data,RT<=5000,check='data')
#
mt_data <- mt_subset(mt_data,error==0,check='data')
#
#########################################################preprocess: add condition1+excludesome trials
# change it into four conditions and analyze the four conditions again
#add condition2:con_ev1; con_ev2: incon_ev1: incon_ev2

mt_3<-mt_data
mt_3 <- mt_subset(mt_data,condition!='real'&condition!='unreal')
condition2 <- mt_3[['data']][['condition']]

condition2[which(mt_3$data$condition1=='congruent'&(mt_3$data$condition=='ev1_word_real'|mt_3$data$condition=='ev1_imag_real'))] <- 'con_ev1'
condition2[which(mt_3$data$condition1=='congruent'&(mt_3$data$condition=='ev2_word_unreal'|mt_3$data$condition=='ev2_imag_unreal'))] <- 'con_ev2'
condition2[which(mt_3$data$condition1=='incongruent'&(mt_3$data$condition=='ev1_word_unreal'|mt_3$data$condition=='ev1_imag_unreal'))] <- 'incon_ev1'
condition2[which(mt_3$data$condition1=='incongruent'&(mt_3$data$condition=='ev2_word_real'|mt_3$data$condition=='ev2_imag_real'))] <- 'incon_ev2'

mt_3[['data']]<-cbind(condition2,mt_3[['data']])
mt_3[['measures']]<-cbind(condition2,mt_3[['measures']])

#########Analysis





# Analysis

## Aggregate trajectories

# Fig. 4
#4 condition trials
mt_plot_aggregate(mt_3, use="tn_trajectories",
                  x="xpos", y="ypos", 
                  color="condition2", subject_id="subjID")+
  scale_color_manual(values=c("darkorange","steelblue",'black','red'))


## Calculate measures ##calculated already in temporal analysis.R

# Calculate velocity and acceleration
mt_3 <- mt_derivatives(mt_3)
# Calculate trial-level indices
mt_3 <- mt_measures(mt_3)
# Calculate sample entropy
mt_3 <- mt_sample_entropy(mt_3, use="tn_trajectories")



## Curvature

### Aggregate analyses
####ev1
# Aggregate MAD values per participant for ev1
mt_ev1 <- mt_subset(mt_3,condition2=='con_ev1'|condition2=='incon_ev1')

agg_mad2 <- mt_aggregate_per_subject(mt_ev1, 
                                     use_variables="MAD", use2_variables="condition2",
                                     subject_id="subjID")
# Compare aggregated MAD values
t.test(MAD~condition2, data=agg_mad2, paired=TRUE)
# Calculate descriptives
agg_mad2 %>% 
  group_by(condition2) %>% 
  summarise_at("MAD",.funs=c("mean","sd")) %>%
  as.data.frame()






### Aggregate analyses(AD is significant for two wrong and true responses in conflict conditions)

# Aggregate AD values per participant and condition
agg_ad2 <- mt_aggregate_per_subject(mt_ev1, 
                                    use_variables="AD", use2_variables="condition2",
                                    subject_id="subjID")
# Compare aggregated MAD values
t.test(AD~condition2, data=agg_ad2, paired=TRUE)
# Calculate descriptives
agg_ad2 %>% 
  group_by(condition2) %>% 
  summarise_at("AD",.funs=c("mean","sd")) %>%
  as.data.frame()

### Aggregate analyses(AUC is significant for two wrong and true responses in conflict conditions)

# Aggregate AD values per participant and condition
agg_auc2 <- mt_aggregate_per_subject(mt_ev1, 
                                     use_variables="AUC", use2_variables="condition2",
                                     subject_id="subjID")
# Compare aggregated MAD values
t.test(AUC~condition2, data=agg_auc2, paired=TRUE)
# Calculate descriptives
agg_auc2 %>% 
  group_by(condition2) %>% 
  summarise_at("AUC",.funs=c("mean","sd")) %>%
  as.data.frame()




####ev2
# Aggregate MAD values per participant for ev2
mt_ev2 <- mt_subset(mt_3,condition2=='con_ev2'|condition2=='incon_ev2')

agg_mad2 <- mt_aggregate_per_subject(mt_ev2, 
                                     use_variables="MAD", use2_variables="condition2",
                                     subject_id="subjID")
# Compare aggregated MAD values
t.test(MAD~condition2, data=agg_mad2)
# Calculate descriptives
agg_mad2 %>% 
  group_by(condition2) %>% 
  summarise_at("MAD",.funs=c("mean","sd")) %>%
  as.data.frame()


### Aggregate analyses(AD is significant for two wrong and true responses in conflict conditions)

# Aggregate AD values per participant and condition
agg_ad2 <- mt_aggregate_per_subject(mt_ev2, 
                                    use_variables="AD", use2_variables="condition2",
                                    subject_id="subjID")
# Compare aggregated MAD values
t.test(AD~condition2, data=agg_ad2)
# Calculate descriptives
agg_ad2 %>% 
  group_by(condition2) %>% 
  summarise_at("AD",.funs=c("mean","sd")) %>%
  as.data.frame()

### Aggregate analyses(AUC is significant for two wrong and true responses in conflict conditions)

# Aggregate AD values per participant and condition
agg_auc2 <- mt_aggregate_per_subject(mt_ev2, 
                                     use_variables="AUC", use2_variables="condition2",
                                     subject_id="subjID")
# Compare aggregated MAD values
t.test(AUC~condition2, data=agg_auc2)
# Calculate descriptives
agg_auc2 %>% 
  group_by(condition2) %>% 
  summarise_at("AUC",.funs=c("mean","sd")) %>%
  as.data.frame()

### Trial level analyses

# Create data.frame that contains the
# trial variables and mouse-tracking indices
results50 <- merge(mt_3$data, mt_3$measures, by="mt_id")
# Load afex package
library(afex)

# Run linear mixed model with Condition as a fixed effect
# and a random intercept and slope per participant
MAD50<-mixed(MAD ~ (1+condition2|subjID)+condition2, data=results50)
AD50<-mixed(AD ~ (1+condition2|subjID)+condition2, data=results50)
AUC50<-mixed(AUC ~ (1+condition2|subjID)+condition2, data=results50)
MAD50
AD50
AUC50



## Trajectory types

### Heatmap of individual trajectories
#Fig. 5, top

mt_heatmap(mt_3,
           xres=1000,
           colors=c("white","black"))


#\pagebreak

### Smoothed heatmap
#Fig. 5, middle

mt_heatmap(mt_3,
           xres=1000,
           colors=c("white","black"),
           smooth_radius=20,
           n_shades=10,
           mean_image=0.2)


#\pagebreak

### Difference of heatmaps between conditions
#Fig. 5, bottom
#```{r}

mt_diffmap(mt_ev1,
           xres=1000,
           condition=mt_ev1$data$condition2,
           colors=c("steelblue","white","darkorange"),
           smooth_radius=20,
           n_shades=10)
mt_diffmap(mt_ev2,
           xres=1000,
           condition=mt_ev2$data$condition2,
           colors=c("steelblue","white","darkorange"),
           smooth_radius=20,
           n_shades=10)


\pagebreak

## Temporal analyses

### Average x positions

# Plot aggregate time-normalized x-positions (Fig. 6)
mt_plot_aggregate(mt_3, use="tn_trajectories",
                  x="steps", y="xpos", color="condition2",
                  subject_id="subjID", points=TRUE)+
  scale_color_manual(values=c("darkorange","steelblue",'black','red'))
# Aggregate time-normalized trajectories per condition
# separately per participant
av_tn_trajectories <- mt_aggregate_per_subject(mt_3,
                                               use="tn_trajectories", use2_variables="condition2",
                                               subject_id="subjID")

# Paired t-tests on coordinates
xpos_t_tests_ev1 <- 
  with(av_tn_trajectories,
       sapply(unique(steps),function(i){
         t.test(xpos[condition2=="con_ev1" & steps==i],
                xpos[condition2=="incon_ev1" & steps==i],
                paired = TRUE)$p.value})
  )
# Retrieve all significant t-tests
which(xpos_t_tests_ev1<.05)





av_tn_trajectories <- mt_aggregate_per_subject(mt_3,
                                               use="tn_trajectories", use2_variables="condition2",
                                               subject_id="subjID")
av_tn_trajectories2<-subset(av_tn_trajectories,subjID!=159&subjID!=145&subjID!=143&subjID!=139)
# Paired t-tests on coordinates
xpos_t_tests_ev2 <- 
  with(av_tn_trajectories2,
       sapply(unique(steps),function(i){
         t.test(xpos[condition2=="con_ev2" & steps==i],
                xpos[condition2=="incon_ev2" & steps==i],
                paired = TRUE)$p.value})
  )
# Retrieve all significant t-tests
which(xpos_t_tests_ev2<.05)

\pagebreak

### Riverbed plot
Fig. 7

mt_plot_riverbed(mt_3, use="tn_trajectories",
                 y="xpos", facet_col="condition2")
