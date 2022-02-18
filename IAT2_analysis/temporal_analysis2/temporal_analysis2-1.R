  title: "IAT1-congruent/ incongruent"
  author: "Xinyi Xu"
  output:
    pdf_document: 
    fig_height: 4
    fig_width: 6
---
  # Preparations
  ## Load libraries

library(abind)
library(mousetrap)
library(ggplot2)
library(dplyr)
library(readbulk)
library(afex)

## Custom ggplot2 theme

theme_set(theme_classic()+ 
            theme(
              axis.line = element_line(colour = "black"),
              axis.ticks = element_line(colour = "black"),
              axis.text = element_text(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA)
            ))
rm(list=ls())
options(width=90)


# Import new dataset from block4


setwd("/Users/orlacamus/desktop/IATexp2/raw_data")
iat <- read_bulk("iat", fun=read_mt, extension=".mt")





iat <- mt_import_wide(iat)



mt_1 <- mt_remap_symmetric(iat)
mt_1 <- mt_align_start_end(mt_1)
mt_1 <- mt_time_normalize(mt_1)
mt_data <- mt_measures(mt_1)

#154的block4 多了200trials
mt_data$data <- mt_data$data[-(7801:8000),]
mt_data$measures <- mt_data$measures[-(7801:8000),]
mt_data$trajectories <- mt_data$trajectories[-(7801:8000),,]
mt_data$tn_trajectories <- mt_data$tn_trajectories[-(7801:8000),,]

#148顺序有误
k1<-mt_data$data[5601:5800,]
mt_data$data[5601:5800,]<-mt_data$data[5801:6000,]
mt_data$data[5801:6000,]<-k1


k2<-mt_data$trajectories[5601:5800,,]
mt_data$trajectories[5601:5800,,]<-mt_data$trajectories[5801:6000,,]
mt_data$trajectories[5801:6000,,]<-k2

k3<-mt_data$tn_trajectories[5601:5800,,]
mt_data$tn_trajectories[5601:5800,,]<-mt_data$tn_trajectories[5801:6000,,]
mt_data$tn_trajectories[5801:6000,,]<-k3

k4<-mt_data$measures[5601:5800,]
mt_data$measures[5601:5800,]<-mt_data$measures[5801:6000,]
mt_data$measures[5801:6000,]<-k4



# Preprocessing
## Spatial transformations

# Remap trajectories
mt_data <- mt_remap_symmetric(mt_data)

# Align trajectories to common start position
mt_data <- mt_align_start(mt_data, start=c(0,0))


## Resampling

# Time-normalize trajectories
mt_data <- mt_time_normalize(mt_data) 


########################################################################saved IAT

##add label congruent/incongruent as condition1
condition1 <- rep(1,400)
condition1[1:200] <- 'incongruent'
condition1[201:400] <- 'congruent'
condition1 <- rep(condition1,26)


mt_data[['data']]<-cbind(condition1,mt_data[['data']])
mt_data[['measures']]<-cbind(condition1,mt_data[['measures']])




## Filtering
# Only keep trials with correct answers and RT<5000
mt_data <- mt_subset(mt_data,RT<=5000,check='data')

mt_data <- mt_subset(mt_data,error==0,check='data')


########################################################data preprocess

# Analysis

## Aggregate trajectories

# Fig. 4
mt_plot_aggregate(mt_data, use="tn_trajectories",
                  x="xpos", y="ypos", 
                  color="condition1", subject_id="subjID")+
  scale_color_manual(values=c("darkorange","steelblue"))


## Calculate measures

# Calculate velocity and acceleration
mt_data <- mt_derivatives(mt_data)

# Calculate sample entropy
mt_data <- mt_sample_entropy(mt_data, use="tn_trajectories")


## Curvature

### Aggregate analyses

# Aggregate MAD values per participant and condition
agg_mad <- mt_aggregate_per_subject(mt_data, 
                                    use_variables="MAD", use2_variables="condition1",
                                    subject_id="subjID")


# Compare aggregated MAD values
t.test(MAD~condition1,data=agg_mad,paired=TRUE)

# Calculate descriptives
agg_mad %>% 
  group_by(condition1) %>% 
  summarise_at("MAD",.funs=c("mean","sd")) %>%
  as.data.frame()


### Trial level analyses

# Create data.frame that contains the
# trial variables and mouse-tracking indices
results <- merge(mt_data$data, mt_data$measures, by="mt_id")
# Load afex package



# Run linear mixed model with Condition as a fixed effect
# and a random intercept and slope per participant
mixed(MAD ~ (1+condition1.x|subjID)+condition1.x, data=results)
# add new model to do analysis
mixed(RT.x ~ (1|subjID) + condition1.x, data=results)

### Heatmap of individual trajectories
#Fig. 5, top

mt_heatmap(mt_data,
           xres=1000,
           colors=c("white","black"))




### Smoothed heatmap
#Fig. 5, middle

mt_heatmap(mt_data,
           xres=1000,
           colors=c("white","black"),
           smooth_radius=20,
           n_shades=10,
           mean_image=0.2)




### Difference of heatmaps between conditions


mt_diffmap(mt_data,
           xres=1000,
           condition=mt_data$data$condition1,
           colors=c("steelblue","white","darkorange"),
           smooth_radius=20,
           n_shades=10)



\pagebreak

## Temporal analyses

### Average x positions

# Plot aggregate time-normalized x-positions (Fig. 6)
mt_plot_aggregate(mt_data, use="tn_trajectories",
                  x="steps", y="xpos", color="condition1",
                  subject_id="subjID", points=TRUE)+
  scale_color_manual(values=c("darkorange","steelblue"))
# Aggregate time-normalized trajectories per condition
# separately per participant
av_tn_trajectories <- mt_aggregate_per_subject(mt_data,
                                               use="tn_trajectories", use2_variables="condition1",
                                               subject_id="subjID")

# Paired t-tests on coordinates
xpos_t_tests <- 
  with(av_tn_trajectories,
       sapply(unique(steps),function(i){
         t.test(xpos[condition1=="congruent" & steps==i],
                xpos[condition1=="incongruent" & steps==i],
                paired = TRUE)$p.value})
  )

# Retrieve all significant t-tests
which(xpos_t_tests<.05)


\pagebreak

### Riverbed plot
Fig. 7

mt_plot_riverbed(mt_data, use="tn_trajectories",
                 y="xpos", facet_col="condition1")

