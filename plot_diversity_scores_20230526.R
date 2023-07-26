
# Header ------------------------------------------------------------------

# Purpose: Create plots of the diversity scores and diversity profile curves
# Author: Michael Lee Wood
# Last Edited: 20230530
# Required Packages: 
#  ggplot2
#  tidyr
#  dplyr

# Load packages and format data -------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)


#should have the objects 'd' and 'd_no_never' in memory after executing previous script
#create copies of the objects with the diversity scores
d2 <- d
d2_no_never <- d_no_never

#Create "topic" factor variables
d2$topic <- as.factor(rownames(d2))
d2_no_never$topic <- as.factor(rownames(d2_no_never))

#Calculate the difference between richness and shannon values
#(to order by the steepness of the slope)
d2$diff <- d2$richness - d2$shannon
d2_no_never$diff <- d2_no_never$richness - d2_no_never$shannon

#reshape to long
d_long <- gather(data = d2, key = Q, value = `Effective Number`, -c(topic,diff))
d_no_long <- gather(data = d2_no_never, key = Q, value = `Effective Number`, -c(topic,diff))

#recode the Q var for plotting
d_long$Q <- recode(d_long$Q, 
                   richness = 0,
                   q.1 = .1,
                   q.2 = .2,
                   q.3 = .3,
                   q.4 = .4,
                   q.5 = .5,
                   q.6 = .6,
                   q.7 = .7,
                   q.8 = .8,
                   q.9 = .9,
                   shannon = 1,
                   q1.1 = 1.1,
                   q1.2 = 1.2,
                   q1.3 = 1.3,
                   q1.4 = 1.4,
                   q1.5 = 1.5,
                   q1.6 = 1.6,
                   q1.7 = 1.7,
                   q1.8 = 1.8,
                   q1.9 = 1.9,
                   gsimpson = 2,
                   q2.1 = 2.1,
                   q2.2 = 2.2,
                   q2.3 = 2.3,
                   q2.4 = 2.4,
                   q2.5 = 2.5,
                   q2.6 = 2.6,
                   q2.7 = 2.7,
                   q2.8 = 2.8,
                   q2.9 = 2.9,
                   q3 = 3)

d_no_long$Q <- recode(d_no_long$Q, 
                      richness = 0,
                      q.1 = .1,
                      q.2 = .2,
                      q.3 = .3,
                      q.4 = .4,
                      q.5 = .5,
                      q.6 = .6,
                      q.7 = .7,
                      q.8 = .8,
                      q.9 = .9,
                      shannon = 1,
                      q1.1 = 1.1,
                      q1.2 = 1.2,
                      q1.3 = 1.3,
                      q1.4 = 1.4,
                      q1.5 = 1.5,
                      q1.6 = 1.6,
                      q1.7 = 1.7,
                      q1.8 = 1.8,
                      q1.9 = 1.9,
                      gsimpson = 2,
                      q2.1 = 2.1,
                      q2.2 = 2.2,
                      q2.3 = 2.3,
                      q2.4 = 2.4,
                      q2.5 = 2.5,
                      q2.6 = 2.6,
                      q2.7 = 2.7,
                      q2.8 = 2.8,
                      q2.9 = 2.9,
                      q3 = 3)



# Plot Shannon and Gini-Simpson Diversity ---------------------------------

#create a subset that has only richness, shannon, and gini-simpson (will help with plotting)
d_long_sub <- d_long %>% 
  filter(Q==0|Q==1|Q==2)

#create a subset that has only shannon and gini-simpson (will help with plotting)
d_long_sub2 <- d_long_sub %>% 
  filter(Q!=0)

#format and recode variables for plotting
d_long_sub2$Q <- as.character(d_long_sub2$Q)
d_long_sub2$Q <- recode(d_long_sub2$Q,
                        "1" = "Shannon",
                        "2" = "Gini-Simpson")
d_long_sub2$Q <- as.factor(d_long_sub2$Q)
d_long_sub2$Q <- factor(d_long_sub2$Q, levels = c("Gini-Simpson", "Shannon"))

#reorder levels by shannon entropy (highest to lowest)
d_long_sub2$topic <- factor(d_long_sub2$topic, levels = c("Wear Makeup",
                                                "Smartphone",
                                                "Start Dating",
                                                "Bedroom Computer",
                                                "Bedroom TV",
                                                "Have Sex",
                                                "R-Rated Movie",
                                                "Social Media",
                                                "Drink Regularly",
                                                "Unsupervised Parties",
                                                "Try Alcohol",
                                                "View Porn",
                                                "Swear",
                                                "Try Drugs"))

p_comb <- ggplot(data = d_long_sub2, aes(x = topic, y = `Effective Number`, fill = Q)) +
  geom_hline(yintercept = 0:15, alpha=.25)+
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha = 1)+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, 15, by = 1),sec.axis = dup_axis())+
  labs(title = "Effective Number of Age Periods by Topic",
       y="",
       x="",
       fill="Diversity Index")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=15))+
  coord_flip()+
  scale_fill_grey() #greyscale for publication
p_comb

#Export plot
jpeg("Effective_Numbers_20230530.jpg", units="in", width=11, height=8.5, res=400,quality=100)
p_comb
dev.off()

# Create Diversity Profiles -----------------------------------------------

dp <- ggplot(d_long) +
  geom_line(data=d_long, aes(x = Q, y = `Effective Number`, color = diff), linewidth = 1.5)+
  geom_point(data = d_long_sub, aes(x = Q, y = `Effective Number`), size = 2.5) +
  scale_x_continuous(breaks = c(0,1,2,3))+
  theme_minimal()+
  labs(title = "Diversity Profiles for Each Topic\n",
       y="Effective Number of Age Periods\n",
       x="\nValue of Q")+
  scale_colour_gradient2(
    midpoint = 3.5,
    low = "white",
    mid = "gray",
    high = "black",
    aesthetics = "color") +
  theme(legend.position = "")+
  facet_wrap(~ reorder(topic,-diff), ncol = 5)+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        strip.text = element_text(size = 11))
dp


#Export Plot
jpeg("Diversity_Profiles_20220701.jpg", units="in", width=11, height=8.5, res=400,quality=100)
dp
dev.off()

# Plot NO NEVER Diversity -------------------------------------------------

d_no_long_sub2 <- d_no_long %>% 
  filter(Q==1|Q==2)

d_no_long_sub2$Q <- as.character(d_no_long_sub2$Q)
d_no_long_sub2$Q <- recode(d_no_long_sub2$Q,
                        "1" = "Shannon",
                        "2" = "Gini-Simpson")
d_no_long_sub2$topic <- factor(d_no_long_sub2$topic, levels = c("Bedroom TV",
                                                          "Swear",
                                                          "Bedroom Computer",
                                                          "Wear Makeup",
                                                          "Smartphone",
                                                          "Start Dating",
                                                          "View Porn",
                                                          "Have Sex",
                                                          "Drink Regularly",
                                                          "Try Drugs",
                                                          "R-Rated Movie",
                                                          "Social Media",
                                                          "Try Alcohol",
                                                          "Unsupervised Parties"
                                                          ))

p_no_comb <- ggplot(data = d_no_long_sub2, aes(x = topic, y = `Effective Number`, fill = Q)) +
  geom_hline(yintercept = 0:17, alpha=.25)+
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha = 1)+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, 17, by = 1),sec.axis = dup_axis())+
  labs(title = "Effective Number of Age Categories by Topic",
       subtitle = "Excluding \'Never\'",
       y="",
       x="",
       fill="Diversity Index")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title=element_text(size=20),
        plot.subtitle =element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=15))+
  coord_flip()+
  scale_fill_grey() #greyscale for publication
p_no_comb

#Export Plot
jpeg("Effective_Numbers_NONEVER_20230530.jpg", units="in", width=11, height=8.5, res=400,quality=100)
p_no_comb
dev.off()
