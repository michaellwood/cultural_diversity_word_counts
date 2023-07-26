
# Header ------------------------------------------------------------------

# Purpose: Calculate diversity scores
# Author: Michael Lee Wood
# Last Edited: 20220701

# Required Packages: 
#  dplyr
#  tidyr
#  diverse 


# Import data and load packages -------------------------------------------

library(dplyr)
library(tidyr)
library(diverse)

keyword_counts_long <- read.csv("keyword_counts_long.csv")

#create subset that drops "never" responses 
keyword_counts_no_never <- keyword_counts_long %>% 
  filter(keyword!="Never")


# Calculate diversity scores ----------------------------------------------

# calculate richness (q=0)
d <- diverse::diversity(keyword_counts_long,type = "td",q=0)
d <- rename(d, "richness"="hill.numbers")
# calculate effective numbers based on the shannon index (q=1)
d$shannon <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1))
# calculate effective numbers based on the gini-simpson index  (q=2)
d$gsimpson <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2))

# To visualize diversity profile curves, we need to calculate 
# effective numbers for more values of q 
d$q.1 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.1))
d$q.2 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.2))
d$q.3 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.3))
d$q.4 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.4))
d$q.5 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.5))
d$q.6 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.6))
d$q.7 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.7))
d$q.8 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.8))
d$q.9 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=.9))

d$q1.1 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.1))
d$q1.2 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.2))
d$q1.3 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.3))
d$q1.4 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.4))
d$q1.5 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.5))
d$q1.6 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.6))
d$q1.7 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.7))
d$q1.8 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.8))
d$q1.9 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=1.9))

d$q2.1 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.1))
d$q2.2 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.2))
d$q2.3 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.3))
d$q2.4 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.4))
d$q2.5 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.5))
d$q2.6 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.6))
d$q2.7 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.7))
d$q2.8 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.8))
d$q2.9 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=2.9))
d$q3 <- unlist(diverse::diversity(keyword_counts_long,type = "td",q=3))




#Do the same as above, but for the "no never" subset
d_no_never <- diverse::diversity(keyword_counts_no_never,type = "td",q=0)
d_no_never <- rename(d_no_never, "richness"="hill.numbers")
d_no_never$shannon <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1))
d_no_never$gsimpson <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2))


d_no_never$q.1 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.1))
d_no_never$q.2 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.2))
d_no_never$q.3 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.3))
d_no_never$q.4 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.4))
d_no_never$q.5 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.5))
d_no_never$q.6 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.6))
d_no_never$q.7 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.7))
d_no_never$q.8 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.8))
d_no_never$q.9 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=.9))

d_no_never$q1.1 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.1))
d_no_never$q1.2 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.2))
d_no_never$q1.3 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.3))
d_no_never$q1.4 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.4))
d_no_never$q1.5 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.5))
d_no_never$q1.6 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.6))
d_no_never$q1.7 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.7))
d_no_never$q1.8 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.8))
d_no_never$q1.9 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=1.9))

d_no_never$q2.1 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.1))
d_no_never$q2.2 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.2))
d_no_never$q2.3 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.3))
d_no_never$q2.4 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.4))
d_no_never$q2.5 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.5))
d_no_never$q2.6 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.6))
d_no_never$q2.7 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.7))
d_no_never$q2.8 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.8))
d_no_never$q2.9 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=2.9))
d_no_never$q3 <- unlist(diverse::diversity(keyword_counts_no_never,type = "td",q=3))

#You are now ready to plot the diversity scores and the diversity profile curves!
