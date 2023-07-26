
# header ------------------------------------------------------------------

# Purpose: create a plot of the concept frequencies for each topic
# Author: Michael Lee Wood
# Last Edited: 20210921


# Load Packages and import data -------------------------------------------

library(ggplot2)


# Two topic plot ----------------------------------------------------------
keyword_counts_smol <- keyword_counts_long %>% 
  filter(text_group == "R-Rated Movie" | text_group == "Social Media")

keyword_counts_smol <- keyword_counts_smol %>% 
  group_by(text_group) %>% 
  mutate(freq = round(n/sum(n), 3))

keyword_counts_smol$keyword <- as.factor(keyword_counts_smol$keyword)
levels(keyword_counts_smol$keyword)

#reorder levels by age category
keyword_counts_smol$keyword <- factor(keyword_counts_smol$keyword, 
                              levels = c("Any Age",
                                         "2",
                                         "3",
                                         "4",
                                         "5",
                                         "6",
                                         "7",
                                         "8",
                                         "9",
                                         "Double Digits",
                                         "10",
                                         "11",
                                         "12",
                                         "13",
                                         "Middle School",
                                         "Puberty",
                                         "14",
                                         "15",
                                         "16",
                                         "17",
                                         "High School",
                                         "Teenage",
                                         "Late Teens",
                                         "18",
                                         "19",
                                         "20",
                                         "20s",
                                         "21",
                                         "22",
                                         "23",
                                         "24",
                                         "25",
                                         "College",
                                         "Mid 20s",
                                         "Grad School",
                                         "Marriage",
                                         "Adult",
                                         "26",
                                         "27",
                                         "28",
                                         "29",
                                         "30",
                                         "40",
                                         "50",
                                         "Never"))

p <- ggplot(data = keyword_counts_smol, aes(x=(keyword), y = freq)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme_minimal()+
  labs(title = "Distribution of Age Categories by Topic",
       y="",
       x="")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, .25, by = .05),
                     limits = c(0,.25))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~ text_group,scales = "free")+
  theme(plot.title=element_text(size=20),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        strip.text = element_text(size = 11))+
  coord_flip()
p

jpeg("Freq_plot_20220705.jpg", units="in", width=11, height=8.5, res=400,quality=100)
p
dev.off()