# Header ------------------------------------------------------------------

# Purpose: Functions I used to tag keywords
# Author: Michael Lee Wood
# Last Edited: 20220705

# Do not run. Only for documentation.



# Load Data and Packages --------------------------------------------------

library(dplyr)

# Load the keyword dictionary
dict <- read.csv("keyword_dict_2.csv")
# Fix number formatting
dict$Keyword <- gsub("\\\\b","\\b",dict$Keyword)
# Read in the prepped text data (NOT INCLUDED)
df <- read.csv("prepped_text_df.csv")


# Text Tagging Function ----------------------------------------------------------------

#Purpose: Tag a set of documents based on presence/absence of topics indicated by keywords

#Input: This function takes the following as input:
# 1. A dataframe with text data
# 2. The name of the column containing text data
# 3. A dataframe containing a keyword dictionary
# 4. The name of the column of keywords in the keyword dictionary
# 5. The name of the column of topics(categories) in the keyword dictionary
# 6. The name of the column containing document ids
# 7. A name that will be used to designate this group of texts

#Output: A dataframe containing the following variables:
#1. text_group: a new variable identifying the name of the set of docs (taken from the name of the text variable)
#2. text: the text, returned as-is to facilitate checking for tagging errors
#3. id: the document ids, returned as-is
#4-?. topics: n columns containing 0 or 1 in each cell indicating whether the topic was found in the document, where n = the number of topics to be tagged

dict <- dict %>% 
  dplyr::rename(Topic = Category)

create_tags <- function(text_df=NULL, text_var="", keyword_dict=NULL,keyword_var="",topic_var="",id="",group =""){
  
  #create a list of topics that is the basis of the loop
  topic_list <- unique(keyword_dict[[topic_var]])
  for(i in 1:length(topic_list)){
    topic <- topic_list[[i]]
    keyword_list <- keyword_dict %>% 
      filter(keyword_dict[[topic_var]]==topic)
    keyword_list <- keyword_list[[keyword_var]]
    text_df[[topic]] <- ifelse(grepl(paste(keyword_list,collapse="|"),
                                     text_df[[text_var]], perl = TRUE),
                               1,0)
  }
  text_df$text_group <- group
  subset_vars <- append(topic_list, c("text_group", text_var,id), after = 0)
  text_df <- text_df %>% 
    select(all_of(subset_vars))
  text_df <- text_df %>% 
    rename(text = all_of(text_var))
  
  return(text_df)
}


# Function Call Example ---------------------------------------------------
df_sex <- create_tags(text_df=df, text_var = "sex", keyword_dict = dict,keyword_var = "Keyword",topic_var = "Topic",id="id", group = "sex")
