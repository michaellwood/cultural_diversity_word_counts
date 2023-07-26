# Header ------------------------------------------------------------------

# Purpose: Functions and loops used to clean text
# Author: Michael Lee Wood
# Last Edited: 20220705

# Do not run. Only for documentation.

#I used this function to fix encoding errors
fix_encoding <- function(x){
  x <- gsub("â€œ", "“", x)  
  x <- gsub("â€", "”", x)
  x <- gsub("â€˜", "‘", x)
  x <- gsub("â€™", "’", x)
  x <- gsub("â€”", "–", x)
  x <- gsub("â€“", "—", x)
  x <- gsub("â€¢", "-", x)
  x <- gsub("â€¦", "…", x)
  return(x)
}

#I used this function to standardize punctuation
standardize_punctuation <- function(x){
  x <- gsub("“","\"",x)
  x <- gsub("”","\"",x)
  x <- gsub("‘","\'",x)
  x <- gsub("’","\'",x)
  x <- gsub("–","-",x)
  x <- gsub("—","-",x)
  x <- gsub("…","...",x)
  return(x)
}

#I used this loop to do limited preprocessing of the interview data
for (n in 1:length(column_names)){
  #remove linebreaks (rm_between doesn't work when there are line breaks)
  df[,n] <- gsub("[\r\n]","",df[,n])
  #NEED TO REPLACE /'s with SPACE
  df[,n] <- gsub("R:"," ",df[,n])
  #remove references to specific times
  df[,n] <- gsub("\\d\\d:\\d\\d"," ", df[,n])
  df[,n] <- gsub("\\d:\\d\\d"," ", df[,n])
  #remove everything except letters, numbers spaces, and apostrophes
  df[,n] <- gsub("[^[:alnum:][:space:]']"," ",df[,n]) #add this if you want to keep asterisks\\*
  #make lowercase
  df[,n] <- tolower(df[,n])
  #gsub "four letter" which causes problems
  df[,n] <- gsub("four letter","fourletter", df[,n])
  df[,n] <- gsub("4 letter","fourletter", df[,n])
  
}