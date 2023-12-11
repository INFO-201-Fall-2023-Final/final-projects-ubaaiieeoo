library(dplyr)
library(stringr)
library(ggplot2)
drug_df <- read.csv("drugoverdose.csv")
hiv_df <- read.csv("hivaids.csv") 
s <- read.csv("hivaids.csv") 

hiv_df <- subset(hiv_df, select = -c(Borough, PROPORTION.OF.CONCURRENT.HIV.AIDS.DIAGNOSES.AMONG.ALL.HIV.DIAGNOSES, HIV.DIAGNOSES.PER.100.000.POPULATION, AIDS.DIAGNOSES.PER.100.000.POPULATION))
drug_df <- subset(drug_df, select = -c(INDICATOR, PANEL, PANEL_NUM, UNIT, UNIT_NUM, STUB_NAME_NUM, STUB_LABEL_NUM, YEAR_NUM, AGE, AGE_NUM, ESTIMATE, FLAG))

sex_drug_df <- c()
for (i in 1:nrow(drug_df)){
  pop <- drug_df$STUB_LABEL[i]
  if (grepl("Female", pop)){
    new_pop <- "Female"
  } else if (grepl("Male", pop)){
      new_pop <- "Male"
  } else {
      new_pop <- "Unknown"
  }
  sex_drug_df[i] <- new_pop
}
drug_df$Sex <- sex_drug_df


race_drug_df <- c()
for (i in 1:nrow(drug_df)){
  pop_r <- drug_df$STUB_LABEL[i]
  # one method 
  if (grepl("Not Hispanic or Latino", pop_r)){
    if (grepl("White", pop_r)){
      new_pop_r <- "White"
    } else if (grepl("Asian", pop_r)){
      new_pop_r <- "Asian"
    } else if(grepl("Black", pop_r)){
      new_pop_r <- "Black"
    } else {
      new_pop_r <- "Unknown"
    }
  } else if(grepl("Hispanic or Latino: All races", pop_r)){
    new_pop_r <- "Hispanic"
  } else if(grepl("White", pop_r)){
    new_pop_r <- "White"
  } else if(grepl("Asian", pop_r)){
    new_pop_r <- "Asian"
  } else if(grepl("Black", pop_r)){
    new_pop_r <- "Black"
  } else{
    new_pop_r <- "Unknown"
  }
  race_drug_df[i] <- new_pop_r
}
drug_df$Race <- race_drug_df


race_hiv_df <- c()
for (i in 1:nrow(hiv_df)){
  pop_r <- hiv_df$RACE.ETHNICITY[i]
  if(grepl("Asian/Pacific Islander", pop_r)){
    new_pop_r <- "Asian"
  } else if(grepl("White", pop_r)){
    new_pop_r <- "White"
  } else if(grepl("Hispanic", pop_r)){
    new_pop_r <- "Hispanic"
  } else if(grepl("Black", pop_r)){
    new_pop_r <- "Black"
  } else{
    new_pop_r <- "Unknown"
  }
  race_hiv_df[i] <- new_pop_r
  }
hiv_df$Race <- race_hiv_df

sum_hiv_df <- c()
for (i in 1:nrow(hiv_df)){
  sum <- suppressWarnings(as.numeric(hiv_df$TOTAL.NUMBER.OF.HIV.DIAGNOSES[i]) + as.numeric(hiv_df$TOTAL.NUMBER.OF.AIDS.DIAGNOSES[i]) + as.numeric(hiv_df$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSES[i]))
  sum_hiv_df[i] <- sum
}
hiv_df$SumHivAids <- sum_hiv_df

colnames(hiv_df)[3] <- "Sex"

df <- merge(x = hiv_df, y = drug_df, all.x = TRUE)

colnames(df)[1] <- "Year"
colnames(df)[4] <- "Neighborhood"
colnames(df)[6] <- "Total HIV"
colnames(df)[7] <- "Total Concurrent HIV Aids"
colnames(df)[8] <- "Total Aids"
df <- subset(df, select = -c(RACE.ETHNICITY, STUB_NAME, STUB_LABEL))


# df used for visualisation
clean_df <- filter(df, df$Race != "Unknown" & df$Sex != "Unknown" & df$Neighborhood != "Unknown" & df$Neighborhood != "All" )
clean_sex_df <- filter(df, df$Sex != "Unknown" &  df$Sex != "All"  )
clean_race_df <- filter(df, df$Race != "Unknown")