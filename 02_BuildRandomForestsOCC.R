year_sets<- c(# Select years - we used the ones below. Suggest not running more than 1-2 at once. 
  # 1976,
  # 1979,
  # 1982,
  # 1983,
  # 1987,
  # 1991,
  # 1992,
  # 1993,
  # 1994,
  # 1996,
  # 1999,
  # 2002,
  # 2003,
  # 2007,
  # 2010,
  # 2011,
  # 2015,
  # 2019,
  # 2020,
  # 2021
  )


library(caret)
library(tidyverse)
library(ranger)


###directory
setwd("your_working_directory_here")


#######################
#CPS data code switches are staggered. Timeline for code switch comes from ipums website

#OCC:
#Codes
#OCC is a 4-digit numeric variable.
#(Codes for 1962-1967 are 2 digits; each is preceded by two zeros).
#(Codes for 1968-2002 are 3 digits; each is preceded by a zero in the first position.)

#TIMELINE:
#1962-1967
#1968-1971
#1972-1982
#1983-1991
#1992-aug1995
#sep1995-2002
#2003-2010
#2011-2019
#2020+

#IND:
#IND is a 4-digit numeric variable.
#(Codes for 1962-1967 are 2 digits; each is preceded by two zeroes in the first positions.)
#(Codes for 1968-2002 are 3 digits; each is preceded by a zero in the first position.)

#1962
#1963-1967
#1968-1970
#1971-1982
#1983-1991
#1992-2002
#2003-2008
#2009-2013
#2014-2019
#2020+
####################################################################################


###READING IN CROSSWALK DATA###

# Load Occupation Crosswalk
xwalk_name<- "https://raw.githubusercontent.com/IndOcc/CPScrosswalks/main/OCC_crosswalk_FULL.csv"


# Clean the crosswalk
crosswalk<- read.csv(xwalk_name)
crosswalk<- janitor::clean_names(crosswalk)
crosswalk<- crosswalk[-1]


###READING IN CPS DATA###

occ<- read.csv("data.csv")


for (i in 1:length(year_sets)){
  
  # Set year of interest for this loop
  year_of_interest<- year_sets[i] 
  #ARGUMENT
  #year_of_interest<- 1991
  
  # Select relevant variables
  temp_varlist <<- c("AGE",
                     "SEX",
                     "CPSIDP",
                     "OCC",
                     "CLASSWKR",
                     "EDUC")
  
  # Filter data to just year of interest
  occ_small<- occ %>% filter(YEAR==year_of_interest)
  
  
  print("through filtering")
  # Creates flags based on what year was read in
  year_ranges<- list(c(1971:1982), #1
                     c(1983:1991), #2
                     c(1992:1995), #3
                     c(1995:2002), #4
                     c(2003:2010), #5
                     c(2011:2019), #6
                     c(2020:2022)) #7
  in_year_range<- c(FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE)
  print("start year loop")
  for (x in 1:length(year_ranges)) {
    print(x)
    if (year_of_interest %in% year_ranges[[x]]){
      print(in_year_range[x])
      in_year_range[x]<- TRUE
      print(in_year_range)
    }
  }
  
  if (year_of_interest %in% seq(1976, 1982, by=1)){
    from <- '1971_1982'
  } else if (year_of_interest %in% seq(1983, 1991, by=1)){
    from <- '1983_1991'
  } else if (year_of_interest %in% seq(1992, 1994, by=1)){
    from <- '1992_aug1995'
  } else if (year_of_interest %in% seq(1996, 2002, by=1)){
    from <- 'sept1995_2002'
  } else if (year_of_interest %in% seq(2003, 2010, by=1)){
    from <- '2003_2010'
  } else if (year_of_interest %in% seq(2011, 2019, by=1)){
    from <- '2011_2019'
  } else if (year_of_interest %in% seq(2020, 2022, by=1)){
    from <- '2020'
  } else {
    print("Invalid Start Year Selected")
  }
  

  
  #Reshaping Crosswalk
  
  print("prep crosswalk code")
  explode_column<- c()
  for (x in 1:7){
    if(in_year_range[x]){
      map_column<- crosswalk[x]
    }
    if(!in_year_range[x]){
      explode_column<-append(explode_column,crosswalk[x])
    }
  }
  
  
  # This loop runs the forests, one for each set of years. 
  print("starting forest")
  for (x in 1:6){
  
    print("create crosswalk")
    # Create/format crosswalk
    used_crosswalk<- data.frame(map_column, explode_column[x])
    crosswalk_era<- names(explode_column[x])
    print(crosswalk_era)
    
    used_crosswalk<- used_crosswalk %>% distinct() %>% mutate(value = 1)  %>% spread(2, value,  fill = 0, sep="_")
    for (col in 1:ncol(crosswalk)){
      colnames(crosswalk)[col] <-  sub("[[:digit:]]+ ", "", colnames(crosswalk)[col])
      colnames(crosswalk)[col] <- sub(" ", "_", colnames(crosswalk)[col])
    }
    
    # Merge crosswalk and data
    print("merge")
    init_merged<- merge(used_crosswalk, occ_small, by.x=1, by.y = "OCC")
    
    names(init_merged)[1]<-"OCC" 
    init_merged$OCC<- as.factor(init_merged$OCC)
    init_merged$SEX<- as.factor(init_merged$SEX)
    init_merged$EDUC<- factor(init_merged$EDUC, order=TRUE, levels=c(0, 10,20,30,40,50,60,70,80,90,100,110,120))
    init_merged$CLASSWKR<- as.factor(init_merged$CLASSWKR)
    
    init_merged <- init_merged %>% select(-c('CPSIDP', 'YEAR', 'MONTH', 'IND')) #Remove non-predictors
    # Split data into training & test
    print("split")
    set.seed(18)#For reproducibility
    nums <- sample(seq_len(nrow(init_merged)), size = floor(.8 * nrow(init_merged)))
    
    
    train <- init_merged[nums,]
    trainx <- train[-1]
    test <- init_merged[-nums,]
    testx <- test[-1]

    # Fit & save forest
    print("fit forests")
    ranger_forest <- ranger(OCC~., data = train[complete.cases(train),], num.trees = 500)
    save(ranger_forest, file = paste0("forest models/forest_OCC", year_of_interest,"from_",crosswalk_era,".RData"))
    
    # Create predictions for testing purposes
    ranger_pred <- predict(ranger_forest, data = testx)
    
    # Create confusion matrix

    confusion <- confusionMatrix(ranger_pred$predictions, test$OCC)
    confusiontable <- as.data.frame(as.matrix(confusion))
    by_class <- as.data.frame(confusion$byClass)


    # Create Node Max Entropy Measure (Later changed to 'complexity')
    crosswalk2 <- crosswalk %>% select(c(paste0("occ_", from), crosswalk_era))
    crosswalk2 <- distinct(crosswalk2)
    one_one_c2 <- crosswalk2 %>% group_by(across(2)) %>% count()
    counts_other_year <- merge(crosswalk2, one_one_c2, all=TRUE)
    
    factor_levels <- levels(train$OCC)
    
    # This is actually used to get the entropy
    check<- as.data.frame(table(true = test$OCC, predicted = ranger_pred$predictions))
    accuracy_by_gp<- check %>%
      group_by(true) %>%
      summarise(accuracy = Freq[true==predicted] / sum(Freq))
    
    max_value <- as.data.frame(rep(0, length = length(names)))
    names(max_value)[1] <- "Entropy"
    
    for (i in 1:length(confusiontable)){
      val = as.integer(paste(accuracy_by_gp$true[i]))
      single_thing <- counts_other_year %>% filter(counts_other_year[2] == val)
      max_value[i,] <- max(single_thing$n)
    }
    
    comparison_table <- tibble(factor_levels, by_class, max_value)
    
    # Export features & write files
    filename = paste0("dataframe_", format(Sys.Date(), '%Y%m%d'), ".Rda")
    save(df, file=filename)
    
    write.csv(comparison_table,paste0("forest result/results_table_", year_of_interest,"from_",crosswalk_era,"_OCC.csv"), row.names = FALSE)
    write.csv(check, paste0("forest result/groupresults_", year_of_interest,"from_",crosswalk_era,"_OCC.csv"), row.names=FALSE)
    write.csv(confusiontable, paste0("forest result/confusion_", year_of_interest,"from_",crosswalk_era,"_OCC.csv"), row.names=FALSE)
  }
}
