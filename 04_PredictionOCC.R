start <- seq(1976, 2022, by=1) #Would suggest subsetting this before running. Note that 1995 is a special case, handle it manually. 

end <- c(1976, 1983, 1992, 1996, 2003, 2011, 2020) # Leave this alone, will get all years into one file for given year's predictions

library(tidyverse)
library(ranger)


setwd("your_working_directory_here")

# Read in data
df <- read.csv("data.csv", colClasses = c('CPSIDP' = 'character'))

# Load crosswalk
xwalk_name<- "https://raw.githubusercontent.com/IndOcc/CPScrosswalks/main/OCC_crosswalk_FULL.csv"

for (start_year in start){

  print(paste0("Start Year:", start_year))
  
  # Filter to year of interest, only those with occupation field filled in
  occ <- df %>% filter(YEAR == start_year) %>% filter(OCC != 0)

  # This makes it possible to have OCC in the output datafile
  dupe_OCC <- occ %>% select(OCC)
  colnames(dupe_OCC) <- c("dupe_OCC")
  occ <- cbind(occ, dupe_OCC)
  print("Data read in")
  
  
  # Special case, will do 1995 in two parts manually
  if (start_year %in% seq(1976, 1982, by=1)){
    from <- '1972_1982'
  } else if (start_year %in% seq(1983, 1991, by=1)){
    from <- '1983_1991'
  } else if (start_year %in% seq(1992, 1994, by=1)){
    from <- '1992_aug1995'
  } else if (start_year %in% seq(1996, 2002, by=1)){
    from <- 'sept1995_2002'
  } else if (start_year %in% seq(2003, 2010, by=1)){
    from <- '2003_2010'
  } else if (start_year %in% seq(2011, 2019, by=1)){
    from <- '2011_2019'
  } else if (start_year %in% seq(2020, 2022, by=1)){
    from <- '2020'
  } else {
    print("Invalid Start Year Selected")
  }
  
  # Prepare General Crosswalk
  crosswalk<- read_csv(xwalk_name)
  crosswalk<- janitor::clean_names(crosswalk)
  crosswalk<- crosswalk[-1]
  
  from1 <- paste0("occ_", from)
  crosswalk_subset <- crosswalk[c(from1)]
  
  dupe_col <- crosswalk_subset[1]
  names(dupe_col)[1] <- 'shadow'
  crosswalk_subset <- tibble(crosswalk_subset[1], dupe_col)
  used_crosswalk<- crosswalk_subset  %>% distinct %>% mutate(value = 1)  %>% spread(1, value,  fill = 0, sep="_")
  
  init_merged<- merge(used_crosswalk, occ, by.x='shadow', by.y = "OCC") 
  init_merged <- init_merged[-c(1)]
  
  # Create unique ID scaffold
  filtered_key <- tibble(init_merged$CPSIDP, init_merged$YEAR, init_merged$MONTH)
  colnames(filtered_key) <- c("CPSIDP", "YEAR", "MONTH")

  # Save unique data for later
  orig <- tibble(init_merged$YEAR, 
                 init_merged$MONTH, 
                 init_merged$CPSIDP, 
                 init_merged$IND, 
                 init_merged$dupe_OCC,
                 init_merged$AGE, 
                 init_merged$SEX, 
                 init_merged$EDUC, 
                 init_merged$CLASSWKR)
  colnames(orig) <- c("YEAR", "MONTH", "CPSIDP", "Original.IND", "Original.OCC", "AGE", "SEX", "EDUC.recoded", "CLASSWKR.recoded")
  
  # Handle 1995 manually - special case
  for (end_year in end){
    print(paste0("End year:", end_year))
    # Break if this round would be predicting itself
    if ((start_year %in% seq(1976, 1982, by=1)) && (end_year %in% seq(1976, 1982, by=1))){
      next
    } else if ((start_year %in% seq(1983, 1991, by=1)) && (end_year %in% seq(1983, 1991, by=1))){
      next
    } else if ((start_year %in% seq(1992, 1994, by=1)) && (end_year %in% seq(1992, 1994, by=1))){
      next
    } else if ((start_year %in% seq(1996, 2002, by=1)) && (end_year %in% seq(1996, 2002, by=1))){
      next
    } else if ((start_year %in% seq(2003, 2010, by=1)) && (end_year %in% seq(2003, 2010, by=1))){
      next
    } else if ((start_year %in% seq(2011, 2019, by=1)) && (end_year %in% seq(2011, 2019, by=1))){
      next
    } else if ((start_year %in% seq(2020, 2022, by=1)) && (end_year %in% seq(2020, 2022, by=1))){
      next
    }
      # Handling 1995 manually
    if (end_year %in% seq(1976, 1982, by=1)){
      first <-  1976
      second <- 1979
      third <- 1982
      set <- "1976.1982"
    } else if (end_year %in% seq(1983, 1991, by=1)){
      first <- 1983
      second <- 1987
      third <- 1991
      set <- "1983.1991"
    } else if (end_year %in% seq(1992, 1994, by=1)){
      first <- 1992
      second <- 1993
      third <- 1994
      set <- "1992.aug1995"
    } else if (end_year %in% seq(1996, 2002, by=1)){
      first <- 1996
      second <- 1999
      third <- 2002
      set <- "sept1995.2002"
    } else if (end_year %in% seq(2003, 2010, by=1)){
      first <- 2003
      second <- 2007
      third <- 2010
      set <- "2003.2010"
    } else if (end_year %in% seq(2011, 2019, by=1)){
      first <- 2011
      second <- 2015
      third <- 2019
      set <- "2011.2019"
    } else if (end_year %in% seq(2020, 2022, by=1)){
      first <- 2020
      second <- 2021
      third <- NA
      set <- "2020.2021"
    } else {
      print("Invalid End Year Selected")
    }
    
    if (start_year %in% seq(1976, 1982, by=1)){
      into <- '1972_1982'
    } else if (start_year %in% seq(1983, 1991, by=1)){
      into <- '1983_1991'
    } else if (start_year %in% seq(1992, 1994, by=1)){
      into <- '1992_aug1995'
    } else if (start_year %in% seq(1996, 2002, by=1)){
      into <- 'sept1995_2002'
    } else if (start_year %in% seq(2003, 2010, by=1)){
      into <- '2003_2010'
    } else if (start_year %in% seq(2011, 2019, by=1)){
      into <- '2011_2019'
    } else if (start_year %in% seq(2020, 2022, by=1)){
      into <- '2020'
    } else {
      print("Invalid Start Year Selected")
    }
    
    # Pull in appropriate models
    model1 <- paste0("forest models/forest_OCC", first, "from_occ_", into, ".RData")
    load(model1)
    forest1 <- ranger_forest
    
    model2 <- paste0("forest models/forest_OCC", second, "from_occ_", into, ".RData")
    load(model2)
    forest2 <- ranger_forest
    
    # We only have 2 years for our last model because 2022 is not finished, so this is optional
    if (!(end_year %in% seq(2020, 2022, by=1))){
      model3 <- paste0("forest models/forest_OCC", third, "from_occ_", into, ".RData")
      load(model3)
      forest3 <- ranger_forest
    }
    

    # Get factor levels that will be predicted
    first_fact <- df %>% filter(YEAR == first) %>% filter(OCC != 0)
    first_fact$OCC<- as.factor(first_fact$OCC)
    occ_factors <- data.frame(levels(first_fact$OCC))
    occ_factors['factor_number'] <- seq.int(nrow(occ_factors))
    
    
    #init_merged$IND<- as.factor(init_merged$IND)
    init_merged$SEX<- as.factor(init_merged$SEX)
    #init_merged$OCC<- as.factor(init_merged$OCC)
    init_merged$EDUC<- as.factor(init_merged$EDUC)
    init_merged$CLASSWKR<- as.factor(init_merged$CLASSWKR)
    
    x_vals <- init_merged %>% select(-c('CPSIDP', 'YEAR', 'MONTH', 'IND'))
    
    forest1_preds <- predict(forest1, data = x_vals, predict.all = TRUE)
    forest2_preds <- predict(forest2, data = x_vals, predict.all = TRUE)
    
    # Extract the predictions & convert from factor number to actual value
    pred_iters1 <- forest1_preds[["predictions"]]

    pred_iters2 <- forest2_preds[["predictions"]]
    
    if (!(end_year %in% seq(2020, 2022, by=1))){
      forest3_preds <- predict(forest3, data = x_vals, predict.all = TRUE)
      pred_iters3  <- forest3_preds[["predictions"]]
    }
    
    # Merge all the predictions together
    all_preds <- cbind(pred_iters1, pred_iters2)
    
    if (!(end_year %in% seq(2020, 2022, by=1))){
      all_preds <- cbind(all_preds, pred_iters3)
    }

    # Get winning prediction
    f <- function(x) with(rle(sort(x)), values[order(lengths, decreasing = TRUE)])
    fn_test <- apply(all_preds, 1, f)
    
    best_pred <- data.frame(unlist(map(fn_test, 1)))
    best_pred$predicted_classification <- factor(best_pred$unlist.map.fn_test..1.., levels = occ_factors$factor_number, labels = occ_factors$levels.first_fact.OCC.)
    

    winner <- best_pred$predicted_classification
    ct <- rowSums(all_preds == rep(best_pred$unlist.map.fn_test..1.., ncol(all_preds)))
    pred_perc <- data.frame(winning_val = winner,
                            ct = ct)
    
    # Generate certainty scores
    pred_perc$certainty_score <- lapply(pred_perc$ct, function(x) round((x/ncol(all_preds)),4))
    winner_colname <- paste0("OCC.Predicted.Value.", set)
    certainty_colname <- paste0("OCC.Prediction.Certainty.Score.", set)
    
    reduce <- tibble(pred_perc$winning_val, pred_perc$certainty_score)
    reduce <- data.frame(lapply(reduce, as.character), stringsAsFactors = FALSE)
    colnames(reduce) <- c(winner_colname, certainty_colname)
    
    print(paste0("Adding ", set))
    # Bind new columns to key
    filtered_key <- cbind(filtered_key, reduce)
    
  } 
  # Load file with IND predictions & merge in OCC predictions
  existing <- read.csv(paste0("Predictions/", start_year, "_into_other_schemas.csv"), colClasses = c('CPSIDP' = 'character'))
  merged <- cbind(orig, filtered_key)
  merged <- merged[-c(10,11,12)]
  
  fully_merged <- merge(existing, merged, by.all = c('YEAR', 'MONTH', 'CPSIDP'), all.x = TRUE, all.y = TRUE)
  
  # Save file with all predictions
  print(paste0("Saving Results: ", start_year))
  write.csv(fully_merged, paste0("Predictions/IND_OCC_", start_year, "_into_other_schemas.csv"), row.names = FALSE)

}

