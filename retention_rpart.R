require(tidyverse) || install.packages("tidyverse")
require(caret) || install.packages("caret")
require(rpart) || install.packages("rpart")

library(tidyverse); theme_set(theme_minimal())
library(caret)
library(rpart)
library(doMC)
registerDoMC(6)

################ DATA CLEANING ################
data_1 <- read.csv("retention_test_20180725.csv") 
data <- data_1 # takes awhile to load so making a copy. 
data <- data %>% dplyr::select(-RETURNED_NEXT_TERM_IND)

# counties of interest
data$slco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT035", 1, 0)
data$utco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT049", 1, 0)
data$weberco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT003", 1, 0)
data$davisco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT011", 1, 0)
data$other <- ifelse(data$COUNTY_OF_ORIGIN_CODE != "UT035" &
                       data$COUNTY_OF_ORIGIN_CODE != "UT049" &
                       data$COUNTY_OF_ORIGIN_CODE != "UT003" &
                       data$COUNTY_OF_ORIGIN_CODE != "UT011", 1, 0)

# GPA  as factor
data$high_school_gpa <- ifelse(data$HIGH_SCHOOL_GPA > 3.699, "A", 
                          ifelse(data$HIGH_SCHOOL_GPA > 2.699, "B", 
                            ifelse(data$HIGH_SCHOOL_GPA > 1.699, "C", 
                              ifelse(data$HIGH_SCHOOL_GPA > 0.699, "D", "F"))))
data$high_school_gpa[is.na(data$high_school_gpa) == T] <- "NA"

data$prior_ug_gpa <- ifelse(data$PRIOR_UG_GPA > 3.699, "A", 
                      ifelse(data$PRIOR_UG_GPA > 2.699, "B", 
                        ifelse(data$PRIOR_UG_GPA > 1.699, "C", 
                          ifelse(data$PRIOR_UG_GPA > 0.699, "D", "F"))))
data$prior_ug_gpa[is.na(data$prior_ug_gpa) == T] <- "NA"

data$term_ug_gpa <- ifelse(data$TERM_UG_GPA  > 3.699, "A", 
                      ifelse(data$TERM_UG_GPA  > 2.699, "B", 
                        ifelse(data$TERM_UG_GPA  > 1.699, "C", 
                          ifelse(data$TERM_UG_GPA  > 0.699, "D", "F"))))
data$term_ug_gpa[is.na(data$term_ug_gpa) == T] <- "NA"

data$PRIOR_UG_CREDITS[is.na(data$PRIOR_UG_CREDITS) == T] <- 0
data$FIRST_GENERATION_IND[is.na(data$FIRST_GENERATION_IND) == T] <- "N"

data <- data[, -which(names(data) %in% c("HIGH_SCHOOL_GPA", "TERM_UG_GPA", "PRIOR_UG_GPA"))]

# scaling CB data by CB pop. imputing mean for missing values
data$hs_pop <- data$HS_POP/data$TOTAL_CB_POP
data$ged_pop <- data$GED_POP/data$TOTAL_CB_POP
data$some_col_1_pop <- data$SOME_COL_1_POP/data$TOTAL_CB_POP
data$some_col_2_pop <- data$SOME_COL_2_POP/data$TOTAL_CB_POP
data$as_pop <- data$AS_POP/data$TOTAL_CB_POP
data$bs_pop <- data$BS_POP/data$TOTAL_CB_POP
data$ms_pop <- data$MS_POP/data$TOTAL_CB_POP
data$pd_pop <- data$PD_POP/data$TOTAL_CB_POP
data$phd_pop <- data$PHD_POP/data$TOTAL_CB_POP

data <- data[, -which(names(data) %in% c("HS_POP", "GED_POP", "SOME_COL_1_POP", "SOME_COL_2_POP", "AS_POP",
                                         "BS_POP", "MS_POP", "PD_POP", "PHD_POP", "TOTAL_CB_POP"))]

# Reducing the grain of CB education variables
data$no_col <- data$ged_pop + data$hs_pop
data$some_col <- data$some_col_1_pop + data$some_col_2_pop + data$as_pop
data$grad <- data$ms_pop + data$pd_pop + data$phd_pop

data <- data[, -which(names(data) %in% c("ged_pop", "hs_pop", "some_col_1_pop", "some_col_2_pop",
                                         "as_pop", "ms_pop", "pd_pop", "phd_pop"))]

data$missing_cb <- ifelse(is.na(data$MED_INC_CB) == T, 1, 0)

# imputing based on county code aggregates
data$bs_pop[is.na(data$bs_pop) == T] <- ifelse(data$slco[is.na(data$bs_pop) == T] == 1, median(data$bs_pop[data$slco == 1], na.rm = T), 
                                          ifelse(data$utco[is.na(data$bs_pop) == T] ==1, median(data$bs_pop[data$utco ==1], na.rm =T),
                                            ifelse(data$weberco[is.na(data$bs_pop) == T] == 1, median(data$bs_pop[data$weberco ==1], na.rm = T),
                                              ifelse(data$davisco[is.na(data$bs_pop) == T] == 1, median(data$bs_pop[data$davisco == 1], na.rm = T), 
                                                median(data$bs_pop[data$other == 1], na.rm = T)))))

data$some_col[is.na(data$some_col) == T] <- ifelse(data$slco[is.na(data$some_col) == T] == 1, median(data$some_col[data$slco == 1], na.rm = T), 
                                              ifelse(data$utco[is.na(data$some_col) == T] ==1, median(data$some_col[data$utco ==1], na.rm =T),
                                                ifelse(data$weberco[is.na(data$some_col) == T]== 1, median(data$some_col[data$weberco ==1], na.rm = T),
                                                  ifelse(data$davisco[is.na(data$some_col) == T] == 1, median(data$some_col[data$davisco == 1], na.rm = T), 
                                                    median(data$some_col[data$other == 1], na.rm = T)))))

data$no_col[is.na(data$no_col) == T] <- ifelse(data$slco[is.na(data$no_col) == T] == 1, median(data$no_col[data$slco == 1], na.rm = T), 
                                          ifelse(data$utco[is.na(data$no_col) == T] == 1, median(data$no_col[data$utco ==1], na.rm =T),
                                            ifelse(data$weberco[is.na(data$no_col) == T] == 1, median(data$no_col[data$weberco ==1], na.rm = T),
                                              ifelse(data$davisco[is.na(data$no_col) == T] == 1, median(data$no_col[data$davisco == 1], na.rm = T), 
                                                median(data$no_col[data$other == 1], na.rm = T)))))

data$grad[is.na(data$grad) == T] <- ifelse(data$slco[is.na(data$grad) == T] == 1, median(data$grad[data$slco == 1], na.rm = T), 
                                      ifelse(data$utco[is.na(data$grad) == T] == 1, median(data$grad[data$utco ==1], na.rm =T),
                                        ifelse(data$weberco[is.na(data$grad) == T] == 1, median(data$grad[data$weberco ==1], na.rm = T),
                                          ifelse(data$davisco[is.na(data$grad) == T] == 1, median(data$grad[data$davisco == 1], na.rm = T), 
                                            median(data$grad[data$other == 1], na.rm = T)))))

# imputing median CB income based on county.
data$MED_INC_CB[is.na(data$MED_INC_CB) == T] <- ifelse(data$slco[is.na(data$MED_INC_CB) == T] == 1, median(data$MED_INC_CB[data$slco == 1], na.rm = T), 
                                                  ifelse(data$utco[is.na(data$MED_INC_CB) == T] == 1, median(data$MED_INC_CB[data$utco ==1], na.rm =T),
                                                    ifelse(data$weberco[is.na(data$MED_INC_CB) == T] == 1, median(data$MED_INC_CB[data$weberco ==1], na.rm = T),
                                                      ifelse(data$davisco[is.na(data$MED_INC_CB) == T] == 1, median(data$MED_INC_CB[data$davisco == 1], na.rm = T), 
                                                        median(data$MED_INC_CB[data$other == 1], na.rm = T)))))


# Selecting potential rpart variables
tree_data <- data %>% select(GENDER, VETERAN_IND, VETERAN_DEPENDENT_IND, MARITAL_STATUS, HISPANIC_IND, 
                             ASIAN_IND, BLACK_IND, AMERICAN_INDIAN_IND, PACIFIC_ISLANDER_IND, WHITE_IND, 
                             REFUGEE_IND, TRANSFER_IN_IND, COHORT_TERM_AGE, FULL_TIME, HS_ONE_YEAR_IND, 
                             DISABILITY_IND, CONCURRENT_STUDENT_IND, FIRST_TERM_NON_CONCURRENT_IND, FIRST_TERM_EVER_IND,
                             TERM_CTE_IND, slco, utco, davisco, weberco, MED_INC_CB, high_school_gpa, prior_ug_gpa, term_ug_gpa, bs_pop,
                             no_col, some_col, grad, missing_cb, RETAINED)

tree_data <- tree_data %>% gather( "eth", "n", 5:10) %>% filter(n == "Y")

# demographics tree
demo_tree_data <- tree_data %>% select(GENDER, VETERAN_IND, 
                                       COHORT_TERM_AGE, TRANSFER_IN_IND, 
                                       FIRST_TERM_NON_CONCURRENT_IND, TERM_CTE_IND, 
                                       FIRST_TERM_EVER_IND, DISABILITY_IND, eth, 
                                       MED_INC_CB, REFUGEE_IND, bs_pop, no_col, some_col,
                                       RETAINED)

set.seed(1983)
tree_train_index <- createDataPartition(demo_tree_data$RETAINED, p = .8, 
                                        list = F, 
                                        times = 1)

demo_tree_train <- demo_tree_data[tree_train_index,] 
demo_tree_test <- demo_tree_data[-tree_train_index,]


tree_model <- train(RETAINED ~.,
                    data = demo_tree_train, 
                    method = "rpart2",
                    tuneLength = 10,
                    trControl = trainControl(method = "cv"))

plot(tree_model)

demo_pred <- predict(tree_model, 
                     newdata = demo_tree_test,
                     type= "class",
                     control = )

cm_rpart <- confusionMatrix(data = demo_pred, 
                reference = demo_tree_test$RETAINED)

ce(actual = demo_tree_test$RETAINED, 
   predicted = demo_pred)

plotcp_rpart <- plotcp(tree_model)


opt_index <- which.min(tree_model$cptable[, "xerror"])
cp_opt <- tree_model$cptable[opt_index, "CP"]

tree_model_opt <- prune(tree = tree_model, 
                        cp = cp_opt)


plot_rpart <- rpart.plot(tree_model_opt)
