require(tidyverse) || install.packages("tidyverse")
require(lme4) || install.packages("lme4")
require(caret) || install.packages("caret")

library(tidyverse); theme_set(theme_minimal())
library(lme4)
library(caret)

################# MLM data setup #################
data <- data_1
data <- data %>% dplyr::select(-RETURNED_NEXT_TERM_IND)

#### counties of interest ####
data$slco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT035", 1, 0)
data$utco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT049", 1, 0)
data$weberco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT003", 1, 0)
data$davisco <- ifelse(data$COUNTY_OF_ORIGIN_CODE == "UT011", 1, 0)

data$other <- ifelse(data$COUNTY_OF_ORIGIN_CODE != "UT035" &
                     data$COUNTY_OF_ORIGIN_CODE != "UT049" &
                     data$COUNTY_OF_ORIGIN_CODE != "UT003" &
                     data$COUNTY_OF_ORIGIN_CODE != "UT011", 1, 0)

#### GPA  as factor ####
# high school gpa
data$high_school_gpa <- ifelse(data$HIGH_SCHOOL_GPA > 3.699, "A", 
                         ifelse(data$HIGH_SCHOOL_GPA > 2.699, "B", 
                          ifelse(data$HIGH_SCHOOL_GPA > 1.699, "C", 
                           ifelse(data$HIGH_SCHOOL_GPA > 0.699, "D", "F"))))

data$high_school_gpa[is.na(data$high_school_gpa) == T] <- "NA"

# prior undergrad gpa
data$prior_ug_gpa <- ifelse(data$PRIOR_UG_GPA > 3.699, "A", 
                      ifelse(data$PRIOR_UG_GPA > 2.699, "B", 
                       ifelse(data$PRIOR_UG_GPA > 1.699, "C", 
                        ifelse(data$PRIOR_UG_GPA > 0.699, "D", "F"))))

data$prior_ug_gpa[is.na(data$prior_ug_gpa) == T] <- "NA"

# term undergrad gpa
data$term_ug_gpa <- ifelse(data$TERM_UG_GPA  > 3.699, "A", 
                     ifelse(data$TERM_UG_GPA  > 2.699, "B", 
                      ifelse(data$TERM_UG_GPA  > 1.699, "C", 
                       ifelse(data$TERM_UG_GPA  > 0.699, "D", "F"))))

data$term_ug_gpa[is.na(data$term_ug_gpa) == T] <- "NA"

# dropping redundent gpa columns
data <- data[, -which(names(data) %in% c("HIGH_SCHOOL_GPA", "TERM_UG_GPA", "PRIOR_UG_GPA"))]

# prior undergrad credits = NA set to 0
data$PRIOR_UG_CREDITS[is.na(data$PRIOR_UG_CREDITS) == T] <- 0

# First generation NA's set to "N"
data$FIRST_GENERATION_IND[is.na(data$FIRST_GENERATION_IND) == T] <- "N"

# scaling CB data by CB pop. imputing median for missing values below.
# will update the imputation later with a more sophisticated method.
data$hs_pop <- data$HS_POP/data$TOTAL_CB_POP
data$ged_pop <- data$GED_POP/data$TOTAL_CB_POP
data$some_col_1_pop <- data$SOME_COL_1_POP/data$TOTAL_CB_POP
data$some_col_2_pop <- data$SOME_COL_2_POP/data$TOTAL_CB_POP
data$as_pop <- data$AS_POP/data$TOTAL_CB_POP
data$bs_pop <- data$BS_POP/data$TOTAL_CB_POP
data$ms_pop <- data$MS_POP/data$TOTAL_CB_POP
data$pd_pop <- data$PD_POP/data$TOTAL_CB_POP
data$phd_pop <- data$PHD_POP/data$TOTAL_CB_POP

# dropping redundent census data columns
data <- data[, -which(names(data) %in% c("HS_POP", "GED_POP", "SOME_COL_1_POP", "SOME_COL_2_POP", "AS_POP",
                                         "BS_POP", "MS_POP", "PD_POP", "PHD_POP", "TOTAL_CB_POP"))]

# Reducing the grain of CB education variables
data$no_col <- data$ged_pop + data$hs_pop
data$some_col <- data$some_col_1_pop + data$some_col_2_pop + data$as_pop
data$grad <- data$ms_pop + data$pd_pop + data$phd_pop

# dropping redundent columns
data <- data[, -which(names(data) %in% c("ged_pop", "hs_pop", "some_col_1_pop", "some_col_2_pop",
                                         "as_pop", "ms_pop", "pd_pop", "phd_pop"))]

# creating a dummy indicator for missing census info
data$missing_cb <- ifelse(is.na(data$MED_INC_CB) == T, 1, 0)

# imputing based on county code aggregates for missing census info
data$bs_pop[is.na(data$bs_pop) == T] <- ifelse(data$slco[is.na(data$bs_pop) == T] == 1, 
                                               median(data$bs_pop[data$slco == 1], na.rm = T), 
                                         ifelse(data$utco[is.na(data$bs_pop) == T] ==1, 
                                                median(data$bs_pop[data$utco ==1], na.rm =T),
                                          ifelse(data$weberco[is.na(data$bs_pop) == T] == 1, 
                                                 median(data$bs_pop[data$weberco ==1], na.rm = T),
                                           ifelse(data$davisco[is.na(data$bs_pop) == T] == 1,
                                                  median(data$bs_pop[data$davisco == 1], na.rm = T), 
                                             median(data$bs_pop[data$other == 1], na.rm = T)))))

data$some_col[is.na(data$some_col) == T] <- ifelse(data$slco[is.na(data$some_col) == T] == 1,
                                                   median(data$some_col[data$slco == 1], na.rm = T), 
                                             ifelse(data$utco[is.na(data$some_col) == T] ==1,
                                                   median(data$some_col[data$utco ==1], na.rm =T),
                                              ifelse(data$weberco[is.na(data$some_col) == T]== 1, 
                                                    median(data$some_col[data$weberco ==1], na.rm = T),
                                               ifelse(data$davisco[is.na(data$some_col) == T] == 1, 
                                                    median(data$some_col[data$davisco == 1], na.rm = T), 
                                                median(data$some_col[data$other == 1], na.rm = T)))))

data$no_col[is.na(data$no_col) == T] <- ifelse(data$slco[is.na(data$no_col) == T] == 1, 
                                               median(data$no_col[data$slco == 1], na.rm = T), 
                                         ifelse(data$utco[is.na(data$no_col) == T] == 1, 
                                               median(data$no_col[data$utco ==1], na.rm =T),
                                          ifelse(data$weberco[is.na(data$no_col) == T] == 1, 
                                               median(data$no_col[data$weberco ==1], na.rm = T),
                                           ifelse(data$davisco[is.na(data$no_col) == T] == 1, 
                                               median(data$no_col[data$davisco == 1], na.rm = T), 
                                             median(data$no_col[data$other == 1], na.rm = T)))))

data$grad[is.na(data$grad) == T] <- ifelse(data$slco[is.na(data$grad) == T] == 1, 
                                           median(data$grad[data$slco == 1], na.rm = T), 
                                     ifelse(data$utco[is.na(data$grad) == T] == 1, 
                                           median(data$grad[data$utco ==1], na.rm =T),
                                      ifelse(data$weberco[is.na(data$grad) == T] == 1, 
                                           median(data$grad[data$weberco ==1], na.rm = T),
                                       ifelse(data$davisco[is.na(data$grad) == T] == 1, 
                                           median(data$grad[data$davisco == 1], na.rm = T), 
                                         median(data$grad[data$other == 1], na.rm = T)))))

data$MED_INC_CB[is.na(data$MED_INC_CB) == T] <- ifelse(data$slco[is.na(data$MED_INC_CB) == T] == 1, 
                                                       median(data$MED_INC_CB[data$slco == 1], na.rm = T), 
                                                 ifelse(data$utco[is.na(data$MED_INC_CB) == T] == 1, 
                                                       median(data$MED_INC_CB[data$utco ==1], na.rm =T),
                                                  ifelse(data$weberco[is.na(data$MED_INC_CB) == T] == 1, 
                                                       median(data$MED_INC_CB[data$weberco ==1], na.rm = T),
                                                   ifelse(data$davisco[is.na(data$MED_INC_CB) == T] == 1, 
                                                       median(data$MED_INC_CB[data$davisco == 1], na.rm = T), 
                                                    median(data$MED_INC_CB[data$other == 1], na.rm = T)))))


#### Creating the MLM specific data set ####

# MLM data prep section: droping columns that will not be used. dropping columns for reference purposes.
# gathering ethnicity into one column
mlm_data <- data %>% 
  gather("eth", "ind", 6:11) %>% 
  filter(ind == "Y") %>% 
  dplyr::select(-ind, -slco, -PRIOR_AP_CREDITS, -PRIOR_CLEP_CREDITS, -CUM_UG_TRANS_CREDITS)

# spliting year and term. will not use term but...
mlm_data$year <- as.factor(substr(mlm_data$TERM_CODE, 1, 4))
mlm_data$term <- substr(mlm_data$TERM_CODE, 5,6)

# only focusing on fall semester, > 17 and excluding years without complete data (too recent)
mlm_data <- mlm_data %>% 
  filter(term == "40") %>% 
  filter(AGE_ON_FIRST_DAY > 17) %>% 
  filter(year != "2017" & year != "2018")

#### setting reference for factor levels. This will make model interpretability easier ####
mlm_data$RESIDENCY_TYPE_CODE <- factor(mlm_data$RESIDENCY_TYPE_CODE, levels = c("N", "R", "U"))
mlm_data$MARITAL_STATUS <- factor(mlm_data$MARITAL_STATUS, levels = c("Unknown", "Divorced", "Married", "Separated", "Single", "Widowed"))

mlm_data$REG_STATUS <- factor(mlm_data$REG_STATUS,
                              levels = c("Non-Matriculated Student", 
                                         "Continuing Student",
                                         "First-Time Student Not within 12 Months of HS", 
                                         "First-Time Student within 12 Months of HS", 
                                         "High-School Student", 
                                         "Returning Student", 
                                         "Transfer-In Student"))

mlm_data$FULL_TIME <- factor(mlm_data$FULL_TIME, levels = c("Part-Time", "Full-Time"))
mlm_data$DISABILITY_IND <- factor(mlm_data$DISABILITY_IND, levels = c("U", "Y", "N"))
mlm_data$high_school_gpa <- factor(mlm_data$high_school_gpa, levels = c("NA", "A", "B", "C", "D", "F"))
mlm_data$prior_ug_gpa <- factor(mlm_data$prior_ug_gpa, levels = c("NA", "A", "B", "C", "D", "F"))
mlm_data$TERM_CTE_IND <- factor(mlm_data$TERM_CTE_IND, levels = c("U", "Y", "N"))
mlm_data$COLLEGE_READY_ENGLISH <- factor(mlm_data$COLLEGE_READY_ENGLISH, levels = c("U", "Y", "N"))
mlm_data$COLLEGE_READY_MATH <- factor(mlm_data$COLLEGE_READY_MATH, levels = c("U", "Y", "N"))
mlm_data$FIRST_GENERATION_IND <- factor(mlm_data$FIRST_GENERATION_IND, levels = c("U", "Y", "N" ))
mlm_data$DEGREE_SEEKING <- factor(mlm_data$DEGREE_SEEKING, levels = c("One-Year Degree", "Two-Year Degree", "Non-Degree-Seeking"))
mlm_data$RETAINED <- ifelse(mlm_data$RETAINED == "Y", 1, 0)


################# MLM variable selection/filter #################
# limiting glmer to just first time students: First-Time Student Not within 12 Months of HS | First-Time Student within 12 Months of HS
mlm_data <- mlm_data %>% 
  filter(REG_STATUS == "First-Time Student Not within 12 Months of HS" | REG_STATUS == "First-Time Student within 12 Months of HS") %>%
  filter(RESIDENCY_TYPE_CODE != "U") %>%
  dplyr::select(RETAINED, GENDER, MARITAL_STATUS, VETERAN_IND, VETERAN_DEPENDENT_IND, REFUGEE_IND, TEXT_OK_IND,
                COLLEGE_READY_MATH, COLLEGE_READY_ENGLISH, RESIDENCY_TYPE_CODE, DEGREE_SEEKING, EVER_CONCURRENT_IND,
                FIRST_GENERATION_IND, PRIOR_UG_CREDITS, FULL_TIME, PELL_ELIGIBLE_IND, HS_ONE_YEAR_IND,
                CONTINUING_STUDENT_IND, FIRST_TERM_NON_CONCURRENT_IND, FIRST_TERM_EVER_IND, MED_INC_CB, utco,
                weberco, davisco, other, high_school_gpa, prior_ug_gpa, term_ug_gpa, bs_pop, AGE_ON_FIRST_DAY,
                TERM_CTE_IND, eth, TERM_MAJOR, year) #only 1 U for RESISDENCY_TYPE_CODE

mlm_data_table <- mlm_data %>% group_by(year) %>% tally()

# create data partition
set.seed(1983)
mlm_data_tt_split <- createDataPartition(mlm_data$RETAINED, 
                                         p = .8, 
                                         list = F,
                                         times = 1)

mlm_data_train <- mlm_data[mlm_data_tt_split,]
mlm_data_test <- mlm_data[-mlm_data_tt_split,]

################# MLM RUN #################

ret_glmer <- glmer(RETAINED ~ GENDER + MARITAL_STATUS + VETERAN_IND + VETERAN_DEPENDENT_IND + REFUGEE_IND + 
                     TEXT_OK_IND + COLLEGE_READY_MATH + COLLEGE_READY_ENGLISH + 
                     RESIDENCY_TYPE_CODE + DEGREE_SEEKING + EVER_CONCURRENT_IND + FIRST_GENERATION_IND + PRIOR_UG_CREDITS + 
                     FULL_TIME + PELL_ELIGIBLE_IND + HS_ONE_YEAR_IND + CONTINUING_STUDENT_IND + 
                     FIRST_TERM_NON_CONCURRENT_IND + FIRST_TERM_EVER_IND + scale(MED_INC_CB) + utco + weberco + davisco +
                     other + high_school_gpa + prior_ug_gpa + term_ug_gpa + bs_pop + scale(AGE_ON_FIRST_DAY) +
                     TERM_CTE_IND +
                     (1|year) + (1|eth) + (1|TERM_MAJOR), family = binomial, data = mlm_data_train)


################# MLM RESULTS #################

summary(ret_glmer)

################# RANDOM EFFECT: PROGRAM #################
rr1 <- ranef(ret_glmer, condVar = T)

rr2 <- data.frame(PROGRAM = rownames(rr1[[1]]),
                  int=unname(rr1[[1]]),
                  se=sqrt(c(attr(rr1[[1]], "postVar"))))
#rr3 <- merge(rr2,dd)
rr4 <- transform(rr2, PROGRAM = reorder(PROGRAM, int))
program_ret <- rr4 %>% 
  ggplot(aes(PROGRAM, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  coord_flip()

################# RANDOM EFFECT: ETHNICITY #################
rr1_eth <- ranef(ret_glmer, condVar = T)
rr2_eth <- data.frame(eth = rownames(rr1_eth[[3]]),
                      int = unname(rr1_eth[[3]]),
                      se=sqrt(c(attr(rr1_eth[[3]], "postVar"))))
rr4_eth <- transform(rr2_eth, eth = reorder(eth, int))

eth_ret <- rr4_eth  %>% 
  ggplot(aes(eth, exp(int), ymin=exp(int-1.96*se), ymax=exp(int+1.96*se))) + 
  geom_pointrange() + 
  coord_flip()


################# RANDOM EFFECT: YEAR #################
rr2_year <- data.frame(year = rownames(rr1[[2]]),
                       int = unname(rr1[[2]]),
                       se = sqrt(c(attr(rr1[[2]], "postVar"))))

year_ret <- rr2_year %>% 
  ggplot(aes(year, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange()

##### assessing the predictive performance of MLM

# ret_glmer_pred <- predict(ret_glmer, mlm_data_test, allow.new.levels = T, type = "response")
# confusionMatrix(ret_glmer_pred > 0.5, mlm_data_test$RETAINED)
