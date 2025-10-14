########## Load Data ################

library(tidymodels)
library(rio)
DataMockup=import("https://ai.lange-analytics.com/data/DataStudyTimeMockup.rds")

########## Script 100 #############################

########## Run OLS as Reference  ################## 

RecipeMockup= recipe(Grade~StudyTime, data=DataMockup)

ModelDesignOLS= linear_reg() %>% 
                set_engine("lm") %>% 
                set_mode("regression")


WFModelGrades = workflow() %>%  
                add_recipe(RecipeMockup) %>% 
                add_model(ModelDesignOLS) %>% 
                fit(DataMockup)

print(WFModelGrades)



######## Define MSE Command ###########

FctMSE=function(VecBeta, data){
  Beta1=VecBeta[1]
  Beta2=VecBeta[2]
  data=data %>%
    rename(Y=1, X=2) %>% 
    mutate(YPred=Beta1*X+Beta2) %>%
    mutate(Error=YPred-Y) %>%
    mutate(ErrorSqt=Error^2)
  
  MSE=mean(data$ErrorSqt)
  
  return(MSE)}


##### Use MSE Command ####

# Opt Betas were b1: 3.963 and beta2: 64.179

FctMSE(c(3.963, 64.179), DataMockup)
# Try different betas


##### Find optimal betas with GridSearch (brute force)

VecBeta1Values=seq(3, 5, length.out=21)
VecBeta2Values=seq(50, 70, length.out=21)

print(VecBeta1Values)
print(VecBeta2Values)

GridBetaPairs=expand.grid(Beta1=VecBeta1Values, Beta2=VecBeta2Values)

VecMSE=apply(GridBetaPairs, FUN=FctMSE, data=DataMockup, MARGIN=1)# margin=1 means use rows to apply

GridBetaPairs=GridBetaPairs %>% 
  mutate(MSE=VecMSE)

##### Find optimal betas with Optimizer (build in R function)

StartForBeta1Beta2=c(0,83) # 83 is the mean grade

BestBetas=optim(StartForBeta1Beta2, FctMSE, data=DataMockup)

print(BestBetas$par)
