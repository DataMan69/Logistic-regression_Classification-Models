library(brglm2)
library(brglm2)
library(psych)
library(detectseparation)
file_path <- "D:/Indian Veterinary Research Institute M.Sc. Agrcultural Statistics/Thesis Work/Interviews/Collapsed_CSV - Copy.csv"
goat_data_raw <- read.csv(file_path, header = TRUE)
goat_data<-data.frame(goat_data_raw)

############################################################################################################################################################################################################################
###############   Variable Declaration

trtstatus<-goat_data$Treatment.Status
Age<-goat_data$Age..years.
agesq<-Age^2
Farm_Experience<-goat_data$Farm.Experience..Years.
Goat_Experience<-goat_data$Experience.in.rearing.of.Goats..Years.
Schooling_Years<-goat_data$No.of.Schooling.Years
Gender<-goat_data$Gender
PO_Farming<-goat_data$PO_Farming
PO_Goat_Farming<-goat_data$PO_Goat.Farming
SO_Farming<-goat_data$SO_Farming
SO_Goat_Farming<-goat_data$SO_Goat.Farming
Dwelling_Semi_Pucca<-goat_data$Dwelling.Structure_Semi.Pucca
Dwelling_Pucca<-goat_data$Dwelling.Structure_Pucca
Household_Size<-goat_data$Household.Size
Land_Holding_Ha<-goat_data$Land.holding.size..Ha.
Herd_Size<-goat_data$Herd_Size
Mkt_Distance_KM<-goat_data$Mkt_Distance_KM
AHS_Distance_KM<-goat_data$AHS_Distance_KM

######################################################################################################################################################################################################################################################################################################################################

###Full Model
modelfull<-glm(trtstatus ~ Age + agesq + Farm_Experience + Goat_Experience +
  Schooling_Years + Gender + PO_Farming + PO_Goat_Farming +
  SO_Farming + SO_Goat_Farming + Dwelling_Semi_Pucca +
  Dwelling_Pucca + Household_Size + Land_Holding_Ha +
  Herd_Size + Mkt_Distance_KM + AHS_Distance_KM,
data = goat_data,
family = binomial(link = "logit"),method="brglmFit")
coef_summary <- summary(modelfull)$coefficients
significance <- ifelse(coef_summary[,4] < 0.05, "Yes", "No")
results <- data.frame(Variable = rownames(coef_summary), Estimate = coef_summary[,1], Std_Error = coef_summary[,2], z_value = coef_summary[,3], p_value = coef_summary[,4], Significant = significance)
print(results)

#######################################################################33333333333333333333####################################################################################################################################################################################################################

###Full Model except AHS & gender
model1<-glm(trtstatus ~ Age + agesq + Farm_Experience + Goat_Experience +
                 Schooling_Years + PO_Farming + PO_Goat_Farming +
                 SO_Farming + SO_Goat_Farming + Dwelling_Semi_Pucca +
                 Dwelling_Pucca + Household_Size + Land_Holding_Ha +
                 Herd_Size,
               data = goat_data,
               family = binomial(link = "logit"),method="brglmFit")
coef_summary <- summary(model1)$coefficients
significance <- ifelse(coef_summary[,4] < 0.05, "Yes", "No")
results <- data.frame(Variable = rownames(coef_summary), Estimate = coef_summary[,1], Std_Error = coef_summary[,2], z_value = coef_summary[,3], p_value = coef_summary[,4], Significant = significance)
print(results)
#########################################3333333333333333333###########################################################################################################################################################################################################################################################
#Actual model
model3 <- glm(trtstatus ~ Age + agesq+ Farm_Experience + Goat_Experience + Schooling_Years +
               Household_Size + Land_Holding_Ha + Herd_Size + PO_Farming + PO_Goat_Farming +
                SO_Farming + SO_Goat_Farming + Dwelling_Semi_Pucca+ Dwelling_Pucca 
               , data = goat_data, 
             family = binomial, method = "brglmFit")

coef_summary <- summary(model3)$coefficients
significance <- ifelse(coef_summary[,4] < 0.05, "Yes", "No")
results <- data.frame(Variable = rownames(coef_summary), Estimate = coef_summary[,1], Std_Error = coef_summary[,2], z_value = coef_summary[,3], p_value = coef_summary[,4], Significant = significance)
print(results)

################################################################################################################################################################################################################################################################################################################
####Second model


model4 <- glm(trtstatus ~ Age + Farm_Experience + Goat_Experience +
                Household_Size + Land_Holding_Ha+ Herd_Size, data = goat_data, 
              family = binomial, method = "brglmFit")

coef_summary <- summary(model4)$coefficients
significance <- ifelse(coef_summary[,4] < 0.05, "Yes", "No")
results <- data.frame(Variable = rownames(coef_summary), Estimate = coef_summary[,1], Std_Error = coef_summary[,2], z_value = coef_summary[,3], p_value = coef_summary[,4], Significant = significance)
print(results)




#########################################################################################################################################################################################################################################################################################################################################################3

firth_model <- glm(
  trtstatus ~ Age + agesq + Farm_Experience + Goat_Experience + Schooling_Years + PO_Farming + PO_Goat_Farming + SO_Farming + SO_Goat_Farming +
    Dwelling_Semi_Pucca + Dwelling_Pucca + Household_Size + Land_Holding_Ha +Herd_Size,
  data = goat_data,
  family = binomial("logit"),
  method = "brglmFit",  # Firth bias reduction
  type = "AS_mean"      # Firth correction (only this part different from all others)
)

# Check model summary
summary(firth_model)


################################################################################################################################################################################################################







normalmod<-glm(formula=trtstatus~Age+agesq+Farm_Experience+Goat_Experience+Schooling_Years+
  PO_Farming+PO_Goat_Farming+SO_Farming+SO_Goat_Farming+Dwelling_Pucca+Dwelling_Semi_Pucca+
  Household_Size+Land_Holding_Ha+Herd_Size, 
data = goat_data, family=binomial())

summary(normalmod)



