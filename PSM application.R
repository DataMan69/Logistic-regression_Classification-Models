library(brglm2)
library(brglm2)
library(detectseparation)
library(MatchIt)
library(cobalt)
library(dplyr)
library(ggplot2)
library(rlemon)
library(Formula)
library(Matching)

####################### Data  file calling ########################################################################################################################################



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


m.out <- matchit(trtstatus~Age+agesq+Farm_Experience+Goat_Experience+Schooling_Years+
                   PO_Farming+PO_Goat_Farming+SO_Farming+SO_Goat_Farming+Dwelling_Pucca+Dwelling_Semi_Pucca+
                   Household_Size+Land_Holding_Ha+Herd_Size, 
                 data = goat_data, 
                 method = "optimal", 
                 distance = "probit", 
                 bw = 0.1)
summary(m.out)
love.plot(m.out, binary = "std")






################ chatGPT territory#########
# Add propensity scores to goat_data
goat_data$pscore <- m.out$distance

# Identify common support range (controls only)
min_control <- min(goat_data$pscore[trtstatus == 0])
max_control <- max(goat_data$pscore[trtstatus == 0])

# Flag treated/control & on/off support
goat_data <- goat_data %>%
  mutate(on_support = pscore >= min_control & pscore <= max_control,
         group = case_when(
           trtstatus == 0 ~ "Untreated",
           trtstatus == 1 & on_support ~ "Treated: On support",
           trtstatus == 1 & !on_support ~ "Treated: Off support"
         ))

# Bin scores to create histogram bars
data_bins <- goat_data %>%
  mutate(bin = cut(pscore, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(bin, group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(count = ifelse(group == "Untreated", -count, count))  # Flip untreated bars

# Plot
ggplot(data_bins, aes(x = bin, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("Untreated" = "blue",
                               "Treated: On support" = "brown",
                               "Treated: Off support" = "green")) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() +
  labs(title = "Propensity Score Common Support",
       x = "Propensity Score", y = NULL, fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################################################


summary(m.out, standardize = TRUE)

MilkYield<-goat_data$Milk.Yield..kg.Annum.

matched.data <- match.data(m.out)
t.test(MilkYield ~ trtstatus, data = matched.data)



get_ATT <- function(matchit_obj, outcome_var, treat_var = "trtstatus") {
  matched.data <- MatchIt::match.data(matchit_obj)
  
  treat <- matched.data[[treat_var]]
  outcome <- matched.data[[outcome_var]]
  wts <- matched.data$weights
  
  # Weighted means
  mean_treat <- weighted.mean(outcome[treat == 1], wts[treat == 1])
  mean_control <- weighted.mean(outcome[treat == 0], wts[treat == 0])
  ATT <- mean_treat - mean_control
  
  # Weighted variances
  var_treat <- sum(wts[treat == 1] * (outcome[treat == 1] - mean_treat)^2) /
    (sum(wts[treat == 1]) - 1)
  var_control <- sum(wts[treat == 0] * (outcome[treat == 0] - mean_control)^2) /
    (sum(wts[treat == 0]) - 1)
  
  # Standard error for difference in means
  se <- sqrt(var_treat / sum(wts[treat == 1]) +
               var_control / sum(wts[treat == 0]))
  
  # t-statistic and p-value
  t_stat <- ATT / se
  df <- length(outcome) - 2
  p_val <- 2 * pt(-abs(t_stat), df)
  
  # 95% confidence interval
  ci_lower <- ATT - qt(0.975, df) * se
  ci_upper <- ATT + qt(0.975, df) * se
  
  list(
    ATT = ATT,
    mean_treat = mean_treat,
    mean_control = mean_control,
    t_statistic = t_stat,
    p_value = p_val,
    conf_int = c(ci_lower, ci_upper)
  )
}

# Example:
result <- get_ATT(m.out, outcome_var = "Milk.Yield..kg.Annum.", treat_var = "Treatment.Status")
print(result)


####################################More Values



get_ATT <- function(matchit_obj, outcome_var, treat_var = "trtstatus") {
  matched.data <- MatchIt::match.data(matchit_obj)
  
  treat <- matched.data[[treat_var]]
  outcome <- matched.data[[outcome_var]]
  wts <- matched.data$weights
  
  # Basic counts
  n_treat <- sum(treat == 1)
  n_control <- sum(treat == 0)
  n_treat_w <- sum(wts[treat == 1])
  n_control_w <- sum(wts[treat == 0])
  
  # Weighted means
  mean_treat_w <- weighted.mean(outcome[treat == 1], wts[treat == 1])
  mean_control_w <- weighted.mean(outcome[treat == 0], wts[treat == 0])
  
  # Unweighted means
  mean_treat_unw <- mean(outcome[treat == 1])
  mean_control_unw <- mean(outcome[treat == 0])
  
  # ATT
  ATT <- mean_treat_w - mean_control_w
  
  # Weighted variances
  var_treat_w <- sum(wts[treat == 1] * (outcome[treat == 1] - mean_treat_w)^2) /
    (n_treat_w - 1)
  var_control_w <- sum(wts[treat == 0] * (outcome[treat == 0] - mean_control_w)^2) /
    (n_control_w - 1)
  
  # Standard error
  se <- sqrt(var_treat_w / n_treat_w +
               var_control_w / n_control_w)
  
  # t-statistic, df, p-value
  t_stat <- ATT / se
  df <- length(outcome) - 2
  p_val <- 2 * pt(-abs(t_stat), df)
  
  # 95% confidence interval
  ci_lower <- ATT - qt(0.975, df) * se
  ci_upper <- ATT + qt(0.975, df) * se
  
  list(
    ATT = ATT,
    mean_treat_weighted = mean_treat_w,
    mean_control_weighted = mean_control_w,
    mean_treat_unweighted = mean_treat_unw,
    mean_control_unweighted = mean_control_unw,
    var_treat_weighted = var_treat_w,
    var_control_weighted = var_control_w,
    n_treat_unweighted = n_treat,
    n_control_unweighted = n_control,
    n_treat_weighted = n_treat_w,
    n_control_weighted = n_control_w,
    standard_error = se,
    t_statistic = t_stat,
    degrees_freedom = df,
    p_value = p_val,
    conf_int_95 = c(lower = ci_lower, upper = ci_upper)
  )
}

# Example:
result <- get_ATT(m.out, outcome_var = "Milk.Yield..kg.Annum.", treat_var = "Treatment.Status")
print(result)


























ps_model <- glm(trtstatus ~ Age + agesq + Farm_Experience + Goat_Experience +
                  Schooling_Years + PO_Farming + PO_Goat_Farming + SO_Farming +
                  SO_Goat_Farming + Dwelling_Pucca + Dwelling_Semi_Pucca +
                  Household_Size + Land_Holding_Ha + Herd_Size,
                family = binomial(link = "probit"),
                data = goat_data)

goat_data$pscore <- ps_model$fitted.values

match.out <- Match(Y = goat_data$Milk.Yield..kg.Annum.,
                   Tr = goat_data$Treatment.Status,
                   X = goat_data$pscore,
                   M = 1, Weight = 2)
summary(match.out)



























assess_matching_quality <- function(matchit_obj, data) {
  
  # Your covariate list (matching Ravi file variable names)
  covariate_list <- c("Age..years.",
                      "agesq",
                      "Farm.Experience..Years.",
                      "Experience.in.rearing.of.Goats..Years.",
                      "No.of.Schooling.Years",
                      "Gender",
                      "PO_Farming",
                      "PO_Goat.Farming",
                      "SO_Farming",
                      "SO_Goat.Farming",
                      "Dwelling.Structure_Pucca",
                      "Dwelling.Structure_Semi.Pucca",
                      "Household.Size",
                      "Land.holding.size..Ha.",
                      "Herd_Size",
                      "Mkt_Distance_KM",
                      "AHS_Distance_KM")
  
  treat_var <- "Treatment.Status"
  
  # 1. Balance summary
  cat("=== Balance Summary (Standardized Differences) ===\n")
  print(summary(matchit_obj, standardize = TRUE))
  
  # 2. Formal balance test
  matched.data <- match.data(matchit_obj)
  
  cov_formula <- as.formula(
    paste(treat_var, "~", paste(covariate_list, collapse = " + "))
  )
  
  cat("\n=== Formal Balance Test ===\n")
  Matching::MatchBalance(cov_formula, 
                         match.out = list(
                           Tr = matched.data[[treat_var]],
                           X = matchit_obj$distance,
                           Weight = matched.data$weights
                         ), 
                         data = matched.data)
  
  # 3. Common support plot
  min_control <- min(matchit_obj$distance[matched.data[[treat_var]] == 0])
  max_control <- max(matchit_obj$distance[matched.data[[treat_var]] == 0])
  
  matched.data <- matched.data %>%
    mutate(on_support = distance >= min_control & distance <= max_control,
           group = case_when(
             .data[[treat_var]] == 0 ~ "Untreated",
             .data[[treat_var]] == 1 & on_support ~ "Treated: On support",
             .data[[treat_var]] == 1 & !on_support ~ "Treated: Off support"
           ))
  
  data_bins <- matched.data %>%
    mutate(bin = cut(distance, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
    group_by(bin, group) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(count = ifelse(group == "Untreated", -count, count))
  
  ggplot(data_bins, aes(x = bin, y = count, fill = group)) +
    geom_bar(stat = "identity", position = "identity") +
    geom_hline(yintercept = 0, color = "black") +
    scale_fill_manual(values = c("Untreated" = "blue",
                                 "Treated: On support" = "brown",
                                 "Treated: Off support" = "green")) +
    theme_minimal() +
    labs(title = "Propensity Score Common Support",
         x = "Propensity Score", y = NULL, fill = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Make agesq in your data before running
goat_data$agesq <- goat_data$Age..years.^2

# Run assessment
assess_matching_quality(m.out, goat_data)


























library(MatchIt)
library(cobalt)
library(dplyr)
library(ggplot2)

# ===================== Matching =====================
m.out <- matchit(Treatment.Status ~ Age..years. + I(Age..years.^2) +
                   Farm.Experience..Years. + Experience.in.rearing.of.Goats..Years. +
                   No.of.Schooling.Years + PO_Farming + PO_Goat.Farming +
                   SO_Farming + SO_Goat.Farming +
                   Dwelling.Structure_Pucca + Dwelling.Structure_Semi.Pucca +
                   Household.Size + Land.holding.size..Ha. + Herd_Size,
                 data = goat_data,
                 method = "optimal",
                 distance = "probit",
                 bw = 0.1)

# ===================== 1. Balance Check =====================
cat("\n=== Balance Summary ===\n")
print(summary(m.out, standardize = TRUE))
love.plot(m.out, binary = "std")

# ===================== 2. Common Support Check =====================
matched_data <- match.data(m.out)

min_c <- min(matched_data$distance[matched_data$Treatment.Status == 0])
max_c <- max(matched_data$distance[matched_data$Treatment.Status == 0])

matched_data <- matched_data %>%
  mutate(on_support = !(Treatment.Status == 1 &
                          (distance < min_c | distance > max_c)))

off_support_count <- sum(!matched_data$on_support & matched_data$Treatment.Status == 1)
cat("Treated units off common support:", off_support_count, "\n")

# Keep only on-support observations
matched_data <- matched_data %>% filter(on_support)

# ===================== 3. Outcome Overlap Check =====================
boxplot(Milk.Yield..kg.Annum. ~ Treatment.Status, data = matched_data,
        main = "Milk Yield by Treatment (Post-Match)",
        xlab = "Treatment Status", ylab = "Milk Yield (kg/Annum)")

# ===================== 4. ATT (Direct Calculation) =====================
ATT <- with(matched_data, 
            weighted.mean(Milk.Yield..kg.Annum.[Treatment.Status == 1], weights[Treatment.Status == 1]) -
              weighted.mean(Milk.Yield..kg.Annum.[Treatment.Status == 0], weights[Treatment.Status == 0]))

cat("ATT (Direct) =", ATT, "\n")

# ===================== 5. ATT via Linear Regression =====================
reg_model <- lm(Milk.Yield..kg.Annum. ~ Treatment.Status,
                data = matched_data,
                weights = weights)
summary(reg_model)

cat("\nLinear Regression ATT =", coef(reg_model)["Treatment.Status"], "\n")



png("milk_yield_boxplot.png", width = 800, height = 600)
boxplot(Milk.Yield..kg.Annum. ~ Treatment.Status, data = matched_data,
        main = "Milk Yield by Treatment (Post-Match)",
        xlab = "Treatment Status", ylab = "Milk Yield (kg/Annum)")
dev.off()

