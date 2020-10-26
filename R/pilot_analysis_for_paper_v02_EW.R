# used for the preparation of the pilot study paper



# Setup -------------------------------------------------------------------

library(data.table)

create_results_table <- FALSE


if(create_results_table == TRUE) {
  file_date <- "2020-10-15"
  file_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Onboarding/"
  filename_start <- "Onboarding_Report_"
  
  all_onboards <- fread(paste(file_location,
                              filename_start, 
                              file_date, 
                              ".csv", 
                              sep = ""),
                        fill = TRUE)
  
  all_onboards[, Consent_end_date := as.Date(Consent_end_date, 
                                             format = "%d/%m/%Y")]
  
  pilots <- all_onboards[Trial_ID == "Wave1-Pilot" & 
                           (Consent_end_date > "2019-11-01" | is.na(Consent_end_date)), ]
  
  consents <- pilots[, .(REGION,
                         IMD,
                         CELL,
                         CONSENT_SOURCE)]
  setnames(consents, 
           c("REGION", "IMD", "CELL", "CONSENT_SOURCE"), 
           c("region", "quintile", "cell", "consent_source"))
  
  
  
  all_contacted <- fread("S:/ENERGINST_EaB_Project_17_SMRP/Data/Researcher data/Pilot/full_addresses_pilot.csv")
  
  summary_contacted <- all_contacted[, .N, keyby = .(region, quintile, cell)]
  
  summary_consents <- consents[, .N, keyby = .(region, quintile, cell)]
  setnames(summary_consents, "N", "consents")
  
  summary_contacted <- summary_consents[summary_contacted]
  summary_contacted[is.na(consents), consents := 0]
  summary_contacted[, refusals := N - consents]
  
  
  results <- consents
  for(i in 1:nrow(summary_contacted)) {
    r <- summary_contacted[i, refusals]
    results <- rbind(results,
                     data.table(region = rep(summary_contacted[i, region], r),
                                quintile = rep(summary_contacted[i, quintile], r),
                                cell = rep(summary_contacted[i, cell], r),
                                consent_source = rep("None", r)
                     )
    )
  }
  
  results[cell == 1, `:=`(p2w = TRUE, version = as.factor(1), incentive = "None")]
  results[cell == 2, `:=`(p2w = TRUE, version = as.factor(1), incentive = "Voucher")]
  results[cell == 3, `:=`(p2w = TRUE, version = as.factor(1), incentive = "Thermometer")]
  results[cell == 4, `:=`(p2w = TRUE, version = as.factor(2), incentive = "None")]
  results[cell == 5, `:=`(p2w = TRUE, version = as.factor(2), incentive = "Voucher")]
  results[cell == 6, `:=`(p2w = TRUE, version = as.factor(2), incentive = "Thermometer")]
  results[cell == 7, `:=`(p2w = FALSE, version = as.factor(1), incentive = "None")]
  results[cell == 8, `:=`(p2w = FALSE, version = as.factor(1), incentive = "Voucher")]
  results[cell == 9, `:=`(p2w = FALSE, version = as.factor(1), incentive = "Thermometer")]
  results[cell == 10, `:=`(p2w = FALSE, version = as.factor(2), incentive = "None")]
  results[cell == 11, `:=`(p2w = FALSE, version = as.factor(2), incentive = "Voucher")]
  results[cell == 12, `:=`(p2w = FALSE, version = as.factor(2), incentive = "Thermometer")]
  
  results[, consent := TRUE]
  results[consent_source == "None", consent := FALSE]
  
  save(results, 
       file = "C:/Users/Ellen Webborn/University College London/SERL Consortium - General/SERL UCL-Essex folders/Participant Recruitment/Pilot Phase/Reports/Pilot study paper/Code/results18000.RData")
  
} else {load("C:/Users/Ellen Webborn/University College London/SERL Consortium - General/SERL UCL-Essex folders/Participant Recruitment/Pilot Phase/Reports/Pilot study paper/Code/results18000.RData")}




# logit models -------------------------------------------------------------

# IMD
responseModelImd <- glm(formula = consent ~
                          #region + 
                          as.factor(quintile),
                        family = binomial(logit), results)
summary(responseModelImd)

responseModelImdCell <- glm(formula = consent ~
                              #region + 
                              as.factor(quintile) + 
                              as.factor(cell),
                            family = binomial(logit), results)
summary(responseModelImdCell)

# IMD and region
responseModelImdRegion <- glm(formula = consent ~
                                as.factor(region) + 
                                as.factor(quintile),
                              family = binomial(logit), results)
summary(responseModelImdRegion)

# Incentives
responseModelIncentive <- glm(formula = consent ~
                                p2w,
                              family = binomial(logit), results)
summary(responseModelIncentive)

exp(cbind(OR = coef(responseModelIncentive), confint(responseModelIncentive)))

# Push to web
responseModelP2w <- glm(formula = consent ~
                          incentive,
                        family = binomial(logit), results)
summary(responseModelP2w)

exp(cbind(OR = coef(responseModelP2w), confint(responseModelP2w)))


# Content version
responseModelVersion <- glm(formula = consent ~
                              relevel(version, ref = "2"),
                            family = binomial(logit), results)
summary(responseModelVersion)

exp(cbind(OR = coef(responseModelVersion), confint(responseModelVersion)))


# IMD-region logit in detail --------------------------------------------------------

responseModelImdRegion <- glm(formula = consent ~
                                as.factor(region) + 
                                as.factor(quintile),
                              family = binomial(logit), results)
summary(responseModelImdRegion)

# robust SE because we may? have clusters within OAs/LSOAs (ought to use a multi-level model really)
rob.responseModelImdRegion <- lmtest::coeftest(noContactModel1, function(x) plm::vcovHC(x, type="HC0")) # requires lmtest & plm

rob.responseModelImdRegion

# try stargazer() for nice regression model outputs

# diagnostics:
library(car)

# use confint to report confidence intervals with bonferroni corrected level 
bc_p <- 0.05/length(responseModelImdRegion$coefficients)
confint(responseModelImdRegion, level = 1 - bc_p)

# save results as log odds
# the cbind function simply 'glues' the columns together side by side 
responseModelImdRegionLO <- cbind(LogOdds = coef(responseModelImdRegion), 
                                  confint(responseModelImdRegion, level = 1 - bc_p))

# convert the log odds given by summary() to odds ratios (easier to understand) # combine the results and save 
responseModelImdRegionOR <- exp(cbind(OddsRatio = coef(responseModelImdRegion), 
                                      confint(responseModelImdRegion, level = 1 - bc_p)))


# Diagnostics:  ----
# Independence of errors
car::durbinWatsonTest(responseModelImdRegion)
# if p < 0.05 then a problem as implies autocorrelation
#  lag Autocorrelation D-W Statistic p-value
#  1       0.9973258   0.004971946       0
#  Alternative hypothesis: rho != 0

# Collinearity (vif)
car::vif(responseModelImdRegion)
# if any values > 10 -> problem
#                         GVIF Df GVIF^(1/(2*Df))
# as.factor(region)   1.086677  6        1.006951
# as.factor(quintile) 1.086677  4        1.010445

# Collinearity (tolerance)
1/car::vif(responseModelImdRegion)
# if any values < 0.2 -> possible problem 
# if any values < 0.1 -> definitely a problem
#                         GVIF        Df GVIF^(1/(2*Df))
# as.factor(region)   0.9202364 0.1666667       0.9930969
# as.factor(quintile) 0.9202364 0.2500000       0.9896632

# Diagnostic plots ----
plot(responseModelImdRegion)
# requires library(car)
car::spreadLevelPlot(responseModelImdRegion)
# Suggested power transformation:  0.4744183 



# Figures ---------------------------------------

# IMD

p.imd <- ggplot(data = signupResponse[is.na(Treatment_type) & Split_by == "IMD"], 
                aes(x = Category, y = Percent, fill = as.factor(Category))) +
  geom_bar(stat = "identity", 
           position = position_dodge()) + 
  geom_text(aes(label = paste(Percent, "%", sep = "")), 
            vjust = 3, 
            size = geom.text.size) +
  labs(x = "IMD quintile (1 is greatest deprivation)", 
       y = "Response rate (%)", 
       fill = "IMD")  +
  scale_fill_brewer(palette = "RdYlBu") +
  theme(text = element_text(size = font.size), 
        legend.position = c(0.1, 0.8))


# Region push to web

p2w_consentReg <- results[consent == TRUE, .N, keyby = .(region, p2w)]
p2w_all <- results[, .N, keyby = .(region, p2w)]
p2w_consentReg <- p2w_consentReg[p2w_all]
setnames(p2w_consentReg, old = c("N", "i.N"), new = c("consents", "startingN"))
p2w_consentReg[, perc := consents / startingN * 100]
p2w_consentReg[, lowerCI := perc - 100 *1.96 * sqrt(perc/100 * (1-perc/100) / startingN)]
p2w_consentReg[, upperCI := perc + 100 *1.96 * sqrt(perc/100 * (1-perc/100) / startingN)]

# IMD push to web

p2w_consentIMD <- results[consent == TRUE, .N, keyby = .(quintile, p2w)]
p2w_all <- results[, .N, keyby = .(quintile, p2w)]
p2w_consentIMD <- p2w_consentIMD[p2w_all]
setnames(p2w_consentIMD, old = c("N", "i.N"), new = c("consents", "startingN"))
p2w_consentIMD[, perc := consents / startingN * 100]
p2w_consentIMD[, lowerCI := perc - 100 *1.96 * sqrt(perc/100 * (1-perc/100) / startingN)]
p2w_consentIMD[, upperCI := perc + 100 *1.96 * sqrt(perc/100 * (1-perc/100) / startingN)]
