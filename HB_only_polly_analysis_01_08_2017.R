## Pollinator models Himalayan Balsam (Impatiens glandulifera) only 
## Ruth Kelly (kellyr44@tcd.ie) 01/08/2017 
## For "Contrasting impacts of high priority invasive plant species on pollinating insect communities"
## Based on data collected by Emily Davis, study design by Emily Davis, Jane Stout and Christine Maggs



####  Part A: GLMMS #### 
HB<- read.csv("HB_with_flower_data_vr1.csv")

str(HB)

plot(HB[,1:10])

table(HB$River,HB$Inv_status)

# # #              Invaded Native
# Coalisland       4      4
# Glenmornan       2      2
# Lagan            4      4
# Roe              4      4
table(HB$Session,HB$Inv_status)

#   Invaded Native
# A       7      7
# B       7      7


names(HB)
#  [1] "Site"                            "Species"                        
# [3] "Species_control"                 "Session"                        
# [5] "River"                           "Site_ID"                        
# [7] "Date_of_Trap"                    "Inv_status"                     
# [9] "IAS_._Cover"                     "Open_Inf"                       
# [11] "SR_flowering_per_10m"            "Unique_currently_flowering_100m"
# [13] "B_lucorum_agg"                   "B_pratorum"                    
# [15] "B_pascuorum"                     "B_hortorum"   
# ...
# [57] "Eupeodes_nitens"                 "Myathropa_florea"               
# [59] "Pipiza_fenestrata"               "Chalcosyrphus_nemorum"          
# [61] "Baccha_elongata"                 "Xylota_segnis"  


summary(HB[,1:12])

##########

#insect species
Spe <- HB[,13:62]
str(Spe)
# abundance
Ab <- rowSums(Spe)

#richness of pollinators
#rich1 <- rowSums(Spe>0)

Shann <- diversity(Spe)

###########

Ab_all_gaus <- glmmadmb(Ab ~ Species_control
                        + (1|River/Session), 
                        data = HB, 
                        family = "gaussian")

hist(Ab_all_gaus$residuals)
shapiro.test(Ab_all_gaus$residuals)

# #Shapiro-Wilk normality test
# 
# data:  Ab_all_gaus$residuals
# W = 0.8504, p-value = 0.0009521

Ab_all_pois <- glmmadmb(Ab~ Species_control + 
                          (1|River/Session) , 
                        data = HB, 
                        family = "poisson")

###

AIC(Ab_all_pois)
# [1] 383.896

Ab_all_nbinom <- glmmadmb(Ab ~ Species_control + 
                          (1|River/Session), 
                        data = HB, 
                        family = "nbinom")

AIC(Ab_all_nbinom)
# [1] 213.618

summary(Ab_all_nbinom)

# glmmadmb(formula = Ab ~ Species_control + (1 | River/Session), 
#          data = HB, family = "nbinom")
# 
# AIC: 213.6 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          2.155      0.470    4.59  4.5e-06 ***
#   Species_controlHB    0.362      0.392    0.92     0.36    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8 



Ab_Coef_results1 <- data.frame(Species = "HB",
                                     Coefficient = coef(Ab_all_nbinom)[2],
                                     Confint_L = confint(Ab_all_nbinom)[2,1],
                                     Confint_U = confint(Ab_all_nbinom)[2,2],
                                     SE = 0.392,
                                    Pvalue = 0.360,
                                     Family = "Negative binomial",
                                     modelName = "Total pollinator abundance")

########### Diversity of all pollinators..

library("lme4")

Shann_all_gaus <- lmer(Shann ~ Species_control
                       + (1|River/Session), 
                       data = HB)

shapiro.test(residuals(Shann_all_gaus))

# Shapiro-Wilk normality test
# 
# data:  residuals(Shann_all_gaus)
# W = 0.97045, p-value = 0.5926

### normal

summary(Shann_all_gaus)

# Linear mixed model fit by REML ['lmerMod']
# Formula: Shann ~ Species_control + (1 | River/Session)
# Data: HB
# 
# REML criterion at convergence: 55.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7886 -0.7070  0.0870  0.7651  1.6947 
# 
# Random effects:
#   Groups        Name        Variance  Std.Dev. 
# Session:River (Intercept) 1.159e-01 3.404e-01
# River         (Intercept) 2.772e-19 5.265e-10
# Residual                  3.325e-01 5.766e-01
# Number of obs: 28, groups:  Session:River, 8; River, 4
# 
# Fixed effects:
#                   Estimate Std. Error t value
# (Intercept)         0.7483     0.1968   3.802
# Species_controlHB   0.5748     0.2179   2.638
# 
# Correlation of Fixed Effects:
#   (Intr)
# Spcs_cntrHB -0.554
#### 

x1 <- confint(Shann_all_gaus)
coefs_1 <- coef(summary(Shann_all_gaus))

Shann_Coef_results1 <- data.frame(Species = "HB",
                            Coefficient = coefs_1[2,1],
                            Confint_L = x1[5,1],
                            Confint_U = x1[5,2],
                            SE = coefs_1[2,2],
                            Pvalue = "0.0068",
                            Family = "Gaussian",
                            modelName = "Total pollinator diversity")

###########################################

#### Now repeat for pollinator sub-groups.
names(Spe)
BB <- Spe[,1:4]
SB <- Spe[,5:12]
HoneyB <- Spe[,13]
HF <- Spe[,14:50]

names(BB)

sort(colSums(BB), decreasing = T)
# 
# B_lucorum_agg   B_pascuorum    B_hortorum    B_pratorum 
#      69            44            37            12 

names(SB)

sort(colSums(SB), decreasing = T)
# #   Lasioglossum_spp       H_rubicudus Andrena_clarkella  Andrena_wilkella 
# 4                 3                 1                 1 
# Sphecodes_spp       Andrena_spp       Hylaeus_spp         Osmia_spp 
# 0                 0                 0                 0 
str(HoneyB)
sum(HoneyB)
#[1] 4

which(HoneyB>0)
# [1] 11 23 25 26


sort(colSums(HF), decreasing = T)

#      Helophilus_pendulus       Helophilus_hybridus             Xylota_segnis 
# 142                        43                        24 
# Melanostoma_mellinum        Neoascia_podagrica       Melanostoma_scalare 
# 15                        14                        12 
# Eristalis_pertinax       Sericomyia_silentis        Rhingia_campestris 
# 7                         6                         5 
# Eristalis_arbustorum      Episyrphus_balteatus     Eristalis_interruptus 
# 4                         4                         3 
# Eristalis_abusivus Platycheirus_granditarsus          Myathropa_florea 
# 2                         2                         2 
# Chrysogaster_cemiteriorum    Platycheirus_albimanus   Platycheirus_podagratus 
# 1                         1                         1 
# Eupeodes_nitens         Pipiza_fenestrata     Chalcosyrphus_nemorum 
# 1                         1                         1 
# Helophilus_trivittatus        Mallota_fuciformis           Eristalis_tenax 
# 0                         0                         0 
# Eristalis_horticola     Eristalis_intricarius     Meliscaeva_auricollis 
# 0                         0                         0 
# Platycheirus_splendidus     Platycheirus_scutatus     Platycheirus_tarsalis 


# abundance
Ab_BB <- rowSums(BB)
Ab_SB <- rowSums(SB)
Ab_HonB <- HoneyB
Ab_HF <- rowSums(HF)

#richness of pollinators
#rich1 <- rowSums(Spe>0)

Shann_BB <- diversity(BB)
Shann_SB <- diversity(SB)
Shann_HF <- diversity(HF)
####

###########

(length(which(Ab_BB >0)))/length(Ab_BB)
#[1] 0.75

Ab_BB_gaus <- glmmadmb(Ab_BB ~ Species_control
                        + (1|River/Session), 
                        data = HB, 
                        family = "gaussian")

hist(Ab_BB_gaus$residuals)
shapiro.test(Ab_BB_gaus$residuals)

#Shapiro-Wilk normality test
# 
# data:  Ab_BB_gaus$residuals
# W = 0.60327, p-value = 1.588e-07

Ab_BB_pois <- glmmadmb(Ab_BB~ Species_control + 
                          (1|River/Session) , 
                        data = HB, 
                        family = "poisson")

### 
AIC(Ab_BB_pois)
# [1] 250.978

Ab_BB_nbinom <- glmmadmb(Ab_BB ~ Species_control + 
                            (1|River/Session), 
                          data = HB, 
                          family = "nbinom")

AIC(Ab_BB_nbinom)
# [1] 147.795

######################

Ab_BB_pois_zero <- glmmadmb(Ab_BB~ Species_control + 
                         (1|River/Session) , 
                       data = HB, 
                       family = "poisson",
                       zeroInflation = TRUE)

### 
AIC(Ab_BB_pois_zero)
# [1] 225.288

Ab_BB_nbinom_zero <- glmmadmb(Ab_BB ~ Species_control + 
                           (1|River/Session), 
                         data = HB, 
                         family = "nbinom",
                         zeroInflation = TRUE)

### did not converge..

summary(Ab_BB_nbinom)

#glmmadmb(formula = Ab_BB ~ Species_control + (1 | River/Session), 
# data = HB, family = "nbinom")
# 
# AIC: 147.8 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)          0.423      0.522    0.81    0.417  
# Species_controlHB    1.186      0.471    2.52    0.012 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8 

Ab_Coef_results2 <- data.frame(Species = "HB",
                               Coefficient = coef(Ab_BB_nbinom)[2],
                               Confint_L = confint(Ab_BB_nbinom)[2,1],
                               Confint_U = confint(Ab_BB_nbinom)[2,2],
                               SE = 0.471,
                               Pvalue = 0.012,
                               Family = "Negative binomial",
                               modelName = "Bumblebee abundance")

########### Diversity of Bumblebees..

Shann_BB_gaus <- glmmadmb(Shann_BB ~ Species_control
                           + (1|River/Session), 
                           data = HB, 
                           family = "gaussian")

hist(Shann_BB_gaus$residuals)
shapiro.test(Shann_BB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# 
# data:  Shann_BB_gaus$residuals
# W = 0.89179, p-value = 0.007386

### non -normal try gamma

Shann_BB_gamma <- glmmadmb(Shann_BB + 0.1 ~ Species_control 
                            + (1|Session/River) , 
                            data = HB, 
                            family = "gamma")

summary(Shann_BB_gamma)

# Call:
# # glmmadmb(formula = Shann_BB + 0.1 ~ Species_control + (1 | Session/River), 
# data = HB, family = "gamma")
# 
# AIC: 19.4 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)         -1.258      0.436   -2.89   0.0039 **
#   Species_controlHB    0.662      0.370    1.79   0.0733 . 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, Session=2, Session:River=8 


#### 
Shann_Coef_results2 <- data.frame(Species = "HB",
                                  Coefficient = coef(Shann_BB_gamma)[2],
                                  Confint_L = confint(Shann_BB_gamma)[2,1],
                                  Confint_U = confint(Shann_BB_gamma)[2,2],
                                  SE = 0.370,
                                  Pvalue = "0.0733",
                                  Family = "Gamma",
                                  modelName = "Bumblebee diversity")

###########################################
########### Now solitary Bees

Ab_SB_gaus <- glmmadmb(Ab_SB ~ Species_control
                        + (1|River/Session), 
                        data = HB, 
                        family = "gaussian")

hist(Ab_SB_gaus$residuals)
shapiro.test(Ab_SB_gaus$residuals)

#Shapiro-Wilk normality test
# 
# data:  Ab_SB_gaus$residuals
# W = 0.89666, p-value = 0.009565

Ab_SB_pois <- glmmadmb(Ab_SB~ Species_control + 
                          (1|River/Session) , 
                        data = HB, 
                        family = "poisson")
AIC(Ab_SB_pois)
#[1] 41.6436


Ab_SB_nbinom <- glmmadmb(Ab_SB ~ Species_control + 
                            (1|River/Session), 
                          data = HB, 
                          family = "nbinom")
AIC(Ab_SB_nbinom)
# [1] 43.651
### 

Ab_SB_poisson_zero <- glmmadmb(Ab_SB ~ Species_control + 
                           (1|River/Session), 
                         data = HB, 
                         family = "poisson",
                         zeroInflation = TRUE)

AIC(Ab_SB_poisson_zero)
## does not resolve 

Ab_SB_nbinom_zero <- glmmadmb(Ab_SB ~ Species_control + 
                           (1|River/Session), 
                         data = HB, 
                         family = "nbinom",
                         zeroInflation = TRUE)

### does not resolve
## [1] 53.6418
### 

summary(Ab_SB_pois)

# glmmadmb(formula = Ab_SB ~ Species_control + (1 | River/Session), 
# data = HB, family = "poisson")
# 
# AIC: 41.6 
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)  
#   (Intercept)          -3.10       1.25   -2.47    0.014 *
#   Species_controlHB     2.08       1.06    1.96    0.050 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8 

Ab_Coef_results3 <- data.frame(Species = "HB",
                               Coefficient = coef(Ab_SB_pois)[2],
                               Confint_L = confint(Ab_SB_pois)[2,1],
                               Confint_U = confint(Ab_SB_pois)[2,2],
                               SE = 1.06,
                               Pvalue = 0.050,
                               Family = "Poisson",
                               modelName = "Solitary bee abundance")

########### Diversity of Solitary Bees..

Shann_SB_gaus <- glmmadmb(Shann_SB ~ Species_control
                           + (1|River/Session), 
                           data = HB, 
                           family = "gaussian")

hist(Shann_SB_gaus$residuals)
shapiro.test(Shann_SB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_SB_gaus$residuals
# W = 0.32115, p-value = 2.508e-10

### non-normal try gamma

Shann_SB_gamma <- glmmadmb(Shann_SB + 0.1 ~ Species_control 
                            + (1|Session/River) , 
                            data = HB, 
                            family = "gamma")

summary(Shann_SB_gamma)

# Call:
#glmmadmb(formula = Shann_SB + 0.1 ~ Species_control + (1 | Session/River), 
# data = HB, family = "gamma")
# 
# AIC: -76.8 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -2.279      0.144  -15.82   <2e-16 ***
#   Species_controlHB    0.259      0.164    1.58     0.11    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, Session=2, Session:River=8 

#### 
Shann_Coef_results3 <- data.frame(Species = "HB",
                                  Coefficient = coef(Shann_SB_gamma)[2],
                                  Confint_L = confint(Shann_SB_gamma)[2,1],
                                  Confint_U = confint(Shann_SB_gamma)[2,2],
                                  SE = 0.164,
                                  Pvalue = "0.110",
                                  Family = "Gamma",
                                  modelName = "Solitary bee diversity")

###########################################
########### hoverflies

Ab_HF_gaus <- glmmadmb(Ab_HF ~ Species_control
                        + (1|River/Session), 
                        data = HB, 
                        family = "gaussian")

hist(Ab_HF_gaus$residuals)
shapiro.test(Ab_HF_gaus$residuals)

#Shapiro-Wilk normality test
# 
# data:  Ab_HF_gaus$residuals
# W = 0.79349, p-value = 8.212e-05

Ab_HF_pois <- glmmadmb(Ab_HF~ Species_control + 
                          (1|River/Session) , 
                        data = HB, 
                        family = "poisson")

###
AIC(Ab_HF_pois)
# [1] 268.03
Ab_HF_nbinom <- glmmadmb(Ab_HF ~ Species_control + 
                            (1|River/Session), 
                          data = HB, 
                          family = "nbinom")
AIC(Ab_HF_nbinom)
#[1] 190.1504

Ab_HF_pois_zero <- glmmadmb(Ab_HF~ Species_control + 
                         (1|River/Session) , 
                       data = HB, 
                       family = "poisson",
                       zeroInflation = T)
AIC(Ab_HF_pois_zero)
## [1] 255.362
### does not resolve 

Ab_HF_nbinom_zero <- glmmadmb(Ab_HF ~ Species_control + 
                           (1|River/Session), 
                         data = HB, 
                         family = "nbinom",
                         zeroInflation = T)

AIC(Ab_HF_nbinom_zero)
# [1] 192.1504


summary(Ab_HF_nbinom)

#glmmadmb(formula = Ab_HF ~ Species_control + (1 | River/Session), 
# data = HB, family = "nbinom")
# 
# AIC: 190.2 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          1.961      0.490    4.00  6.3e-05 ***
#   Species_controlHB   -0.146      0.421   -0.35     0.73    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8 

Ab_Coef_results4 <- data.frame(Species = "HB",
                               Coefficient = coef(Ab_HF_nbinom_zero)[2],
                               Confint_L = confint(Ab_HF_nbinom_zero)[2,1],
                               Confint_U = confint(Ab_HF_nbinom_zero)[2,2],
                               SE = 0.421,
                               Pvalue = 0.730,
                               Family = "Negative binomial",
                               modelName = "Hoverfly abundance")

########### Diversity of hoverflies..

Shann_HF_gaus <- glmmadmb(Shann_HF ~ Species_control
                           + (1|River/Session), 
                           data = HB, 
                           family = "gaussian")

hist(Shann_HF_gaus$residuals)
shapiro.test(Shann_HF_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_HF_gaus$residuals
# W = 0.94768, p-value = 0.1732

### normal

summary(Shann_HF_gaus)

# glmmadmb(formula = Shann_HF ~ Species_control + (1 | River/Session), 
# data = HB, family = "gaussian")
# 
# AIC: 49 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)          0.467      0.176    2.65    0.008 **
#   Species_controlHB    0.348      0.148    2.35    0.019 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8  
#### 

Shann_Coef_results4 <- data.frame(Species = "HB",
                                  Coefficient = coef(Shann_HF_gaus)[2],
                                  Confint_L = confint(Shann_HF_gaus)[2,1],
                                  Confint_U = confint(Shann_HF_gaus)[2,2],
                                  SE = 0.148,
                                  Pvalue = "0.019",
                                  Family = "Gaussian",
                                  modelName = "Hoverfly diversity")

###########################################

###########################################
###########

which(Ab_HonB>0)

HB[which(Ab_HonB>0),1:12]

### Only four honey bees trapped.  At a control site on the Bann

## 3 HB sites - 1 control

Ab_HoneyB_gaus <- glmmadmb(Ab_HonB ~ Species_control
                       + (1|River/Session), 
                       data = HB, 
                       family = "gaussian")

hist(Ab_HoneyB_gaus$residuals)
shapiro.test(Ab_HoneyB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Ab_HoneyB_gaus$residuals
# W = 0.60435, p-value = 1.636e-07


Ab_HoneyB_pois <- glmmadmb(Ab_HonB~ Species_control + 
                         (1|River/Session) , 
                       data = HB, 
                       family = "poisson")

AIC(Ab_HoneyB_pois)
## [1] 30.5208


Ab_HoneyB_nbinom <- glmmadmb(Ab_HonB ~ Species_control + 
                           (1|River/Session), 
                         data = HB, 
                         family = "nbinom")

AIC(Ab_HoneyB_nbinom)
## [1] 32.5226

Ab_HoneyB_pois_zero <- glmmadmb(Ab_HonB~ Species_control + 
                             (1|River/Session) , 
                           data = HB, 
                           family = "poisson",
                           zeroInflation = T)

AIC(Ab_HoneyB_pois_zero)
# [1] 32.5208

Ab_HoneyB_nbinom_zero <- glmmadmb(Ab_HonB ~ Species_control + 
                               (1|River/Session), 
                             data = HB, 
                             family = "nbinom",
                             zeroInflation = T)

AIC(Ab_HoneyB_nbinom_zero)
# [1] 34.5226

summary(Ab_HoneyB_pois)


# glmmadmb(formula = Ab_HonB ~ Species_control + (1 | River/Session), 
# data = HB, family = "poisson")
# 
# AIC: 30.5 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)          -2.64       1.00   -2.64   0.0083 **
#   Species_controlHB     1.10       1.16    0.95   0.3414   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=28, River=4, River:Session=8

Ab_Coef_results5 <- data.frame(Species = "HB",
                               Coefficient = coef(Ab_HoneyB_pois)[2],
                               Confint_L = confint(Ab_HoneyB_pois)[2,1],
                               Confint_U = confint(Ab_HoneyB_pois)[2,2],
                               SE = 1.16,
                               Pvalue = 0.3414,
                               Family = "Poisson",
                               modelName = "Honey bee abundance")

HB_abundance_impact_results <- rbind(Ab_Coef_results1,Ab_Coef_results2,
                                     Ab_Coef_results3, Ab_Coef_results4,
                                     Ab_Coef_results5)

HB_diversity_impact_results <- rbind(Shann_Coef_results1,Shann_Coef_results2,
                                     Shann_Coef_results3, Shann_Coef_results4)



#########
# write.csv(HB_abundance_impact_results, "HB_abundance_impact_results_01_08_2017.csv")
# write.csv(HB_diversity_impact_results, "HB_diversity_impact_results_01_08_2017.csv")

######### Part B: community analysis - RDA ########################

HB<- read.csv("HB_with_flower_data_vr1.csv")

str(HB)
names(HB)
# #  [1] "Site"                            "Species"                        
# [3] "Species_control"                 "Session"                        
# [5] "River"                           "Site_ID"                        
# [7] "Date_of_Trap"                    "Inv_status"                     
# [9] "IAS_._Cover"                     "Open_Inf"                       
# [11] "SR_flowering_per_10m"            "Unique_currently_flowering_100m"
# [13] "B_lucorum_agg"                   "B_pratorum"                     
# [15] "B_pascuorum"                     "B_hortorum"                     
# [17] "Sphecodes_spp"                   "H_rubicudus"                    
# [19] "Lasioglossum_spp"                "Andrena_spp"                    
# [21] "Andrena_clarkella"               "Andrena_wilkella"               
# [23] "Hylaeus_spp"                     "Osmia_spp"                      
# [25] "Apis_mellifera"                  "Rhingia_campestris"             
# [27] "Helophilus_pendulus"             "Helophilus_hybridus"            
# [29] "Helophilus_trivittatus"          "Melanostoma_scalare"            
# [31] "Melanostoma_mellinum"            "Mallota_fuciformis"             
# [33] "Eristalis_tenax"                 "Eristalis_abusivus"             
# [35] "Eristalis_pertinax"              "Eristalis_arbustorum"           
# [37] "Eristalis_interruptus"           "Eristalis_horticola"            
# [39] "Eristalis_intricarius"           "Chrysogaster_cemiteriorum"      
# [41] "Meliscaeva_auricollis"           "Platycheirus_albimanus"         
# [43] "Platycheirus_splendidus"         "Platycheirus_granditarsus"      
# [45] "Platycheirus_scutatus"           "Platycheirus_podagratus"        
# [47] "Platycheirus_tarsalis"           "Syrphus_ribesii"                
# [49] "Syrphus_vitripennis"             "Neoascia_podagrica"             
# [51] "Episyrphus_balteatus"            "Sericomyia_lappona"             
# [53] "Sericomyia_silentis"             "Eupeodes_corollae"              
# [55] "Eupeodes_latifasciatus"          "Eupeodes_luniger"               
# [57] "Eupeodes_nitens"                 "Myathropa_florea"               
# [59] "Pipiza_fenestrata"               "Chalcosyrphus_nemorum"          
# [61] "Baccha_elongata"                 "Xylota_segnis"   

### Split community data into groups , Honeybees, Bumblebees, Solitary Bees and Hoverflies

Spe <- HB[,13:62]

BB <- Spe[,1:4]
names(BB)
newBB <- BB[,which(colSums(BB)>0)]
names(newBB)
## 4 sp
SB <- Spe[,5:12]
names(SB)
newSB <- SB[,which(colSums(SB)>0)]
names(newSB)
### 4 sp
HoneyB <- Spe[,13]

HF <- Spe[,14:50]
names(HF)
newHF <- HF[,which(colSums(HF)>0)]
names(newHF)
#21 sp

Env <- HB[,1:12]

names(Env)

# [1] "Site"                            "Species"                        
# [3] "Species_control"                 "Session"                        
# [5] "River"                           "Site_ID"                        
# [7] "Date_of_Trap"                    "Inv_status"                     
# [9] "IAS_._Cover"                     "Open_Inf"                       
# [11] "SR_flowering_per_10m"            "Unique_currently_flowering_100m"     


summary(Env$Species_control)
# Control      JK 
# 14      14 

summary(Env$Session)

# A  B 
# 14 14


### First Bumblebees

### transform community data prior to RDA
hell_BB <- decostand(newBB, method = "hellinger")

mod_rda1 <- rda(hell_BB~ Species_control + Condition(River + Session), data = Env)
anova(mod_rda1, by = "margin", model = "reduced")

#Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hell_BB ~ Species_control + Condition(River + Session), data = Env)
# Df Variance    F Pr(>F)
# Species_control  1  0.01742 1.04  0.383
# Residual        22  0.36840

mod_rda1
# Call: rda(formula = hell_BB ~ Species_control + Condition(River + Session), data = Env)
# 
#               Inertia Proportion Rank
# Total         0.44460    1.00000     
# Conditional   0.05878    0.13221    4
# Constrained   0.01742    0.03917    1
# Unconstrained 0.36840    0.82862    4
# Inertia is variance 


### Solitary Bees

hellSB <- decostand(newSB, method = "hellinger")
mod_cca_SB <- rda(hellSB~ Species_control + Condition(River + Session), data = Env)
anova(mod_cca_SB, by = "margin", model = "reduced")
# 
# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hellSB ~ Species_control + Condition(River + Session), data = Env)
#                  Df Variance      F Pr(>F)
# Species_control  1  0.01455 1.9918  0.142
# Residual        22  0.16071    
# ##################################################

mod_cca_SB
# 
# Call: rda(formula = hellSB ~ Species_control + Condition(River + Session), data = Env)
# 
# Inertia Proportion Rank
# Total         0.23413    1.00000     
# Conditional   0.05886    0.25141    4
# Constrained   0.01455    0.06215    1
# Unconstrained 0.16071    0.68644    3
# Inertia is variance 

### Hoverflies

hellHF <- decostand(newHF, method = "hellinger")
mod_cca_HF2 <- rda(hellHF~ Species_control + Condition(River + Session), data = Env)
anova(mod_cca_HF2, by = "margin", model = "reduced")

#Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hellHF ~ Species_control + Condition(River + Session), data = Env)
#                 Df Variance      F Pr(>F)
# Species_control  1  0.02342 1.3274  0.242
# Residual        22  0.38819  


mod_cca_HF2
# Call: rda(formula = hellHF ~ Species_control + Condition(River + Session), data = Env)
# 
#                Inertia Proportion Rank
# Total         0.53934    1.00000     
# Conditional   0.12772    0.23681    4
# Constrained   0.02342    0.04343    1
# Unconstrained 0.38819    0.71976   19

######################################


######### Calculate variance explained per species by RDA

inert_spe <- as.data.frame(inertcomp(mod_cca_HF2, display = c("species"),
                                     statistic = c("explained"), proportional = TRUE))

#########################  

