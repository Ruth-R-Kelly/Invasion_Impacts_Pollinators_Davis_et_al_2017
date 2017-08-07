## Pollinator models Japanese knotweed (Fallopia japonica) only (GLMMs and RDAs)
## Ruth Kelly (kellyr44@tcd.ie) 01/08/2017 
## For "Contrasting impacts of high priority invasive plant species on pollinating insect communities"
## Based on data collected by Emily Davis, study design by Emily Davis, Jane Stout and Christine Maggs


##########  
library("glmmADMB") ### for calculating GLMM's

library("vegan") ### for calculating shannon diversity


#### Part A: GLMMs ####

JK<- read.csv("JK_with_flower_data_vr1.csv")

summary(JK)

table(JK$River,JK$Inv_status)

# Invaded Native
# Faughan              6      6
# Lagan                2      2
# Mon Blackwater       4      4
# Roe                  4      4


table(JK$Session,JK$Inv_status)

#     Invaded Native
# A       8      8
# B       8      8


names(JK)
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


summary(JK[,1:12])

##########

#insect species
Spe <- JK[,13:62]
str(Spe)
# abundance
Ab <- rowSums(Spe)

#richness of pollinators
rich1 <- rowSums(Spe>0)

Shann <- diversity(Spe)

###########

Ab_all_gaus <- glmmadmb(Ab ~ Species_control
                        + (1|River/Session), 
                        data = JK, 
                        family = "gaussian")

hist(Ab_all_gaus$residuals)
shapiro.test(Ab_all_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Ab_all_gaus$residuals
# W = 0.86352, p-value = 0.0008259

Ab_all_pois <- glmmadmb(Ab~ Species_control + 
                          (1|River/Session) , 
                        data = JK, 
                        family = "poisson")

###
AIC(Ab_all_pois)
# [1] 828.668
Ab_all_nbinom <- glmmadmb(Ab ~ Species_control + 
                          (1|River/Session), 
                        data = JK, 
                        family = "nbinom")

AIC(Ab_all_nbinom)
## [1] 340.372  much better!!

summary(Ab_all_nbinom)

# Call:
# glmmadmb(formula = Ab ~ Species_control + (1 | River/Session), 
#          data = JK, family = "nbinom")
# 
# AIC: 340.4 
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          4.237      0.301   14.08   <2e-16 ***
#   Species_controlJK   -0.338      0.278   -1.22     0.22    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=32, River=4, River:Session=8 
# Random effect variance(s):

Ab_Coef_results1 <- data.frame(Species = "JK",
                                     Coefficient = coef(Ab_all_nbinom)[2],
                                     Confint_L = confint(Ab_all_nbinom)[2,1],
                                     Confint_U = confint(Ab_all_nbinom)[2,2],
                                     SE = 0.278,
                                    Pvalue = 0.220,
                                     Family = "Negative binomial",
                                     modelName = "Total pollinator abundance")

########### Diversity of all pollinators..

Shann_all_gaus <- glmmadmb(Shann ~ Species_control
                         + (1|River/Session), 
                        data = JK, 
                        family = "gaussian")

hist(Shann_all_gaus$residuals)
shapiro.test(Shann_all_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_all_gaus$residuals
# W = 0.93059, p-value = 0.04067

### non -normal try gamma

Shann_all_gamma <- glmmadmb(Shann + 0.1 ~ Species_control 
                              + (1|Session/River) , 
                         data = JK, 
                         family = "gamma")

summary(Shann_all_gamma)

# glmmadmb(formula = Shann + 0.1 ~ Species_control + (1 | Session/River), 
#          data = JK, family = "gamma")
# 
# AIC: 65.3 
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)  
# (Intercept)         0.0308     0.1405    0.22    0.827  
# Species_controlJK   0.3705     0.1578    2.35    0.019 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### 
Shann_Coef_results1 <- data.frame(Species = "JK",
                            Coefficient = coef(Shann_all_gamma)[2],
                            Confint_L = confint(Shann_all_gamma)[2,1],
                            Confint_U = confint(Shann_all_gamma)[2,2],
                            SE = 0.1578,
                            Pvalue = "0.019",
                            Family = "Gamma",
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
# B_lucorum_agg   B_pascuorum    B_pratorum    B_hortorum 
# 29            12             8             0 

names(SB)

sort(colSums(SB), decreasing = T)  ### only 6 individuals in total do not run models for this.. 
# Andrena_spp  Lasioglossum_spp         Osmia_spp     Sphecodes_spp 
# 3                 2                 1                 0 
# H_rubicudus Andrena_clarkella  Andrena_wilkella       Hylaeus_spp 
# 0                 0                 0                 0 

sum(HoneyB)
#[1] 10

sort(colSums(HF), decreasing = T)

# Helophilus_pendulus      Eristalis_arbustorum       Helophilus_hybridus 
# 1385                       161                       111 
# Eristalis_pertinax        Neoascia_podagrica       Melanostoma_scalare 
# 98                        97                        94 
# Melanostoma_mellinum    Platycheirus_albimanus           Eristalis_tenax 
# 93                        55                        41 
# Episyrphus_balteatus           Syrphus_ribesii     Eristalis_interruptus 
# 28                        26                        21 
# Rhingia_campestris       Sericomyia_silentis     Eristalis_intricarius 
# 14                        14                         7 
# Chrysogaster_cemiteriorum         Eupeodes_corollae       Eristalis_horticola 
# 6                         6                         4 
# Meliscaeva_auricollis Platycheirus_granditarsus       Syrphus_vitripennis 
# 3                         3                         3 
# Eristalis_abusivus   Platycheirus_podagratus    Helophilus_trivittatus 
# 2                         2                         1 
# Platycheirus_tarsalis        Sericomyia_lappona    Eupeodes_latifasciatus 
# 1                         1                         1 
# Eupeodes_luniger           Eupeodes_nitens           Baccha_elongata 
# 1                         1                         1 
# Mallota_fuciformis   Platycheirus_splendidus     Platycheirus_scutatus 
# 0                         0                         0 
# Myathropa_florea         Pipiza_fenestrata     Chalcosyrphus_nemorum 
# 0                         0                         0 
# Xylota_segnis 
# 0 


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

Ab_BB_gaus <- glmmadmb(Ab_BB ~ Species_control
                        + (1|River/Session), 
                        data = JK, 
                        family = "gaussian")

hist(Ab_BB_gaus$residuals)
shapiro.test(Ab_BB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Ab_BB_gaus$residuals
# W = 0.90889, p-value = 0.01051

Ab_BB_pois <- glmmadmb(Ab_BB~ Species_control + 
                          (1|River/Session) , 
                        data = JK, 
                        family = "poisson")

### 
AIC(Ab_BB_pois)
# [1] 107.8508

Ab_BB_nbinom <- glmmadmb(Ab_BB ~ Species_control + 
                            (1|River/Session), 
                          data = JK, 
                          family = "nbinom")

AIC(Ab_BB_nbinom)
# [1] 107.253

##

Ab_BB_pois_zero <- glmmadmb(Ab_BB~ Species_control + 
                         (1|River/Session) , 
                       data = JK, 
                       family = "poisson",
                       zeroInflation = T)

### 
AIC(Ab_BB_pois_zero)
# [1] 106.6544

Ab_BB_nbinom_zero <- glmmadmb(Ab_BB ~ Species_control + 
                           (1|River/Session), 
                         data = JK, 
                         family = "nbinom",
                         zeroInflation = T)

AIC(Ab_BB_nbinom_zero)
## [1] 108.279

summary(Ab_BB_pois_zero) 

# Call:
# glmmadmb(formula = Ab_BB ~ Species_control + (1 | River/Session), 
#          data = JK, family = "poisson", zeroInflation = T)
# 
# AIC: 106.7 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)         -0.437      0.587   -0.74    0.457  
# Species_controlJK    0.752      0.310    2.43    0.015 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=32, River=4, River:Session=8 
# 
# Number of observations: total=32, River=4, River:Session=8 

Ab_Coef_results2 <- data.frame(Species = "JK",
                               Coefficient = coef(Ab_BB_pois_zero)[2],
                               Confint_L = confint(Ab_BB_pois_zero)[2,1],
                               Confint_U = confint(Ab_BB_pois_zero)[2,2],
                               SE = 0.310,
                               Pvalue = 0.015,
                               Family = "Zero inflated poisson",
                               modelName = "Bumblebee abundance")

########### Diversity of Bumblebees..

Shann_BB_gaus <- glmmadmb(Shann_BB ~ Species_control
                           + (1|River/Session), 
                           data = JK, 
                           family = "gaussian")

hist(Shann_BB_gaus$residuals)
shapiro.test(Shann_BB_gaus$residuals)

# Shapiro-Wilk normality test

# data:  Shann_all_gaus$residuals
# W = 0.85525, p-value = 0.00054

### non -normal try gamma

Shann_BB_gamma <- glmmadmb(Shann_BB + 0.1 ~ Species_control 
                            + (1|Session/River) , 
                            data = JK, 
                            family = "gamma")

summary(Shann_BB_gamma)

# glmmadmb(formula = Shann_BB + 0.1 ~ Species_control + (1 | Session/River), 
#          data = JK, family = "gamma")
# 
# AIC: -14.7 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.465      0.292   -5.02  5.1e-07 ***
#   Species_controlJK   -0.105      0.276   -0.38      0.7    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=32, Session=2, Session:River=8 


#### 
Shann_Coef_results2 <- data.frame(Species = "JK",
                                  Coefficient = coef(Shann_BB_gamma)[2],
                                  Confint_L = confint(Shann_BB_gamma)[2,1],
                                  Confint_U = confint(Shann_BB_gamma)[2,2],
                                  SE = 0.276,
                                  Pvalue = "0.700",
                                  Family = "Gamma",
                                  modelName = "Bumblebee diversity")

###########################################
########### No models for Solitary bees as there are insufficient numbers caught.. 

###########################################
########### hoverflies

Ab_HF_gaus <- glmmadmb(Ab_HF ~ Species_control
                        + (1|River/Session), 
                        data = JK, 
                        family = "gaussian")

hist(Ab_HF_gaus$residuals)
shapiro.test(Ab_HF_gaus$residuals)

#Shapiro-Wilk normality test
# 
# data:  Ab_HF_gaus$residuals
# W = 0.85806, p-value = 0.0006232

Ab_HF_pois <- glmmadmb(Ab_HF~ Species_control + 
                          (1|River/Session) , 
                        data = JK, 
                        family = "poisson")

### does not resolve

Ab_HF_nbinom <- glmmadmb(Ab_HF ~ Species_control + 
                            (1|River/Session), 
                          data = JK, 
                          family = "nbinom")
AIC(Ab_HF_nbinom)
##[1] 337.812

Ab_HF_pois_zero <- glmmadmb(Ab_HF~ Species_control + 
                         (1|River/Session) , 
                       data = JK, 
                       family = "poisson",
                       zeroInflation = TRUE)

### does not resolve

Ab_HF_nbinom_zero <- glmmadmb(Ab_HF ~ Species_control + 
                           (1|River/Session), 
                         data = JK, 
                         family = "nbinom",
                         zeroInflation = TRUE)
AIC(Ab_HF_nbinom_zero)

##[1] 339.812
summary(Ab_HF_nbinom)




#Call:
# glmmadmb(formula = Ab_HF ~ Species_control + (1 | River/Session), 
#          data = JK, family = "nbinom")
# 
# AIC: 337.8 
# 
# Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          4.226      0.307   13.76   <2e-16 ***
#   Species_controlJK   -0.400      0.280   -1.43     0.15    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=32, River=4, River:Session=8 

Ab_Coef_results4 <- data.frame(Species = "JK",
                               Coefficient = coef(Ab_HF_nbinom)[2],
                               Confint_L = confint(Ab_HF_nbinom)[2,1],
                               Confint_U = confint(Ab_HF_nbinom)[2,2],
                               SE = 0.280,
                               Pvalue = 0.150,
                               Family = "Negative binomial",
                               modelName = "Hoverfly abundance")

########### Diversity of hoverflies..

Shann_HF_gaus <- glmmadmb(Shann_HF ~ Species_control
                           + (1|River/Session), 
                           data = JK, 
                           family = "gaussian")

hist(Shann_HF_gaus$residuals)
shapiro.test(Shann_HF_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_HF_gaus$residuals
# W = 0.94257, p-value = 0.08856

### normal

summary(Shann_HF_gaus)

# glmmadmb(formula = Shann_HF ~ Species_control + (1 | River/Session), 
# data = JK, family = "gaussian")
# 
# AIC: 43.8 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          0.834      0.171    4.88    1e-06 ***
#   Species_controlJK    0.421      0.122    3.44  0.00057 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=32, River=4, River:Session=8 


#### 
Shann_Coef_results4 <- data.frame(Species = "JK",
                                  Coefficient = coef(Shann_HF_gaus)[2],
                                  Confint_L = confint(Shann_HF_gaus)[2,1],
                                  Confint_U = confint(Shann_HF_gaus)[2,2],
                                  SE = 0.122,
                                  Pvalue = "<0.001",
                                  Family = "Gaussian",
                                  modelName = "Hoverfly diversity")

###########################################

###########################################
###########

Ab_HoneyB_gaus <- glmmadmb(Ab_HonB ~ Species_control
                       + (1|River/Session), 
                       data = JK, 
                       family = "gaussian")

hist(Ab_HoneyB_gaus$residuals)
shapiro.test(Ab_HoneyB_gaus$residuals)

#Shapiro-Wilk normality test

# data:  Ab_HoneyB_gaus$residuals
# W = 0.46865, p-value = 1.198e-09


summary(Ab_HonB)
which(Ab_HonB >0)
# [1]  3  4  9 21 22 25

JK[which(Ab_HonB >0),]

Ab_HoneyB_pois <- glmmadmb(Ab_HonB~ Species_control + 
                         (1|River/Session) , 
                       data = JK, 
                       family = "poisson")

### does not resolve

Ab_HoneyB_nbinom <- glmmadmb(Ab_HonB ~ Species_control + 
                           (1|River/Session), 
                         data = JK, 
                         family = "nbinom")

AIC(Ab_HoneyB_nbinom)

# [1] 52.8564

Ab_HoneyB_pois_zero <- glmmadmb(Ab_HonB~ Species_control + 
                             (1|River/Session) , 
                           data = JK, 
                           family = "poisson",
                           zeroInflation = T)

## does not resolve

Ab_HoneyB_nbinom_zero <- glmmadmb(Ab_HonB ~ Species_control + 
                               (1|River/Session), 
                             data = JK, 
                             family = "nbinom",
                             zeroInflation = T)

AIC(Ab_HoneyB_nbinom_zero)
# [1] 54.8564

summary(Ab_HoneyB_nbinom_zero)

#Call:
# glmmadmb(formula = Ab_HonB ~ Species_control + (1 | River/Session), 
# data = JK, family = "poisson", zeroInflation = T)
# 
# AIC: 53.9 
# 
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)
# (Intercept)         -0.754      0.800   -0.94     0.35
# Species_controlJK    1.322      0.947    1.40     0.16
# 
# Number of observations: total=32, River=4, River:Session=8 


Ab_Coef_results5 <- data.frame(Species = "JK",
                               Coefficient = coef(Ab_HoneyB_nbinom_zero)[2],
                               Confint_L = confint(Ab_HoneyB_nbinom_zero)[2,1],
                               Confint_U = confint(Ab_HoneyB_nbinom_zero)[2,2],
                               SE = 0.947,
                               Pvalue = 0.160,
                               Family = "Zero inflated negative binomial",
                               modelName = "Honey bee abundance")

JK_abundance_impact_results <- rbind(Ab_Coef_results1,Ab_Coef_results2,
                                     Ab_Coef_results3, Ab_Coef_results4,
                                     Ab_Coef_results5)

JK_diversity_impact_results <- rbind(Shann_Coef_results1,Shann_Coef_results2,
                                     Shann_Coef_results3, Shann_Coef_results4)



#########

# write.csv(JK_abundance_impact_results, "JK_abundance_impact_results_01_08_2017.csv")
# write.csv(JK_diversity_impact_results, "JK_diversity_impact_results_01_08_2017.csv")

###########################################

####### Part B:  Ordination analysis of community data. ####

JK<- read.csv("JK_with_flower_data_vr1.csv")

str(JK)
names(JK)
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

Spe <- JK[,13:62]

BB <- Spe[,1:4]
names(BB)
newBB <- BB[,which(colSums(BB)>0)]
names(newBB)

SB <- Spe[,5:12]
names(SB)
newSB <- SB[,which(colSums(SB)>0)]
names(newSB)

HoneyB <- Spe[,13]

HF <- Spe[,14:50]
names(HF)
newHF <- HF[,which(colSums(HF)>0)]
names(newHF)

Env <- JK[,1:12]

names(Env)

# [1] "Site"                            "Species"                        
# [3] "Species_control"                 "Session"                        
# [5] "River"                           "Site_ID"                        
# [7] "Date_of_Trap"                    "Inv_status"                     
# [9] "IAS_._Cover"                     "Open_Inf"                       
# [11] "SR_flowering_per_10m"            "Unique_currently_flowering_100m"     


summary(Env$Species_control)
# Control      JK 
# 16      16 

summary(Env$Session)

# A  B 
# 16 16

hell_BB <- decostand(newBB, method = "hellinger")

mod_rda1 <- rda(hell_BB~ Species_control + Condition(River + Session), data = Env)
anova(mod_rda1, by = "margin", model = "reduced")

# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hell_BB ~ Species_control + Condition(River + Session), data = Env)
#                  Df Variance      F Pr(>F)
# Species_control  1 0.008551 0.7505  0.527
# Residual        26 0.296227

mod_rda1
# #Call: rda(formula = hell_BB ~ Species_control + Condition(River + Session), data = Env)

# Inertia Proportion Rank
# Total         0.385126   1.000000     
# Conditional   0.080348   0.208628    4
# Constrained   0.008551   0.022203    1
# Unconstrained 0.296227   0.769169    3
# 

##### No solitary bee model because too few records...


hellHF <- decostand(newHF, method = "hellinger")
mod_cca_HF <- rda(hellHF~ Species_control + Condition(River + Session), data = Env)
anova(mod_cca_HF, by = "margin", model = "reduced")

# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hellHF ~ Species_control + Condition(River + Session), data = Env)
# Df Variance      F Pr(>F)   
# Species_control  1 0.021919 2.4294  0.009 **
#   Residual        26 0.234584                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

mod_cca_HF
# Call: rda(formula = hellHF ~ Species_control + Condition(River + Session), data = Env)
# 
#                Inertia Proportion Rank
# Total         0.34346    1.00000     
# Conditional   0.08696    0.25319    4
# Constrained   0.02192    0.06382    1
# Unconstrained 0.23458    0.68299   26




#########  Calculating variance explained per species by RDA analyses. Proportional by pCCA, CCA and CA. 

inert_spe <- as.data.frame(inertcomp(mod_cca_HF, display = c("species"),
                                     statistic = c("explained"), proportional = TRUE))

inert_spe
###############

