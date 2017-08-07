## Pollinator models Giant Hogweed (Heracleum mantegazzianum) only (GLMMS and RDAs)
## Ruth Kelly (kellyr44@tcd.ie) 01/08/2017 
## For "Contrasting impacts of high priority invasive plant species on pollinating insect communities"
## Based on data collected by Emily Davis, study design by Emily Davis, Jane Stout and Christine Maggs


####
library("glmmADMB") ### for calculating GLMM's

library("vegan") ### for calculating shannon diversity

#### Part A: GLMMs ####

GH<- read.csv("GH_with_flower_data_vr1.csv")

str(GH)



table(GH$River,GH$Inv_status)

# #           Invaded Native
# Ballinderry          4      4
# Bann                 4      4
# Castle               4      4
# Mon Blackwater       6      6

table(GH$Session,GH$Inv_status)

#   Invaded Native
# A       9      9
# B       9      9


names(GH)
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


summary(GH[,1:12])

##########

#insect species
Spe <- GH[,13:62]
str(Spe)
# abundance
Ab <- rowSums(Spe)


Shann <- diversity(Spe)

###########

Ab_all_gaus <- glmmadmb(Ab ~ Species_control
                        + (1|River/Session), 
                        data = GH, 
                        family = "gaussian")

hist(Ab_all_gaus$residuals)
shapiro.test(Ab_all_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Ab_all_gaus$residuals
# W = 0.93659, p-value = 0.03981

Ab_all_pois <- glmmadmb(Ab~ Species_control + 
                          (1|River/Session) , 
                        data = GH, 
                        family = "poisson")

###

AIC(Ab_all_pois)
# [1] 300.222

Ab_all_nbinom <- glmmadmb(Ab ~ Species_control + 
                          (1|River/Session), 
                        data = GH, 
                        family = "nbinom")

AIC(Ab_all_nbinom)
# [1] 230.126

summary(Ab_all_nbinom)

# glmmadmb(formula = Ab ~ Species_control + (1 | River/Session), 
#          data = GH, family = "nbinom")
# 
# AIC: 230.1 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          2.221      0.413    5.38  7.6e-08 ***
#   Species_controlGH   -0.523      0.326   -1.60     0.11    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 
# Random effect variance(s):

Ab_Coef_results1 <- data.frame(Species = "GH",
                                     Coefficient = coef(Ab_all_nbinom)[2],
                                     Confint_L = confint(Ab_all_nbinom)[2,1],
                                     Confint_U = confint(Ab_all_nbinom)[2,2],
                                     SE = 0.326,
                                    Pvalue = 0.110,
                                     Family = "Negative binomial",
                                     modelName = "Total pollinator abundance")

########### Diversity of all pollinators..

Shann_all_gaus <- glmmadmb(Shann ~ Species_control
                         + (1|River/Session), 
                        data = GH, 
                        family = "gaussian")

hist(Shann_all_gaus$residuals)
shapiro.test(Shann_all_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_all_gaus$residuals
# W = 0.95846, p-value = 0.1927

### normal

summary(Shann_all_gaus)

#glmmadmb(formula = Shann ~ Species_control + (1 | River/Session), 
# data = GH, family = "gaussian")
# 
# AIC: 59.1 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         0.7197     0.1957    3.68  0.00023 ***
#   Species_controlGH   0.0668     0.1316    0.51  0.61166    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 

#### 
Shann_Coef_results1 <- data.frame(Species = "GH",
                            Coefficient = coef(Shann_all_gaus)[2],
                            Confint_L = confint(Shann_all_gaus)[2,1],
                            Confint_U = confint(Shann_all_gaus)[2,2],
                            SE = 0.1316,
                            Pvalue = "0.612",
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
# B_lucorum_agg    B_pratorum   B_pascuorum    B_hortorum 
# 38             7             0             0 

names(SB)

sort(colSums(SB), decreasing = T)
#       H_rubicudus  Lasioglossum_spp       Andrena_spp       Hylaeus_spp 
#            10                 3                 3                 2 
# Sphecodes_spp Andrena_clarkella  Andrena_wilkella         Osmia_spp 
#     1                 0                 0                 0 

str(HoneyB)
sum(HoneyB)
#[1] 1

sort(colSums(HF), decreasing = T)

#  Helophilus_pendulus        Rhingia_campestris      Eristalis_arbustorum 
# 163                        23                        16 
# Helophilus_hybridus      Melanostoma_mellinum Chrysogaster_cemiteriorum 
# 7                         6                         5 
# Melanostoma_scalare        Eristalis_pertinax           Syrphus_ribesii 
# 4                         4                         4 
# Xylota_segnis           Eristalis_tenax        Eristalis_abusivus 
# 4                         3                         3 
# Platycheirus_granditarsus        Mallota_fuciformis     Meliscaeva_auricollis 
# 2                         1                         1 
# Platycheirus_splendidus     Platycheirus_scutatus    Helophilus_trivittatus 
# 1                         1                         0 
# Eristalis_interruptus       Eristalis_horticola     Eristalis_intricarius 
# 0                         0                         0 
# Platycheirus_albimanus   Platycheirus_podagratus     Platycheirus_tarsalis 
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

(length(which(Ab_BB >0)))/length(Ab_BB)
#[1] 0.3055556

Ab_BB_gaus <- glmmadmb(Ab_BB ~ Species_control
                        + (1|River/Session), 
                        data = GH, 
                        family = "gaussian")

hist(Ab_BB_gaus$residuals)
shapiro.test(Ab_BB_gaus$residuals)

#Shapiro-Wilk normality test
# #
# data:  Ab_BB_gaus$residuals
# W = 0.87053, p-value = 0.0005866

Ab_BB_pois <- glmmadmb(Ab_BB~ Species_control + 
                          (1|River/Session) , 
                        data = GH, 
                        family = "poisson")

### 
AIC(Ab_BB_pois)
# [1]120.7856

Ab_BB_nbinom <- glmmadmb(Ab_BB ~ Species_control + 
                            (1|River/Session), 
                          data = GH, 
                          family = "nbinom")

AIC(Ab_BB_nbinom)
# [1] 105.1192
######################

Ab_BB_pois_zero <- glmmadmb(Ab_BB~ Species_control + 
                         (1|River/Session) , 
                       data = GH, 
                       family = "poisson",
                       zeroInflation = TRUE)

### 
AIC(Ab_BB_pois_zero)
# [1] 103.0482

Ab_BB_nbinom_zero <- glmmadmb(Ab_BB ~ Species_control + 
                           (1|River/Session), 
                         data = GH, 
                         family = "nbinom",
                         zeroInflation = TRUE)

AIC(Ab_BB_nbinom_zero)
# [1] 104.731

summary(Ab_BB_pois_zero)

#Call:
# glmmadmb(formula = Ab_BB ~ Species_control + (1 | River/Session), 
#          data = GH, family = "poisson", zeroInflation = TRUE)
# 
# AIC: 103 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          1.334      0.377    3.54  0.00041 ***
#   Species_controlGH   -0.456      0.352   -1.30  0.19464    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 

Ab_Coef_results2 <- data.frame(Species = "GH",
                               Coefficient = coef(Ab_BB_nbinom)[2],
                               Confint_L = confint(Ab_BB_nbinom)[2,1],
                               Confint_U = confint(Ab_BB_nbinom)[2,2],
                               SE = 0.352,
                               Pvalue = 0.195,
                               Family = "Zero inflated poisson",
                               modelName = "Bumblebee abundance")

########### Diversity of Bumblebees..

Shann_BB_gaus <- glmmadmb(Shann_BB ~ Species_control
                           + (1|River/Session), 
                           data = GH, 
                           family = "gaussian")

hist(Shann_BB_gaus$residuals)
shapiro.test(Shann_BB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# 
# data:  Shann_BB_gaus$residuals
# W = 0.49501, p-value = 5.595e-10

### non -normal try gamma

Shann_BB_gamma <- glmmadmb(Shann_BB + 0.1 ~ Species_control 
                            + (1|Session/River) , 
                            data = GH, 
                            family = "gamma")

summary(Shann_BB_gamma)

# Call:
# glmmadmb(formula = Shann_BB + 0.1 ~ Species_control + (1 | Session/River), 
#          data = GH, family = "gamma")
# 
# AIC: -50.3 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -1.873      0.206   -9.09   <2e-16 ***
#   Species_controlGH    0.184      0.245    0.75     0.45    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Number of observations: total=32, Session=2, Session:River=8 


#### 
Shann_Coef_results2 <- data.frame(Species = "GH",
                                  Coefficient = coef(Shann_BB_gamma)[2],
                                  Confint_L = confint(Shann_BB_gamma)[2,1],
                                  Confint_U = confint(Shann_BB_gamma)[2,2],
                                  SE = 0.245,
                                  Pvalue = "0.450",
                                  Family = "Gamma",
                                  modelName = "Bumblebee diversity")

###########################################
########### Now solitary Bees

Ab_SB_gaus <- glmmadmb(Ab_SB ~ Species_control
                        + (1|River/Session), 
                        data = GH, 
                        family = "gaussian")

hist(Ab_SB_gaus$residuals)
shapiro.test(Ab_SB_gaus$residuals)

#Shapiro-Wilk normality test

# #data:  Ab_SB_gaus$residuals
# W = 0.65927, p-value = 6.868e-08

Ab_SB_pois <- glmmadmb(Ab_SB~ Species_control + 
                          (1|River/Session) , 
                        data = GH, 
                        family = "poisson")
AIC(Ab_SB_pois)
#[1] 49.6382
### does not resolve

Ab_SB_nbinom <- glmmadmb(Ab_SB ~ Species_control + 
                            (1|River/Session), 
                          data = GH, 
                          family = "nbinom")
AIC(Ab_SB_nbinom)
# [1] 51.6418
### 

Ab_SB_poisson_zero <- glmmadmb(Ab_SB ~ Species_control + 
                           (1|River/Session), 
                         data = GH, 
                         family = "poisson",
                         zeroInflation = TRUE)

AIC(Ab_SB_poisson_zero)
##[1] 51.6382
### this is the only one that resolves 

Ab_SB_nbinom_zero <- glmmadmb(Ab_SB ~ Species_control + 
                           (1|River/Session), 
                         data = GH, 
                         family = "nbinom",
                         zeroInflation = TRUE)

AIC(Ab_SB_nbinom_zero)
## [1] 53.6418
### 

summary(Ab_SB_pois)

# glmmadmb(formula = Ab_SB ~ Species_control + (1 | River/Session), 
#          data = GH, family = "poisson")
# 
# AIC: 49.6 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)         -2.516      1.927   -1.31   0.1916   
# Species_controlGH   -1.674      0.629   -2.66   0.0078 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 

Ab_Coef_results3 <- data.frame(Species = "GH",
                               Coefficient = coef(Ab_SB_pois)[2],
                               Confint_L = confint(Ab_SB_pois)[2,1],
                               Confint_U = confint(Ab_SB_pois)[2,2],
                               SE = 0.629,
                               Pvalue = 0.0078,
                               Family = "Poisson",
                               modelName = "Solitary bee abundance")

########### Diversity of Solitary Bees..

Shann_SB_gaus <- glmmadmb(Shann_SB ~ Species_control
                           + (1|River/Session), 
                           data = GH, 
                           family = "gaussian")

hist(Shann_SB_gaus$residuals)
shapiro.test(Shann_SB_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_SB_gaus$residuals
# W = 0.53965, p-value = 1.84e-09

### non-normal try gamma

Shann_SB_gamma <- glmmadmb(Shann_SB + 0.1 ~ Species_control 
                            + (1|Session/River) , 
                            data = GH, 
                            family = "gamma")

summary(Shann_SB_gamma)

# Call:
# glmmadmb(formula = Shann_SB + 0.1 ~ Species_control + (1 | Session/River), 
#          data = GH, family = "gamma")
# 
# AIC: -103.8 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -2.0122     0.2278   -8.83   <2e-16 ***
#   Species_controlGH  -0.0849     0.0986   -0.86     0.39    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, Session=2, Session:River=8 


#### 
Shann_Coef_results3 <- data.frame(Species = "GH",
                                  Coefficient = coef(Shann_SB_gamma)[2],
                                  Confint_L = confint(Shann_SB_gamma)[2,1],
                                  Confint_U = confint(Shann_SB_gamma)[2,2],
                                  SE = 0.0986,
                                  Pvalue = "0.390",
                                  Family = "Gamma",
                                  modelName = "Solitary bee diversity")

###########################################
########### hoverflies

Ab_HF_gaus <- glmmadmb(Ab_HF ~ Species_control
                        + (1|River/Session), 
                        data = GH, 
                        family = "gaussian")

hist(Ab_HF_gaus$residuals)
shapiro.test(Ab_HF_gaus$residuals)

#Shapiro-Wilk normality test
# 
# data:  Ab_HF_gaus$residuals
# W = 0.86323, p-value = 0.0003896

Ab_HF_pois <- glmmadmb(Ab_HF~ Species_control + 
                          (1|River/Session) , 
                        data = GH, 
                        family = "poisson")

### does not resolve 

Ab_HF_nbinom <- glmmadmb(Ab_HF ~ Species_control + 
                            (1|River/Session), 
                          data = GH, 
                          family = "nbinom")
AIC(Ab_HF_nbinom)
#[1] 214.918

Ab_HF_pois_zero <- glmmadmb(Ab_HF~ Species_control + 
                         (1|River/Session) , 
                       data = GH, 
                       family = "poisson",
                       zeroInflation = T)
AIC(Ab_HF_pois_zero)
## [1] 241.594
### does not resolve 

Ab_HF_nbinom_zero <- glmmadmb(Ab_HF ~ Species_control + 
                           (1|River/Session), 
                         data = GH, 
                         family = "nbinom",
                         zeroInflation = T)

AIC(Ab_HF_nbinom_zero)
# [1] 214.35


summary(Ab_HF_nbinom_zero)

#glmmadmb(formula = Ab_HF ~ Species_control + (1 | River/Session), 
# data = GH, family = "nbinom", zeroInflation = T)
# 
# AIC: 214.3 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          2.261      0.306    7.40  1.4e-13 ***
#   Species_controlGH   -0.914      0.280   -3.27   0.0011 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 

Ab_Coef_results4 <- data.frame(Species = "GH",
                               Coefficient = coef(Ab_HF_nbinom_zero)[2],
                               Confint_L = confint(Ab_HF_nbinom_zero)[2,1],
                               Confint_U = confint(Ab_HF_nbinom_zero)[2,2],
                               SE = 0.280,
                               Pvalue = 0.0011,
                               Family = "Zero inflated Negative binomial",
                               modelName = "Hoverfly abundance")

########### Diversity of hoverflies..

Shann_HF_gaus <- glmmadmb(Shann_HF ~ Species_control
                           + (1|River/Session), 
                           data = GH, 
                           family = "gaussian")

hist(Shann_HF_gaus$residuals)
shapiro.test(Shann_HF_gaus$residuals)

# Shapiro-Wilk normality test
# 
# data:  Shann_HF_gaus$residuals
# W = 0.95142, p-value = 0.1159

### normal

summary(Shann_HF_gaus)

# glmmadmb(formula = Shann_HF ~ Species_control + (1 | River/Session), 
#          data = GH, family = "gaussian")
# 
# AIC: 50.3 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         0.5076     0.1244    4.08  4.5e-05 ***
#   Species_controlGH   0.0411     0.1262    0.33     0.74    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Number of observations: total=36, River=4, River:Session=8 
#### 

Shann_Coef_results4 <- data.frame(Species = "GH",
                                  Coefficient = coef(Shann_HF_gaus)[2],
                                  Confint_L = confint(Shann_HF_gaus)[2,1],
                                  Confint_U = confint(Shann_HF_gaus)[2,2],
                                  SE = 0.126,
                                  Pvalue = "0.740",
                                  Family = "Gaussian",
                                  modelName = "Hoverfly diversity")

###########################################

###########################################
###########

which(Ab_HonB>0)

GH[30,1:12]

### Only one honey bee trapped.  At a control site on the Bann

#   Site Species Species_control Session River    Site_ID Date_of_Trap
# 30 BAN1      GH         Control       B  Bann BAN1estate   03/07/2012
# Inv_status IAS_._Cover Open_Inf SR_flowering_per_10m
# 30     Native           4      104                  4.2
# Unique_currently_flowering_100m
# 30                               5
#


GH_abundance_impact_results <- rbind(Ab_Coef_results1,Ab_Coef_results2,
                                     Ab_Coef_results3, Ab_Coef_results4)

GH_diversity_impact_results <- rbind(Shann_Coef_results1,Shann_Coef_results2,
                                     Shann_Coef_results3, Shann_Coef_results4)



#########
# 
# write.csv(GH_abundance_impact_results, "GH_abundance_impact_results_01_08_2017.csv")
# write.csv(GH_diversity_impact_results, "GH_diversity_impact_results_01_08_2017.csv")

################ Part B: Ordination analysis community data ####

GH<- read.csv("GH_with_flower_data_vr1.csv")


str(GH)
names(GH)
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

Spe <- GH[,13:62]

BB <- Spe[,1:4]
names(BB)
newBB <- BB[,which(colSums(BB)>0)]
newBB
SB <- Spe[,5:12]
names(SB)
newSB <- SB[,which(colSums(SB)>0)]

HoneyB <- Spe[,13]

HF <- Spe[,14:50]
names(HF)
newHF <- HF[,which(colSums(HF)>0)]
names(newHF)
Env <- GH[,1:12]

names(Env)

# [1] "Site"                            "Species"                        
# [3] "Species_control"                 "Session"                        
# [5] "River"                           "Site_ID"                        
# [7] "Date_of_Trap"                    "Inv_status"                     
# [9] "IAS_._Cover"                     "Open_Inf"                       
# [11] "SR_flowering_per_10m"            "Unique_currently_flowering_100m"     


summary(Env$Species_control)
# Control      JK 
# 18      18 

summary(Env$Session)

# A  B 
# 18 18

### For Bumblebees 

### hellinger transform community data. 

hell_BB <- decostand(newBB, method = "hellinger")

mod_cca1 <- rda(hell_BB~ Species_control + Condition(River + Session), data = Env)

anova(mod_cca1, by = "margin", model = "reduced")

# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hell_BB ~ Species_control + Condition(River + Session), data = Env)
#                  Df Variance      F Pr(>F)
# Species_control  1 0.000805 0.1581  0.811
# Residual        30 0.152759 

mod_cca1
# # Call: rda(formula = hell_BB ~ Species_control + Condition(River + Session),
# data = Env)
# 
# Inertia Proportion Rank
# Total         0.2279226  1.0000000     
# Conditional   0.0743581  0.3262428    4
# Constrained   0.0008052  0.0035329    1
# Unconstrained 0.1527593  0.6702243    2
# Inertia is variance 
# 
# Eigenvalues for constrained axes:
#   RDA1 
# 0.0008052 
# 
# Eigenvalues for unconstrained axes:
#   PC1     PC2 
# 0.12813 0.02463 
#####

#### Solitary Bees

hellSB <- decostand(newSB, method = "hellinger")
mod_cca_SB <- rda(hellSB~ Species_control + Condition(River + Session), data = Env)
anova(mod_cca_SB, by = "margin", model = "reduced")

# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hellSB ~ Species_control + Condition(River + Session), data = Env)
# Df Variance      F Pr(>F)  
# Species_control  1  0.00809 2.1235  0.037 *
#   Residual        30  0.11429                
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


################
mod_cca_SB

# Call: rda(formula = hellSB ~ Species_control + Condition(River + Session),
#           data = Env)
# 
#                    Inertia Proportion Rank
# Total         0.18403    1.00000     
# Conditional   0.06165    0.33499    4
# Constrained   0.00809    0.04396    1
# Unconstrained 0.11429    0.62105    5


#### Hoverflies 

hellHF <- decostand(newHF, method = "hellinger")
mod_cca_HF <- rda(hellHF~ Species_control + Condition(River + Session), data = Env)
anova(mod_cca_HF, by = "margin", model = "reduced")

# Permutation test for rda under NA model
# Marginal effects of terms
# Permutation: free
# Number of permutations: 999
# 
# Model: rda(formula = hellHF ~ Species_control + Condition(River + Session), data = Env)
#                 Df Variance      F Pr(>F)
# Species_control  1  0.00662 0.4984  0.857
# Residual        30  0.39833 
mod_cca_HF

# Call: rda(formula = hellHF ~ Species_control + Condition(River + Session),
#           data = Env)
# 
#               Inertia Proportion Rank
# Total         0.533796   1.000000     
# Conditional   0.128851   0.241385    4
# Constrained   0.006618   0.012398    1
# Unconstrained 0.398327   0.746216   17


######################################

### plotting.. 

###########  Calculate variance explained per species by RDA analysis. 

inert_spe <- as.data.frame(inertcomp(mod_cca_SB, display = c("species"),
                                     statistic = c("explained"), proportional = TRUE))


##########

