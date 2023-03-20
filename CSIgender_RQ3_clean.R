#### Women drive efforts to highlight concealable stigmatized identities in U.S. academic science and engineering
### Carly Busch
### March 20, 2023
### Research question 3

library(dplyr)
library(stringr)
library(MASS)
library(reghelper)
library(ordinal)
library(ggplot2)

long_data <- read.csv("CSIgender_long_data23mar20.csv")
long_data2 <- read.csv("CSIgender_long_data223mar20.csv")

long_data2$gender3 <- NA
long_data2[long_data2$gender2 == "man" & !is.na(long_data2$gender2),]$gender3 <- "man"
long_data2[long_data2$gender2 == "woman" & !is.na(long_data2$gender2),]$gender3 <- "woman"

long_data2$race5 <- NA
long_data2[long_data2$race2 == "asian" & !is.na(long_data2$race2),]$race5 <- "asian"
long_data2[long_data2$race2 == "amak" & !is.na(long_data2$race2),]$race5 <- "peer"
long_data2[long_data2$race2 == "black" & !is.na(long_data2$race2),]$race5 <- "peer"
long_data2[long_data2$race2 == "hipi" & !is.na(long_data2$race2),]$race5 <- "peer"
long_data2[long_data2$race2 == "latinx" & !is.na(long_data2$race2),]$race5 <- "peer"
long_data2[long_data2$race2 == "white" & !is.na(long_data2$race2),]$race5 <- "white"
  
long_data2$race5 <- factor(long_data2$race5, 
                           levels = c("asian", "peer",
                                      "white"))
long_data2$race5 <- relevel(long_data2$race5, ref = "white")
                                      
long_data2$age3 <- NA
long_data2[long_data2$age2 == "23-37" & !is.na(long_data2$age2),]$age3 <- "23-49"
long_data2[long_data2$age2 == "38-49" & !is.na(long_data2$age2),]$age3 <- "23-49"
long_data2[long_data2$age2 == "50-59" & !is.na(long_data2$age2),]$age3 <- "50+"
long_data2[long_data2$age2 == "60+" & !is.na(long_data2$age2),]$age3 <- "50+"
                                      
####To what extent are there gender differences for stigma ratings generally, or specific identities?#####
                                      
####STATS: ordinal regression of stigma ratings overall -----
long_data2$stigma <- as.factor(long_data2$stigma)
                                      
stigma_model <- ordinal::clm(stigma ~ gender3 + race5 + age3 + appointment + (1 | ID),
                                                                   data = long_data2[long_data2$csi != "tgnc" & long_data2$csi != "addiction",],
                                                                   Hess = T)
summary(stigma_model)
                                      
                                      
####STATS: ordinal regression of stigma ratings by CSI ----
                                      
long_data$stigma <- as.factor(long_data$stigma)
long_data2$stigma <- as.factor(long_data2$stigma)
                                      
                                      
stigma_ord_out <- do.call(rbind, lapply(c("lgbq", "depress", "anxiety", "ses",
                                          "fgen", "struggle", "disability", "transfer"),
                                        function(x){
                                          mod_out <- as.data.frame(summary(
                                            ordinal::clm(stigma ~ gender3 + race5 + age3 +
                                                           appointment + (1|ID),
                                                         data = long_data2[long_data2$csi == x,],
                                                         Hess = TRUE))$coefficients)
                                          mod_out$csi <- x
                                          mod_out$predictor <- rownames(mod_out)
                                          return(mod_out)
                                          }))
stigma_ord_out <- stigma_ord_out %>%
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(stigma_ord_out) <- NULL
#calculate odds ratios
stigma_ord_out$or <- exp(stigma_ord_out$est)
                                 
stigma_ord_out %>% filter(pval < .05 & predictor != "1|1.5" & predictor != "1.5|2"
                          & predictor != "2|2.5" & predictor != "2.5|3"
                          & predictor != "3|3.5" & predictor != "3.5|4" &
                            predictor != "1|2" & predictor != "2|3" & predictor != "3|4")
                                      
####FIGURE: forest plot odds ratios women stigma ratings ----
                                      
fig3b <- stigma_ord_out %>%
                                        filter(predictor =="gender3woman") %>%
                                        mutate(csi = factor(csi, levels = c("anxiety", "fgen", "depress",
                                                                            "ses", "struggle", "transfer",
                                                                            "disability", "lgbq"))) %>%
                                        ggplot(aes(x = or, y = csi)) +
                                        geom_point(size = 4, color = natparks.pals("Acadia")[2]) +
                                        geom_vline(aes(xintercept = 1), linetype="dashed", size=.5, color = "grey25") + 
                                        geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))), 
                                                       position=ggstance::position_dodgev(height=0.5), alpha = 1, size = 1.5,
                                                       height = .5, color = natparks.pals("Acadia")[2]) +  
                                        labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "") + 
                                        scale_y_discrete(labels = c("struggle" = "struggled academically",
                                                                    "disability" = "has a disability",
                                                                    "ses" = "low SES",
                                                                    "fgen" = "first-generation",
                                                                    "transfer" = "community college transfer",
                                                                    "lgbq" = "LGBQ+",
                                                                    "depress" = "depression"),
                                                         limits = rev) +
                                        theme_classic() +
                                        theme(legend.position = "none", 
                                              axis.title.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"), 
                                              axis.text.x = element_text(family="Helvetica", color = "black", size = 12),
                                              axis.ticks.x = element_blank(),
                                              axis.title.y = element_blank(), 
                                              axis.ticks.y = element_blank(),
                                              axis.line.y = element_blank(),
                                              axis.text.y = element_text(family="Helvetica", color = "black", size = 12),
                                              plot.title = element_blank())
                                      
                                      