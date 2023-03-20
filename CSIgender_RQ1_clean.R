#### Women drive efforts to highlight concealable stigmatized identities in U.S. academic science and engineering
### Carly Busch
### March 20, 2023
### Research question 1

library(stringr)
library(MASS)
library(reghelper)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(NatParksPalettes)

my_data <- read.csv("CSIgender_deID_data23mar20.csv")


####To what extent are there gender differences in reporting CSIs?####
####TABLE: demographics by gender----
gender_demo_fxn <- function(x){
  tmp <- data.frame(my_data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)), 
                "my_data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo$`Percent (n)` <- paste0(demo$perc, " (", demo$count, ")")
  demo <- arrange(demo, desc(count))
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  demo$gender <- "all"
  demo$denom <- NA
  tmp2 <- data.frame(table(my_data[[x]], my_data$gender2))
  colnames(tmp2) <- c("demo", "gender", "count")
  tmp2$name <- x
  tmp2$count <- as.numeric(tmp2$count)
  tmp2$demo <- stringr::str_to_title(tmp2$demo)
  tmp2$denom <- NA
  tmp2[tmp2$gender == "man",]$denom <- data.frame(aggregate(tmp2$count, list(tmp2$gender), FUN = sum))[1,2]
  tmp2[tmp2$gender == "woman",]$denom <- data.frame(aggregate(tmp2$count, list(tmp2$gender), FUN = sum))[3,2]
  tmp2[tmp2$gender == "nb",]$denom <- data.frame(aggregate(tmp2$count, list(tmp2$gender), FUN = sum))[2,2]
  tmp2$perc <- round((tmp2$count/tmp2$denom)*100, 2)
  tmp2$`Percent (n)` <- paste0(tmp2$perc, " (", tmp2$count, ")")
  demo2 <- rbind(demo, tmp2)
  return(demo2)
}

gender_demo_table <- do.call(rbind, lapply(c("lgbq",
                                             "race", "depression",
                                             "anxiety", "ses", "addiction",
                                             "gen.stat",
                                             "struggle","disability",
                                             "transfer"),
                                           gender_demo_fxn))

gender_csi_table <- gender_demo_table
gender_csi_table$csi <- NA
gender_csi_table[gender_csi_table$demo == "Someone Who Has Or Has Had Depression", ]$csi <- "depression"
gender_csi_table[gender_csi_table$demo == "Someone Who Has Or Has Had Anxiety", ]$csi <- "anxiety"
gender_csi_table[gender_csi_table$demo == "Someone Who Has Or Has Had An Addiction To Drugs Or Alcohol", ]$csi <- "addiction"
gender_csi_table[gender_csi_table$demo == "A First-Generation College Student", ]$csi <- "fgen"
gender_csi_table[gender_csi_table$demo == "I Transferred To A 4-Year Institution From A 2-Year College, A Community College, A Junior College, Or A Technical College", ]$csi <- "transfer"
gender_csi_table[gender_csi_table$demo == "Having A Disability", ]$csi <- "disability"
gender_csi_table[gender_csi_table$demo == "Someone Who Struggled Academically In College", ]$csi <- "struggled"
gender_csi_table[gender_csi_table$demo == "Lowincome", ]$csi <- "ses"

gender_csi_table[gender_csi_table$demo == "1" & gender_csi_table$name == "lgbq", ]$csi <- "lgbq"

gender_csi_table <- gender_csi_table %>% filter(!is.na(csi)) %>%
  dplyr::select(c("csi", "gender", "count", "denom", "perc", `Percent (n)`))

####FIGURE: prevalence of CSIs overall and by gender----

fig1a <- gender_csi_table %>%
  filter(gender != "nb" & gender != "all") %>% 
  mutate(gender = str_replace(gender, "man", "men")) %>%
  filter(csi != "addiction" & csi != "parent" & csi != "tgnc") %>%
  mutate(csi = factor(csi, levels = c("lgbq", "disability", "transfer",
                                      "struggled", "ses", "depression", "fgen",
                                      "anxiety"))) %>%
  mutate(gender = factor(gender, levels = c("women", "men"))) %>%
  ggplot(aes(x = perc/100, y = csi, fill = gender)) +
  geom_col(position = "dodge2") +
  geom_text(aes(label = paste0(formatC(round(perc, 1), format = "f", digits = 1), "%")), 
            position=position_dodge(width=0.9), size=4, fontface="bold", color = "black", hjust = -.1) +
  labs(x = "Percent of group", y = "") +
  scale_x_continuous(expand = c(0,0), limits = c(0, .5), breaks = seq(0, 1, .20), labels = scales::percent) +
  scale_y_discrete(labels = c("struggled" = "struggled \nacademically",
                              "ses" = "low SES",
                              "fgen" = "first-generation",
                              "transfer" = "community college\ntransfer",
                              "lgbq" = "LGBQ+",
                              "disability" = "has a disability")) +
  scale_fill_manual(name = "Gender",
                    breaks = c("men", "women"),
                    values = c("men" = natparks.pals("Volcanoes")[3],
                               "women" = natparks.pals("DeathValley")[3])) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
        axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 12),
        legend.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"))



####STATS: logistic regression of CSI by gender ----
my_data$race5 <- factor(my_data$race5, 
                        levels = c("asian", "peer",
                                   "white"))
                                   
my_data$race5 <- relevel(my_data$race5, ref = "white")

logistic_csi_gender <- do.call(rbind, lapply(c("lgbq", "depression2",
                                               "anxiety2", "lowses",
                                               "fgen", "struggle2","disability2",
                                               "transfer2"), function(x){
                                                 tmp.mod <- as.data.frame(summary(glm(formula = as.formula(paste0(x, "~ gender3 + race5 + age3 + appointment")), 
                                                                                      data = my_data))$coefficients[,-3])
                                                 tmp.mod$csi <- x
                                                 tmp.mod$predictor <- rownames(tmp.mod)
                                                 tmp.mod$stdest <- beta(glm(formula = as.formula(paste0(x, "~ gender3 + race5 + age3 + appointment")), data = my_data))$coefficients[,1]
                                                 return(tmp.mod)
                                               })) 

logistic_csi_gender <- logistic_csi_gender %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
logistic_csi_gender$or <- exp(logistic_csi_gender$est)


####FIGURE: forest plot log reg csi by gender ----
fig1b <- logistic_csi_gender %>%
  filter(predictor =="gender3woman") %>%
  mutate(csi = factor(csi, levels = c("lgbq", "disability2", "transfer2",
                                      "struggle2", "lowses", "depression2", "fgen",
                                      "anxiety2"))) %>%
  ggplot(aes(x = or, y = csi)) +
  geom_point(size = 2, color = natparks.pals("Acadia")[2]) +
  geom_vline(aes(xintercept = 1), linetype="dashed", size=.5, color = "grey25") + 
  geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))), 
                 position=ggstance::position_dodgev(height=0.5), alpha = 1, size = 1,
                 height = .5, color = natparks.pals("Acadia")[2]) +  
  labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "") + 
  scale_y_discrete(labels = c("struggle2" = "struggled \nacademically",
                              "lowses" = "low SES",
                              "fgen" = "first-generation",
                              "transfer2" = "community college\ntransfer",
                              "lgbq" = "LGBQ+",
                              "anxiety2" = "anxiety",
                              "disability2" = "has a disability",
                              "depression2" = "depression")) +
  scale_x_log10(limits = c(.9, 1.2)) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 12),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12),
        title = element_blank())

