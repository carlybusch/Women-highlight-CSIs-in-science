#### Women drive efforts to highlight concealable stigmatized identities in U.S. academic science and engineering
### Carly Busch
### March 20, 2023
### Research question 2

library(tidyverse)
library(dplyr)
library(stringr)
library(MASS)
library(reghelper)
library(ordinal)
library(nnet)
library(ggstance)
library(car)
library(NatParksPalettes)


my_data <- read.csv("CSIgender_deID_data23mar20.csv")
long_data <- read.csv("CSIgender_long_data23mar20.csv")


####To what extent are there gender differences in revealing CSIs?####

####TABLE of extent of reveal for all csis individually by gender----
gender_extent_out_fxn <- function(x, y, z){
  tmp <- data.frame(sum(my_data[[x]], na.rm = T))
  tmp$total_women <- sum(my_data[my_data$gender3 == "woman",][[x]], na.rm = T)
  tmp$total_men <- sum(my_data[my_data$gender3 == "man",][[x]], na.rm = T)
  tmp$id <- x
  tmp$csi <- y
  tmp$conceal_count <- as.data.frame(table(my_data[my_data[[x]] == 1,][[y]])) %>%
    subset(Var1 == "yes", select = "Freq")
  tmp$conceal_count_women <- as.data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender3 == "woman",][[y]])) %>%
    subset(Var1 == "yes", select = "Freq")
  tmp$conceal_count_men <- as.data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender3 == "man",][[y]])) %>%
    subset(Var1 == "yes", select = "Freq")
  colnames(tmp) <- c("total_count", "total_women", "total_men", "id", "csi", "conceal_count",
                     "conceal_count_women", "conceal_count_men")
  tmp$reveal_all <- as.numeric(ifelse(length(data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                               subset(Var1 == "all", select = "Freq")) == 0, 0, 
                                      data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                        subset(Var1 == "all", select = "Freq")))
  tmp$reveal_some <- as.numeric(ifelse(length(data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                                subset(Var1 == "some", select = "Freq")) == 0, 0,
                                       data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                         subset(Var1 == "some", select = "Freq")))
  tmp$reveal_none <- as.numeric(ifelse(length(data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                                subset(Var1 == "none", select = "Freq")) == 0, 0,
                                       data.frame(table(my_data[my_data[[x]] == 1,][[z]]))%>%
                                         subset(Var1 == "none", select = "Freq")))
  tmp$csi <- str_split(tmp$csi, "\\.")[[1]][1]
  tmp$id <- str_split(tmp$id, "2")[[1]][1]
  tmp$reveal_all_women <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "all", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "all", select = "Freq")))
  tmp$reveal_some_women <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "some", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "some", select = "Freq")))
  tmp$reveal_none_women <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "none", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "woman",][[z]]))%>%
      subset(Var1 == "none", select = "Freq")))
  tmp$reveal_all_men <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "all", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "all", select = "Freq")))
  tmp$reveal_some_men <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "some", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "some", select = "Freq")))
  tmp$reveal_none_men <- as.numeric(ifelse(length(data.frame(table(
    my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "none", select = "Freq")) == 0, 0,
    data.frame(table(my_data[my_data[[x]] == 1 & my_data$gender2 == "man",][[z]]))%>%
      subset(Var1 == "none", select = "Freq")))  
  return(tmp)
}


demo_clean <- c("lgbq",
                "depression2", "anxiety2", "struggle2",
                "lowses", "transfer2", "fgen",
                "disability2")

concealable_colnames <- c("lgbq.conceal",
                          "depress.conceal",
                          "anxiety.conceal",
                          "struggle.conceal",
                          "ses.conceal",
                          "transfer.conceal",
                          "fgen.conceal",
                          "disability.conceal")

revealugs_colnames <- c("lgbq.reveal.ugs",
                        "depress.reveal.ugs",
                        "anxiety.reveal.ugs",
                        "struggle.reveal.ugs",
                        "ses.reveal.ugs",
                        "transfer.reveal.ugs",
                        "fgen.reveal.ugs",
                        "disability.reveal.ugs")

gender_extent_out_table <- data.frame(matrix(NA, ncol = 17, nrow = length(demo_clean)))
colnames(gender_extent_out_table) <- c("total_count", "total_women", "total_men", "csi", "id", "concealable_count", "concealable_count_women", "concealable_count_men", "reveal_all", "reveal_some", "reveal_none", "reveal_all_women", "reveal_some_women", "reveal_none_women", "reveal_all_men", "reveal_some_men", "reveal_none_men")

for(i in 1:length(demo_clean)){
  
  gender_extent_out_table[i,] <- gender_extent_out_fxn(demo_clean[i], 
                                                       concealable_colnames[i], 
                                                       revealugs_colnames[i])
  
}

gender_extent_out_table$total_count <- as.numeric(gender_extent_out_table$total_count)
gender_extent_out_table$total_women <- as.numeric(gender_extent_out_table$total_women)
gender_extent_out_table$total_men <- as.numeric(gender_extent_out_table$total_men)
gender_extent_out_table$concealable_count <- as.numeric(gender_extent_out_table$concealable_count)
gender_extent_out_table$concealable_count_women <- as.numeric(gender_extent_out_table$concealable_count_women)
gender_extent_out_table$concealable_count_men <- as.numeric(gender_extent_out_table$concealable_count_men)
gender_extent_out_table$out_total <- gender_extent_out_table$reveal_all +
  (gender_extent_out_table$total_count - gender_extent_out_table$concealable_count)
gender_extent_out_table$out_total_women <- gender_extent_out_table$reveal_all_women +
  (gender_extent_out_table$total_women - gender_extent_out_table$concealable_count_women)
gender_extent_out_table$out_total_men <- gender_extent_out_table$reveal_all_men +
  (gender_extent_out_table$total_men - gender_extent_out_table$concealable_count_men)
gender_extent_out_table$reveal_sum <-  rowSums(gender_extent_out_table[,c("out_total", "reveal_some",
                                                                          "reveal_none")])
gender_extent_out_table$reveal_sum_women <-  rowSums(gender_extent_out_table[,c("out_total_women",
                                                                                "reveal_some_women", "reveal_none_women")])
gender_extent_out_table$reveal_sum_men <-  rowSums(gender_extent_out_table[,c("out_total_men",
                                                                              "reveal_some_men", "reveal_none_men")])

####TABLE of extent of reveal for csis in aggregate by gender-----

gender_out_aggregate <- as.data.frame(table(long_data$extent_out, long_data$gender3))
colnames(gender_out_aggregate) <- c("extent_out", "gender", "count")
gender_out_aggregate$total <- NA
gender_out_aggregate[gender_out_aggregate$gender == "man",]$total <- nrow(long_data[long_data$gender3 == "man" & !is.na(long_data$gender3),])
gender_out_aggregate[gender_out_aggregate$gender == "woman",]$total <- nrow(long_data[long_data$gender3 == "woman" & !is.na(long_data$gender3),])

####STATS multinomial regression extent of reveal for csis in aggregate -----
long_data$race5 <- factor(long_data$race5, 
                          levels = c("asian", "peer",
                                     "white"))
long_data$race5 <- relevel(long_data$race5, ref = "white")
long_data$extent_out <- factor(long_data$extent_out, levels = c("none", "some", "all"), ordered = F)
long_data$extent_out <- relevel(long_data$extent_out, ref = "none")
reveal_model <- multinom(extent_out ~ gender3 + race5 + age3 + appointment + (1 | ID),
                                                              data = long_data,
                                                              na.action = na.omit) 
summary(reveal_model)
z <- summary(reveal_model)$coefficients/summary(reveal_model)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
p<.05
reveal.m.df <- as.data.frame(summary(reveal_model)$coefficients)
some.df <- reveal.m.df[1,]
some.df<-some.df %>% pivot_longer(cols = everything())
some.df$extent<- "some"
some.df <- some.df %>%
dplyr::rename(predictor = name, est = value) %>%
  as.data.frame()
some.df$pval <- p[1,]
some.df$se <- summary(reveal_model)$standard.errors[1,]
                                     
all.df <- reveal.m.df[2,]
all.df<-all.df %>% pivot_longer(cols = everything())
all.df$extent<- "all"
all.df <- all.df %>%
dplyr::rename(predictor = name, est = value) %>%
  as.data.frame()
all.df$pval <- p[2,]
all.df$se <- summary(reveal_model)$standard.errors[2,]
                                     
reveal.mod <- rbind(some.df, all.df)
reveal.mod <- reveal.mod %>% filter(predictor != "(Intercept)" & predictor != "1 | IDTRUE")
reveal.mod$or <- exp(reveal.mod$est)
                                     
reveal.mod %>% filter(pval < .05)
                                     
####STATS: multinomial regression extent out by gender per csi----
                                     
csi_reveal_mdf <- do.call(rbind, lapply(c("lgbq", "depress", "anxiety", "ses",
                                          "fgen", "struggle", "disability", "transfer"), 
                                        function(x){
                                          mod_out <- as.data.frame(summary(
                                            multinom(formula = extent_out ~ gender3 + race5 +
                                                       age3 + appointment + (1|ID),
                                                     data = long_data[long_data$csi == x,],
                                                     na.action = na.omit))$coefficients)
                                          z <- summary(multinom(formula = extent_out ~ gender3 + race5 +
                                                                  age3 + appointment + (1|ID),
                                                                data = long_data[long_data$csi == x,],
                                                                na.action = na.omit)
                                                       )$coefficients/summary(
                                                         multinom(formula = extent_out ~ gender3 +
                                                                    race5 + age3 + appointment +
                                                                    (1|ID),
                                                                  data = long_data[long_data$csi == x,],
                                                                  na.action = na.omit)
                                                         )$standard.errors
                                          p <- (1 - pnorm(abs(z), 0, 1))*2
                                          mod1 <- mod_out[1,]
                                          mod1<-mod1 %>% pivot_longer(cols = everything())
                                          mod1$extent<- "some"
                                          mod1 <- mod1 %>%
                                            dplyr::rename(predictor = name, est = value) %>%
                                            as.data.frame()
                                          mod1$pval <- p[1,]
                                          mod1$se <- summary(multinom(formula = extent_out ~ gender3 + 
                                                                        race5 + age3 + appointment + (1|ID),
                                                                      data = long_data[long_data$csi == x,],
                                                                      na.action = na.omit)
                                                             )$standard.errors[1,]
                                          mod2 <- mod_out[2,]
                                          mod2<-mod2 %>% pivot_longer(cols = everything())
                                          mod2$extent<- "all"
                                          mod2 <- mod2 %>%
                                            dplyr::rename(predictor = name, est = value) %>%
                                            as.data.frame()
                                          mod2$pval <- p[2,]
                                          mod2$se <- summary(multinom(formula = extent_out ~ gender3 + 
                                                                        race5 + age3 + appointment + (1|ID),
                                                                      data = long_data[long_data$csi == x,],
                                                                      na.action = na.omit)
                                                             )$standard.errors[2,]
                                          mod_df <- rbind(mod1, mod2)
                                          mod_df <- mod_df %>%
                                            filter(predictor != "(Intercept)" & predictor != "1 | IDTRUE")
                                          mod_df$csi <- x
                                          return(mod_df)
                                          }))
                                     
csi_reveal_mdf$or <- exp(csi_reveal_mdf$est)
csi_reveal_mdf %>% filter(pval < .05) %>% filter(predictor == "gender3woman")

####FIGURE: multinomial forest plot odds ratios women reveal CSIs ----
                                     
fig2d <- reveal.mod %>%
                                       filter(predictor == "gender3woman") %>%
                                       mutate(psig = ifelse(pval < .05, "sig", "nonsig")) %>%
                                       ggplot(aes(x = exp(est), y = extent, color = extent, alpha = psig)) +
                                       geom_point(size = 4) +
                                       geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.5, color = "grey25") + 
                                       geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))),
                                                      size = 1.5,
                                                      height = .5) +
                                       labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "", color = "Extent of\nReveal") + 
                                       scale_color_manual(breaks = c("some", "all"),
                                                          values = c("all" = natparks.pals("CraterLake")[5],
                                                                     "some" = natparks.pals("CraterLake")[6])) +
                                       scale_alpha_manual(values = c(.6, 1)) +
                                       scale_x_log10(limits = c(.7, 2), breaks = seq(.7, 2, by = .3)) +
                                       guides(alpha = "none") +
                                       theme_classic() +
                                       theme(legend.position = "right", 
                                             axis.title.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"), 
                                             axis.text.x = element_text(family="Helvetica", color = "black", size = 12),
                                             legend.text = element_text(family="Helvetica", color = "black", size = 12),
                                             legend.title = element_text(family="Helvetica", color = "black", size = 12),
                                             axis.ticks.x = element_blank(),
                                             axis.title.y = element_blank(), 
                                             axis.ticks.y = element_blank(),
                                             axis.line.y = element_blank(),
                                             axis.text.y = element_blank())

####FIGURE: multinomial forest plot OR women reveal per CSI-----
                                     
                                     fig2b <- csi_reveal_mdf %>%
                                       filter(predictor == "gender3woman") %>%
                                       mutate(csi = factor(csi, levels = c("anxiety", "fgen", "depress",
                                                                           "ses", "struggle", "transfer",
                                                                           "disability", "lgbq"))) %>%
                                       mutate(psig = ifelse(pval < .05, "sig", "nonsig")) %>%
                                       ggplot(aes(x = exp(est), y = extent, color = extent, alpha = psig)) +
                                       geom_point(size = 3) +
                                       geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.5, color = "grey25") + 
                                       geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))),
                                                      size = 1.5,
                                                      height = .5) +  
                                       scale_x_log10(breaks = c(.1, 1, 3, 5, 10)) +
                                       labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "", color = "Extent of\nReveal") + 
                                       scale_color_manual(breaks = c("some", "all"),
                                                          values = c("all" = natparks.pals("CraterLake")[5],
                                                                     "some" = natparks.pals("CraterLake")[6])) +
                                       scale_alpha_manual(values = c(.6, 1)) +
                                       guides(alpha = "none") +
                                       facet_wrap(.~csi, ncol = 1, strip.position = "left",
                                                  labeller = labeller(csi = c("anxiety" = "anxiety",
                                                                              "fgen" = "first-generation",
                                                                              "depress" = "depression",
                                                                              "ses" = "low SES",
                                                                              "struggle" = "struggled\nacademically",
                                                                              "transfer" = "community college\ntransfer",
                                                                              "disability" = "has a disability",
                                                                              "lgbq" = "LGBQ+"))) +
                                       theme_classic() +
                                       theme(legend.position = "right", 
                                             axis.title.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"), 
                                             axis.text.x = element_text(family="Helvetica", color = "black", size = 12),
                                             legend.text = element_text(family="Helvetica", color = "black", size = 12),
                                             legend.title = element_text(family="Helvetica", color = "black", size = 12),
                                             strip.text.y.left = element_text(family="Helvetica", color = "black", size = 12, angle = 0),
                                             strip.background = element_blank(),
                                             axis.ticks.x = element_blank(),
                                             axis.title.y = element_blank(), 
                                             axis.ticks.y = element_blank(),
                                             axis.line.y = element_blank(),
                                             axis.text.y = element_blank())
                                     
                                     
                                     
####FIGURE: stacked bar graph aggregate out to all, some, none ----
                                     
                                     fig2c <- gender_out_aggregate %>%
                                       filter(gender != "nb") %>%
                                       mutate(gender = str_replace(gender, "man", "men")) %>%
                                       mutate(extent_out = factor(extent_out, levels = c("none", "some", "all"))) %>%
                                       ggplot(aes(x = gender)) +
                                       geom_col(aes(y = round(count/total, 3), fill = extent_out), position = "stack",
                                                color = natparks.pals("Acadia")[2]) +
                                       scale_fill_manual(breaks = c("all", "some", "none"),
                                                         values = c("none" = "white",
                                                                    "some" = alpha(natparks.pals("Acadia")[2], .75),
                                                                    "all" = natparks.pals("Acadia")[2])) +
                                       scale_y_continuous(expand = c(0,0), labels = scales::percent) +
                                       scale_x_discrete(labels =  c("lowses" = "low SES growing up", 
                                                                    "disability" = "has a disability",
                                                                    "fgen" = "first-gen",
                                                                    "struggle" = "struggled academically",
                                                                    "transfer" = "CC transfer student",
                                                                    "depress" = "depression", "lgbq" = "LGBQ+")) +
                                       labs(y = "Percent", x = "", fill = "Extent of\nReveal") +
                                       theme_classic() +
                                       theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
                                             axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
                                             legend.position = "right",
                                             legend.text = element_text(family="Helvetica", color = "black", size = 12),
                                             legend.title = element_text(family="Helvetica", color = "black", size = 12)) +
                                       coord_flip()
                                     
                                     
                                     