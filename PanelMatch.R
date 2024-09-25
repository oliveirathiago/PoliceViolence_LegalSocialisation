library(tidyverse)
library(haven)
library(PanelMatch)
options(scipen=999)

data <- 
  readRDS(file = 'data/data_reduced.RDS') %>%
  as.data.frame()

## treatment: stop

m_stop_lead0 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "stop", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 0, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_stop_lead1 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "stop", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 1, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_stop_lead2 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "stop", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 2, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

## treatment: handcuff

m_handcuff_lead0 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "handcuff", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 0, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_handcuff_lead1 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "handcuff", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 1, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_handcuff_lead2 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "handcuff", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 2, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

## treatment: beat

m_beat_lead0 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "beat", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 0, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_beat_lead1 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "beat", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 1, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

m_beat_lead2 <- 
  PanelMatch(lag = 1, time.id = "wave", unit.id = "id", 
             treatment = "beat", refinement.method = "mahalanobis", 
             data = data, match.missing = TRUE, 
             covs.formula = ~ I(lag(legitimacy_PML, 1:1)) + exp1 + exp2 + exp3 + exp4 + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest + peer_theft + peer_disorder + peer_violent + peer_drugs + bullying + bullied + male + white + privsch,
             size.match = 5, qoi = "att", outcome.var = "legitimacy_PML",
             lead = 2, forbid.treatment.reversal = F, 
             use.diagonal.variance.matrix = F)

get_covariate_balance(m_stop_lead0$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                  "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

get_covariate_balance(m_handcuff_lead0$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                  "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

get_covariate_balance(m_beat_lead0$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                    "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

get_covariate_balance(m_stop_lead2$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                  "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

get_covariate_balance(m_handcuff_lead2$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                  "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

get_covariate_balance(m_beat_lead2$att,
                      data = data, covariates = c("exp1", "exp2", "exp3", "exp4", "male", "white", "privsch",
                                                  "homevict_assault", "famfr_robbed", "famfr_murdered", "famfr_arrest",
                                                  "peer_theft", "peer_disorder", "peer_violent", "peer_drugs", "bullying", "bullied"),
                      plot = F)

set.seed(161803)
results.stop_lead0 <- PanelEstimate(sets = m_stop_lead0, data = data)
results.stop_lead1 <- PanelEstimate(sets = m_stop_lead1, data = data)
results.stop_lead2 <- PanelEstimate(sets = m_stop_lead2, data = data)
results.handcuff_lead0 <- PanelEstimate(sets = m_handcuff_lead0, data = data)
results.handcuff_lead1 <- PanelEstimate(sets = m_handcuff_lead1, data = data)
results.handcuff_lead2 <- PanelEstimate(sets = m_handcuff_lead2, data = data)
results.beat_lead0 <- PanelEstimate(sets = m_beat_lead0, data = data)
results.beat_lead1 <- PanelEstimate(sets = m_beat_lead1, data = data)
results.beat_lead2 <- PanelEstimate(sets = m_beat_lead2, data = data)


dataplot <- 
  tibble(
    var = c("Police stop", "Police arrest", "Police violence") %>% rep(each = 3),
    leads = c("Contemporaneous effects", "Effects after one year", "Effects after two years") %>% rep(3) %>% as_factor,
    coef = c(
      results.stop_lead0$estimates, results.stop_lead1$estimates, results.stop_lead2$estimates,
      results.handcuff_lead0$estimates, results.handcuff_lead1$estimates, results.handcuff_lead2$estimates,
      results.beat_lead0$estimates, results.beat_lead1$estimates, results.beat_lead2$estimates
    ),
    ci.low = c(
      quantile(results.stop_lead0$bootstrapped.estimates, 0.025), quantile(results.stop_lead1$bootstrapped.estimates, 0.025), quantile(results.stop_lead2$bootstrapped.estimates, 0.025),
      quantile(results.handcuff_lead0$bootstrapped.estimates, 0.025), quantile(results.handcuff_lead1$bootstrapped.estimates, 0.025), quantile(results.handcuff_lead2$bootstrapped.estimates, 0.025),
      quantile(results.beat_lead0$bootstrapped.estimates, 0.025), quantile(results.beat_lead1$bootstrapped.estimates, 0.025), quantile(results.beat_lead2$bootstrapped.estimates, 0.025)
    ),
    ci.upp = c(
      quantile(results.stop_lead0$bootstrapped.estimates, 0.975), quantile(results.stop_lead1$bootstrapped.estimates, 0.975), quantile(results.stop_lead2$bootstrapped.estimates, 0.975),
      quantile(results.handcuff_lead0$bootstrapped.estimates, 0.975), quantile(results.handcuff_lead1$bootstrapped.estimates, 0.975), quantile(results.handcuff_lead2$bootstrapped.estimates, 0.975),
      quantile(results.beat_lead0$bootstrapped.estimates, 0.975), quantile(results.beat_lead1$bootstrapped.estimates, 0.975), quantile(results.beat_lead2$bootstrapped.estimates, 0.975)
    )
  ) %>% 
  mutate(significance = case_when(
    ci.upp < 0 & ci.low < 0 ~ "significant",
    ci.upp > 0 & ci.low > 0 ~ "significant",
    TRUE ~ "non significant"),
         var = factor(var, levels = rev(c("Police stop", "Police arrest", "Police violence"))),
         leads = factor(leads, levels = rev(c("Contemporaneous effects", "Effects after one year", "Effects after two years"))))

plot_matchingdid <-  
  ggplot(dataplot, aes(y = coef, x = var, group = leads, color = leads)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .25, position = position_dodge(width = .4), lwd = .75, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = leads), 
             size = ifelse(dataplot$significance == "significant", 3, 0), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("","95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .5, color = 'darkgray') + 
  ylim(-2,2) +
  coord_flip() + 
  ggtitle("Effects of exposure to policing on legitimacy_PML beliefs") + 
  ylab("") + xlab("") + 
  guides(alpha = "none", color = guide_legend(title = ""),
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,4)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2) + 
  scale_color_brewer(palette = "Set1",
                     limits = c("Contemporaneous effects", "Effects after one year", "Effects after two years"),
                     breaks = c("Contemporaneous effects", "Effects after one year", "Effects after two years")) + 
  scale_x_discrete(limits = c("Police stop", "Police arrest", "Police violence") %>% rev,
                   breaks = c("Police stop", "Police arrest", "Police violence") %>% rev)

pdf("plots/matching_did.pdf", width = 10, height = 10, paper = 'a4r')
plot_matchingdid
dev.off()

## should we just report F = 0 as conteporaneous effects and F = 2 as long-term/cumulative effects?

dataplot_new <-
  dataplot %>%
  filter(leads != "Effects after one year") %>%
  mutate(leads = case_when(
    leads == "Contemporaneous effects" ~ "Contemporaneous effects",
    leads == "Effects after two years" ~ "Cumulative effects",
    TRUE ~ leads
  )) %>%
  mutate(
    leads = factor(leads, levels = rev(c("Contemporaneous effects", "Cumulative effects"))))

plot_matchingdid_new <-  
  ggplot(dataplot_new, aes(y = coef, x = var, group = leads, color = leads)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .25, position = position_dodge(width = .4), lwd = .75, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = leads), 
             size = ifelse(dataplot_new$significance == "significant", 3, 0), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("","95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-1,1) +
  coord_flip() + 
  ggtitle("Effects of exposure to policing on legitimacy beliefs") + 
  ylab("") + xlab("") + 
  guides(alpha = "none", color = guide_legend(title = ""),
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,4)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 12),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.key = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 1.25) + 
  scale_color_brewer(palette = "Dark2",
                     limits = c("Contemporaneous effects", "Cumulative effects"),
                     breaks = c("Contemporaneous effects", "Cumulative effects")) + 
  scale_x_discrete(limits = c("Police stop", "Police arrest", "Police violence") %>% rev,
                   breaks = c("Police stop", "Police arrest", "Police violence") %>% rev)

pdf("plots/matching_did_new.pdf", width = 10, height = 10, paper = 'a4r')
plot_matchingdid_new
dev.off()