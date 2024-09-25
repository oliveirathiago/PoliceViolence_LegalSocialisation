library(tidyverse)
library(haven)
library(lme4)
library(panelr)
library(brglm2)
options(scipen=999)

data <- readRDS(file = 'data/data_reduced.RDS')


##########
data.panel_nested <-
  data %>%
  mutate(across(c(theft, disorder, violent, drugs), ~case_when(
    . == 1 ~ TRUE,
    TRUE ~ FALSE
  ))) %>%
  pivot_longer(
    cols = c(theft, disorder, violent, drugs)
  ) %>%
  mutate(wave_panel = wave - 1) %>%
  panel_data(id = id,
             wave = wave_panel)

m_stop_noleg <-
  wbm(value ~
        stop
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
        )

m_stop_leg <-
  wbm(value ~
        stop
      + legitimacy_PML
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs
      +  bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m_handcuff_noleg <-
  wbm(value ~
        handcuff
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m_handcuff_leg <-
  wbm(value ~
        handcuff
      + legitimacy_PML
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m_beat_noleg <-
  wbm(value ~
        beat
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m_beat_leg <-
  wbm(value ~
        beat
      + legitimacy_PML
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )


dataplot_wbm <-
  # exp to police stops | no legitimacy
  summary(m_stop_noleg)$within_table %>%
  bind_rows(summary(m_stop_noleg)$between_table) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
  filter(var == "legitimacy_PML" | var == "stop" |
           var == "imean(legitimacy_PML)" | var == "imean(stop)") %>%
  mutate(
    var = str_replace(var, "imean", ""),
    var = str_replace(var, "\\(", ""),
    var = str_replace(var, "\\)", ""),
    eff = c('Within effects', 'Between effects'),
    ci.low = coef - 1.96 * se,
    ci.upp = coef + 1.96 * se,
    significance = case_when(
      p < 0.05 ~ "significant",
      TRUE ~ "non.significant"),
    main = 'Police stop',
    legitimacy = 'No'
  ) %>%
  # exp to police stops | yes legitimacy
  bind_rows(
    summary(m_stop_leg)$within_table %>%
      bind_rows(summary(m_stop_noleg)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "stop" |
               var == "imean(legitimacy_PML)" | var == "imean(stop)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Within effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        main = 'Police stop',
        legitimacy = 'Yes')
  ) %>%
  # exp to police arrests | no legitimacy
  bind_rows(
    summary(m_handcuff_noleg)$within_table %>%
      bind_rows(summary(m_handcuff_noleg)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "handcuff" |
               var == "imean(legitimacy_PML)" | var == "imean(handcuff)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        main = 'Police arrest',
        legitimacy = 'No')
      ) %>%
  # exp to police arrests | yes legitimacy
  bind_rows(
    summary(m_handcuff_leg)$within_table %>%
      bind_rows(summary(m_handcuff_noleg)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "handcuff" |
               var == "imean(legitimacy_PML)" | var == "imean(handcuff)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Within effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        main = 'Police arrest',
        legitimacy = 'Yes')
  ) %>%
  # exp to police violence | no legitimacy
  bind_rows(
    summary(m_beat_noleg)$within_table %>%
      bind_rows(summary(m_beat_noleg)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "beat" |
               var == "imean(legitimacy_PML)" | var == "imean(beat)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        main = 'Police violence',
        legitimacy = 'No')
  ) %>%
  # exp to police violence | yes legitimacy
  bind_rows(
    summary(m_beat_leg)$within_table %>%
      bind_rows(summary(m_beat_noleg)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "beat" |
               var == "imean(legitimacy_PML)" | var == "imean(beat)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Within effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        main = 'Police violence',
        legitimacy = 'Yes')
  ) %>%
  mutate(var_plot = case_when(
    var == "stop" ~ "exposure",
    var == "handcuff" ~ "exposure",
    var == "beat" ~ "exposure",
    var == "legitimacy_PML" ~ "legitimacy"),
    main = factor(main, levels = c('Police stop', 'Police arrest', 'Police violence'))
    , legitimacy = factor(legitimacy, levels = c('Yes', 'No'))
    )
      
  

plot_rasch <-  
  ggplot(dataplot_wbm %>% filter(eff == "Within effects"), aes(y = coef, x = var_plot, group = legitimacy, colour = legitimacy)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .15, position = position_dodge(width = .4), lwd = .75, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = legitimacy), 
             size = ifelse(dataplot_wbm[dataplot_wbm$eff == "Within effects",]$significance == "significant", 3, 0), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("","95% confidence interval\ndoes not cross zero")) +
  facet_wrap( ~ main) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-1,1) +
  coord_flip() + 
  ggtitle("Propensity to self-reported deviant behavior") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = "Model includes\nlegitimacy beliefs"),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,2.5)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(size = 11, colour = "#3C3C3C"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 3) + 
  scale_x_discrete(limits = c('legitimacy', 'exposure'),# %>% rev,
                   breaks = c('legitimacy', 'exposure'),# %>% rev,
                   labels = c('Beliefs about the\nlegitimacy of the law', 'Exposure to\npolice behavior'))+#%>% rev) + 
  scale_color_brewer(palette = "Dark2"
                     , breaks = c("No", "Yes")
                     #, labels = c("No", "Yes")
                     )

plot_rasch

pdf("plots/rasch_wbm.pdf", width = 10, height = 10, paper = 'a4r')
plot_rasch
dev.off()


##############

## alternative specs

m1 <-
  wbm(value ~
        stop
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial,
      #, method = "brglmFit"
  )

m2 <-
  wbm(value ~
        stop
      + handcuff
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs
      +  bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m3 <-
  wbm(value ~
        stop
      + handcuff
      + beat
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )

m4 <-
  wbm(value ~
        stop
      + handcuff
      + beat
      + legitimacy_PML
      + exp1 + exp2 + exp3 + exp4
      + homevict_seen + homevict_assault + famfr_robbed + famfr_murdered + famfr_arrest 
      + peer_theft + peer_disorder + peer_violent + peer_drugs + 
        bullying + bullied
      | male + white + privsch
      + wave_panel + wave_panel_sq
      + as_factor(name)
      + wave_panel + wave_panel_sq
      | (wave_panel | id)
      , data = data.panel_nested %>% mutate(wave_panel_sq = wave_panel^2)
      , family = binomial
  )



dataplot_wbm_alt <-
  # exp to police stops | no legitimacy
  summary(m1)$within_table %>%
  bind_rows(summary(m1)$between_table) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
  filter(var == "stop" |
         var == "imean(stop)") %>%
  mutate(
    var = str_replace(var, "imean", ""),
    var = str_replace(var, "\\(", ""),
    var = str_replace(var, "\\)", ""),
    eff = c('Within effects', 'Between effects'),
    ci.low = coef - 1.96 * se,
    ci.upp = coef + 1.96 * se,
    significance = case_when(
      p < 0.05 ~ "significant",
      TRUE ~ "non.significant"),
    model = 'm1'
  ) %>%
  # exp to police stops | yes legitimacy
  bind_rows(
    summary(m2)$within_table %>%
      bind_rows(summary(m2)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "handcuff" | var == "stop" |
               var == "imean(handcuff)" | var == "imean(stop)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Within effects', 'Between effects', 'Between effects'),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        model = "m2")
  ) %>%
  # exp to police arrests | no legitimacy
  bind_rows(
    summary(m3)$within_table %>%
      bind_rows(summary(m3)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "stop" | var == "handcuff" | var == "beat" |
               var == "imean(stop)" | var == "imean(handcuff)" | var == "imean(beat)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Between effects') %>% rep(each = 3),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        model = "m3")
  ) %>%
  # exp to police arrests | yes legitimacy
  bind_rows(
    summary(m4)$within_table %>%
      bind_rows(summary(m4)$between_table) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p) %>%
      filter(var == "legitimacy_PML" | var == "handcuff" | var == "stop" | var == "beat" |
               var == "imean(legitimacy_PML)" | var == "imean(handcuff)" | var == "imean(stop)" | var == "imean(beat)") %>%
      mutate(
        var = str_replace(var, "imean", ""),
        var = str_replace(var, "\\(", ""),
        var = str_replace(var, "\\)", ""),
        eff = c('Within effects', 'Between effects') %>% rep(each = 4),
        ci.low = coef - 1.96 * se,
        ci.upp = coef + 1.96 * se,
        significance = case_when(
          p < 0.05 ~ "significant",
          TRUE ~ "non.significant"),
        model = "m4")
  ) %>%
  mutate(model = factor(model, levels = c('m1', 'm2', 'm3', 'm4') %>% rev))


plot_rasch_alt <-  
  ggplot(dataplot_wbm_alt %>% filter(eff == "Within effects"), aes(y = coef, x = var, group = model, colour = model)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .15, position = position_dodge(width = .4), lwd = .75, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = model), 
             size = ifelse(dataplot_wbm_alt[dataplot_wbm_alt$eff == "Within effects",]$significance == "significant", 3, 0), 
             position = position_dodge(width = .4)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("","95% confidence interval\ndoes not cross zero")) +
  #facet_wrap( ~ main) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-1,1) +
  coord_flip() + 
  ggtitle("Propensity to self-reported offending behavior") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,2.5)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(size = 11, colour = "#3C3C3C"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2) + 
  scale_x_discrete(limits = c('legitimacy_PML', 'beat', 'handcuff', 'stop'),# %>% rev,
                   breaks = c('legitimacy_PML', 'beat', 'handcuff', 'stop'),# %>% rev,
                   labels = c('Beliefs about the\nlegitimacy of the law', 'Exposure to\npolice violence',
                              'Exposure to\npolice arrests', 'Exposure to\npolice stops'))+#%>% rev) + 
  scale_color_brewer(palette = "Dark2"
                     , breaks = c("m1", "m2", "m3", "m4")
                     , labels = c("Model 1", "Model 2", "Model 3", "Model 4")
  )

plot_rasch_alt

pdf("plots/rasch_wbm_new.pdf", width = 10, height = 10, paper = 'a4r')
plot_rasch_alt
dev.off()

#######

dataplot_appendix <-
  summary(m1)$within_table %>%
  mutate(eff = "Within effects") %>%
  bind_rows(summary(m1)$between_table %>%
              mutate(eff = "Between effects")) %>%
  rownames_to_column() %>%
  as_tibble() %>%
  dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p, eff) %>%
  mutate(model = 'm1') %>%
  bind_rows(
    summary(m2)$within_table %>%
      mutate(eff = "Within effects") %>%
      bind_rows(summary(m2)$between_table %>%
                  mutate(eff = "Between effects")) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p, eff) %>%
      mutate(model = 'm2')
  ) %>%
  bind_rows(
    summary(m3)$within_table %>%
      mutate(eff = "Within effects") %>%
      bind_rows(summary(m3)$between_table %>%
                  mutate(eff = "Between effects")) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p, eff) %>%
      mutate(model = 'm3')
  ) %>%
  bind_rows(
    summary(m4)$within_table %>%
      mutate(eff = "Within effects") %>%
      bind_rows(summary(m4)$between_table %>%
                  mutate(eff = "Between effects")) %>%
      rownames_to_column() %>%
      as_tibble() %>%
      dplyr::select(var = rowname, coef = 'Est.', se = 'S.E.', p, eff) %>%
      mutate(model = 'm4')
  ) %>%
  mutate(
    var = str_replace(var, "imean", ""),
    var = str_replace(var, "\\(", ""),
    var = str_replace(var, "\\)", ""),
    ci.low = coef - 1.96 * se,
    ci.upp = coef + 1.96 * se,
    significance = case_when(
      p < 0.05 ~ "significant",
      TRUE ~ "non.significant")) %>%
  filter(var != "Intercept") %>%
  filter(!str_starts(var, "as_factor")) %>%
  mutate(model = factor(model, levels = c('m1', 'm2', 'm3', 'm4') %>% rev),
         eff = factor(eff, levels = c("Within effects", "Between effects")))


plot_rasch_app <-  
  ggplot(dataplot_appendix %>% filter(model == "m4"), aes(y = coef, x = var, group = model, colour = model)) +
  geom_errorbar(aes(ymin = ci.low, ymax = ci.upp, alpha = significance), width = .5, position = position_dodge(width = 2), lwd = .75, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = model), 
             #size = ifelse(dataplot_appendix$significance == "significant", 2, 0), 
             position = position_dodge(width = 2)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("","95% confidence interval\ndoes not cross zero")) +
  facet_wrap( ~ eff) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  ylim(-2,2.5) +
  coord_flip() + 
  ggtitle("Propensity to self-reported offending behavior") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(0,1),
                                                  shape = c(NA,8),
                                                  size = c(0,3)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(size = 11, colour = "#3C3C3C"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2.5) + 
  scale_x_discrete(limits = c('male', '', 'white', '', 'privsch', '',
                              'bullying', '', 'bullied', '',
                              'peer_theft', '', 'peer_disorder', '', 'peer_drugs', '', 'peer_violent', '', 
                              'famfr_arrest', '', 'famfr_murdered', '', 'famfr_robbed', '', 'homevict_seen', '', 'homevict_assault',  '',
                              'exp4', '', 'exp3', '', 'exp2', '', 'exp1',  '',
                              'wave_panel_sq', '', 'wave_panel', '',
                              'legitimacy_PML', '', 'beat', '', 'handcuff', '', 'stop'),# %>% rev,
                   breaks = c('male', '', 'white', '', 'privsch', '',
                              'bullying', '', 'bullied', '',
                              'peer_theft', '', 'peer_disorder', '', 'peer_drugs', '', 'peer_violent', '', 
                              'famfr_arrest', '', 'famfr_murdered', '', 'famfr_robbed', '', 'homevict_seen', '', 'homevict_assault',  '',
                              'exp4', '', 'exp3', '', 'exp2', '', 'exp1',  '',
                              'wave_panel_sq', '', 'wave_panel', '',
                              'legitimacy_PML', '', 'beat', '', 'handcuff', '', 'stop'),# %>% rev,
                   labels = c('Gender: male', '', 'Race: white', '', 'School type: private',  '',
                              'Bullying behavior', '', 'Bullying victimization', '',
                              'Offending behavior among peers:\nTheft', '', 'Offending behavior among peers:\nDisorder', '', 
                              'Offending behavior among peers:\nDrug use', '', 'Offending behavior among peers:\nViolent behavior',  '',
                              'Family victimization: arrests',  '', 'Family victimization: murder', '', 'Family victimization: robbery', '',
                              'Personal victimization: assault', '', "Personal victimization: fights", '',
                              'Seen gunshots', '', 'Seen people carrying guns', '', 'Seen robberies', '', 'Seen people selling drugs', '', 
                              'Age squared', '', 'Age', '',
                              'Beliefs about the\nlegitimacy of the law', '', 'Exposure to police violence', '',
                              'Exposure to police arrests', '', 'Exposure to police stops')) +
  scale_color_brewer(palette = "Dark2"
                     , breaks = c("m1", "m2", "m3", "m4")
                     , labels = c("Model 1", "Model 2", "Model 3", "Model 4")
  )

pdf("plots/rasch_wbm_app.pdf", width = 10, height = 15, paper = 'a4r')
plot_rasch_app
dev.off()

