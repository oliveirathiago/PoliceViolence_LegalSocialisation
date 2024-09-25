library(haven)
library(tidyverse)
library(readxl)
library(lavaan)
library(GGally)
library(flextable)

# Load data
w1 <- read_dta('data/SPLSS_w1.dta')               # first wave
w2 <- read_dta('data/SPLSS_w2.dta')               # second wave
w3 <- read_dta('data/SPLSS_w3.dta')               # third wave
w4 <- read_dta('data/SPLSS_w4.dta')               # fourth wave
ac <- read_excel('data/Jovens_DadosAC.xlsx')      # key areas info

# Cleaning data
names(w1) <- tolower(names(w1))
names(w2) <- tolower(names(w2))
names(w3) <- tolower(names(w3))
names(w4) <- tolower(names(w4))

w1$sbjnum <- w1$sbjnum %>% as.character()
w2$sbjnum <- w2$sbjnum %>% as.character()
w3$sbjnum <- w3$sbjnum %>% as.character()
w4$sbjnum <- w4$sbjnum %>% as.character()

w1$codigo_entidade <- w1$codigo_entidade %>% as.factor()
w2$dados_escola_6 <- w2$dados_escola_6 %>% as.factor()
w3$dados_escola_6 <- w3$dados_escola_6 %>% as.factor()
w4$dados_escola_6 <- w4$dados_escola_6 %>% as.factor()

# Filter relevant variables
w1 <- w1 %>%
  select(id = sbjnum, school = codigo_entidade, race = p35, privsch = dependencia, male = sexo_aluno,
         duty1 = p2601, duty2 = p2602, rule1 = p2603, rule2 = p2604, rule3 = p2606,
         exp1 = p2901, exp2 = p2902, exp3 = p2903, exp4 = p2904,
         stop = p3001, handcuff = p3002, beat = p3003,
         pj1 = p3201, pj2 = p3202, pj3 = p3203, pj4 = p3204,
         theft = p2501, disorder = p2502, violent = p2503, drugs = p2504,
         homevict_seen = p1101, homevict_assault = p1103,
         famfr_robbed = p1201, famfr_murdered = p1202, famfr_arrest = p1203,
         peer_theft = p1501, peer_disorder = p1502, peer_violent = p1503, peer_drugs = p1504,
         bullying1 = p1601, bullying2 = p1602, bullying3 = p1603, bullying4 = p1604, bullying5 = p1605,
         bullied1 = p1701, bullied2 = p1702, bullied3 = p1703, bullied4 = p1704, bullied5 = p1705) %>%
  mutate(panelid = paste(id, ".", 1),
         privsch = privsch == "4 - Privada",
         white = race == 1,
         male = male == 'MASCULINO',
         duty1 = duty1 == 1,
         duty2 = duty2 == 0,
         rule1 = rule1 == 1,
         rule2 = rule2 == 0,
         rule3 = rule3 == 1,
         stop  = stop  == 1,
         handcuff = handcuff == 1,
         beat  = beat  == 1,
         exp1 = exp1 == 1 | exp1 == 2,
         exp2 = exp2 == 1 | exp2 == 2,
         exp3 = exp3 == 1 | exp3 == 2,
         exp4 = exp4 == 1 | exp4 == 2,
         pj1 = pj1 == 1,
         pj2 = pj2 == 1,
         pj3 = pj3 == 1,
         pj4 = pj4 == 1)

w2 <- w2 %>%
  select(id = sbjnum,
         duty1 = p2201, duty2 = p2202, rule1 = p2203, rule2 = p2204, rule3 = p2206,
         exp1 = p2801, exp2 = p2802, exp3 = p2803, exp4 = p2804,
         stop = p2901, handcuff = p2902, beat = p2903,
         pj1 = p3101, pj2 = p3102, pj3 = p3103, pj4 = p3104,
         theft = p2101, disorder = p2102, violent = p2103, drugs = p2104,
         homevict_seen = p901, homevict_assault = p903,
         famfr_robbed = p1001, famfr_murdered = p1002, famfr_arrest = p1003,
         peer_theft = p1101, peer_disorder = p1102, peer_violent = p1103, peer_drugs = p1104,
         bullying1 = p1201, bullying2 = p1202, bullying3 = p1203, bullying4 = p1204, bullying5 = p1205,
         bullied1 = p1301, bullied2 = p1302, bullied3 = p1303, bullied4 = p1304, bullied5 = p1305
         ) %>%
  mutate(panelid = paste(id, ".", 2),
         duty1 = duty1 == 3 | duty1 == 4,   # 1 = duty; 0 = no duty
         duty2 = duty2 == 1 | duty2 == 2,   # 1 = duty; 0 = no duty
         rule1 = rule1 == 3 | rule1 == 4,   # 1 = positive views on the rule of law
         rule2 = rule2 == 1 | rule2 == 2,   # 1 = positive views on the rule of law
         rule3 = rule3 == 3 | rule3 == 4,   # 1 = positive views on the rule of law
         stop = stop != 0,                    # 1 = seen police stop
         handcuff = handcuff != 0,            # 1 = seen police handcuff
         beat = beat != 0,                    # 1 = seen police beating,
         exp1 = exp1 == 2 | exp1 == 3 | exp1 == 4,      # 1 = exposed to violence: drugs
         exp2 = exp2 == 2 | exp2 == 3 | exp2 == 4,      # 1 = exposed to violence: robbery
         exp3 = exp3 == 2 | exp3 == 3 | exp3 == 4,      # 1 = exposed to violence: guns
         exp4 = exp4 == 2 | exp4 == 3 | exp4 == 4,       # 1 = exposed to violence: gunshot
         pj1 = pj1 == 3 | pj1 == 4,   # 1 = positive views about PJ
         pj2 = pj2 == 3 | pj2 == 4,   # 1 = positive views about PJ
         pj3 = pj3 == 3 | pj3 == 4,   # 1 = positive views about PJ
         pj4 = pj4 == 3 | pj4 == 4    # 1 = positive views about PJ
  )


w3 <- w3 %>%
  select(id = sbjnum,
         duty1 = p2101, duty2 = p2102, rule1 = p2103, rule2 = p2104, rule3 = p2106,
         exp1 = p2701, exp2 = p2702, exp3 = p2703, exp4 = p2704,
         stop = p2901, handcuff = p2902, beat = p2903,
         pj1 = p3201, pj2 = p3202, pj3 = p3203, pj4 = p3204,
         theft = p2001, disorder = p2002, violent = p2003, drugs = p2004,
         homevict_seen = p901, homevict_assault = p903,
         famfr_robbed = p1001, famfr_murdered = p1002, famfr_arrest = p1003,
         peer_theft = p1101, peer_disorder = p1102, peer_violent = p1103, peer_drugs = p1104,
         bullying1 = p1201, bullying2 = p1202, bullying3 = p1203, bullying4 = p1204, bullying5 = p1205,
         bullied1 = p1301, bullied2 = p1302, bullied3 = p1303, bullied4 = p1304, bullied5 = p1305) %>%
  mutate(panelid = paste(id, ".", 3),
         duty1 = duty1 == 3 | duty1 == 4,   # 1 = duty; 0 = no duty
         duty2 = duty2 == 1 | duty2 == 2,   # 1 = duty; 0 = no duty
         rule1 = rule1 == 3 | rule1 == 4,   # 1 = positive views on the rule of law
         rule2 = rule2 == 1 | rule2 == 2,   # 1 = positive views on the rule of law
         rule3 = rule3 == 3 | rule3 == 4,   # 1 = positive views on the rule of law
         stop = stop != 0,                    # 1 = seen police stop
         handcuff = handcuff != 0,            # 1 = seen police handcuff
         beat = beat != 0,                    # 1 = seen police beating,
         exp1 = exp1 == 1 | exp1 == 2 | exp1 == 3,      # 1 = exposed to violence: drugs
         exp2 = exp2 == 1 | exp2 == 2 | exp2 == 3,      # 1 = exposed to violence: robbery
         exp3 = exp3 == 1 | exp3 == 2 | exp3 == 3,      # 1 = exposed to violence: guns
         exp4 = exp4 == 1 | exp4 == 2 | exp4 == 3,      # 1 = exposed to violence: gunshot
         pj1 = pj1 == 3 | pj1 == 4,    # 1 = positive views about PJ
         pj2 = pj2 == 3 | pj2 == 4,    # 1 = positive views about PJ
         pj3 = pj3 == 3 | pj3 == 4,    # 1 = positive views about PJ
         pj4 = pj4 == 3 | pj4 == 4     # 1 = positive views about PJ
  )


w4 <- w4 %>%
  select(id = sbjnum,
         duty1 = p2001, duty2 = p2002, rule1 = p2003, rule2 = p2004, rule3 = p2006,
         exp1 = p2601, exp2 = p2602, exp3 = p2603, exp4 = p2604,
         stop = p2801, handcuff = p2802, beat = p2803,
         pj1 = p3101, pj2 = p3102, pj3 = p3103, pj4 = p3104,
         theft = p1901, disorder = p1902, violent = p1903, drugs = p1904,
         homevict_seen = p801, homevict_assault = p803,
         famfr_robbed = p901, famfr_murdered = p902, famfr_arrest = p903,
         peer_theft = p1001, peer_disorder = p1002, peer_violent = p1003, peer_drugs = p1004,
         bullying1 = p1101, bullying2 = p1102, bullying3 = p1103, bullying4 = p1104, bullying5 = p1105,
         bullied1 = p1201, bullied2 = p1202, bullied3 = p1203, bullied4 = p1204, bullied5 = p1205) %>%
  mutate(panelid = paste(id, ".", 4),
         #white = race == 1,
         duty1 = duty1 == 3 | duty1 == 4,   # 1 = duty; 0 = no duty
         duty2 = duty2 == 1 | duty2 == 2,   # 1 = duty; 0 = no duty
         rule1 = rule1 == 3 | rule1 == 4,   # 1 = positive views on the rule of law
         rule2 = rule2 == 1 | rule2 == 2,   # 1 = positive views on the rule of law
         rule3 = rule3 == 3 | rule3 == 4,   # 1 = positive views on the rule of law
         stop = stop != 0,                    # 1 = seen police stop
         handcuff = handcuff != 0,            # 1 = seen police handcuff
         beat = beat != 0,                    # 1 = seen police beating,
         exp1 = exp1 == 1 | exp1 == 2 | exp1 == 3,      # 1 = exposed to violence: drugs
         exp2 = exp2 == 1 | exp2 == 2 | exp2 == 3,      # 1 = exposed to violence: robbery
         exp3 = exp3 == 1 | exp3 == 2 | exp3 == 3,      # 1 = exposed to violence: guns
         exp4 = exp4 == 1 | exp4 == 2 | exp4 == 3,      # 1 = exposed to violence: gunshot
         pj1 = pj1 == 3 | pj1 == 4,
         pj2 = pj2 == 3 | pj2 == 4,
         pj3 = pj3 == 3 | pj3 == 4,
         pj4 = pj4 == 3 | pj4 == 4
  )

#ac <- ac %>%
#  select(MUMERO, CLUSTER_TP) %>%
#  rename('id' = 'MUMERO',
#         'area' = 'CLUSTER_TP') %>%
#  mutate_at(vars(id), as.character)

#w1 <- w1 %>%
#  left_join(ac, by = 'id') %>%
#  mutate_at(vars(area), ~na_if(., "-")) %>%
#  mutate_at(vars(area), as.factor)

data <-
  w1 %>%
  dplyr::select(id, duty1:panelid) %>%
  mutate(wave = 1) %>%
  bind_rows(w2 %>% mutate(wave = 2)) %>%
  bind_rows(w3 %>% mutate(wave = 3)) %>%
  bind_rows(w4 %>% mutate(wave = 4)) %>%
  mutate(across(c(theft:bullied5), ~ . != 0)) %>%
  mutate(bullying = bullying1 + bullying2 + bullying3 + bullying4 + bullying5,
         bullied = bullied1 + bullied2 + bullied3 + bullied4 + bullied5) %>%
  left_join(w1 %>%
              dplyr::select(id, school, race, privsch, male, white)) %>%
  mutate(across(c(id:bullied5, privsch, male, white), as.numeric),
         wave = as.integer(wave))

cfa.legitimacy_MLR <-
  'legitimacy_MLR =~ duty1 + duty2 + rule1 + rule2 + rule3' %>%
  cfa(data = data, std.lv = T, missing = 'ML', estimator = "MLR")

cfa.legitimacy_DWLS <-
  'legitimacy_DWLS =~ duty1 + duty2 + rule1 + rule2 + rule3' %>%
  cfa(data = data %>% mutate(across(c(duty1:rule3), ~as.ordered(.))), 
      ordered = c("duty1", "duty2", "rule1", "rule2", "rule3")
      , std.lv = T)

cfa.legitimacy_PML <-
  'legitimacy_PML =~ duty1 + duty2 + rule1 + rule2 + rule3' %>%
  cfa(data = data %>% mutate(across(c(duty1:rule3), ~as.logical(.))) 
      , ordered = c("duty1", "duty2", "rule1", "rule2", "rule3")
      , std.lv = T, estimator = "PML", missing = "available.cases")

data <-
  data %>% 
  mutate(legitimacy_MLR = lavPredict(cfa.legitimacy_MLR)[, "legitimacy_MLR"])

for (fs in colnames(lavPredict(cfa.legitimacy_DWLS))) {
  data[lavInspect(cfa.legitimacy_DWLS, "case.idx"), fs] <- lavPredict(cfa.legitimacy_DWLS)[ , fs]
}

for (fs in colnames(lavPredict(cfa.legitimacy_PML))) {
  data[lavInspect(cfa.legitimacy_PML, "case.idx"), fs] <- lavPredict(cfa.legitimacy_PML)[ , fs]
}

data %>% dplyr::select(legitimacy_MLR, legitimacy_DWLS, legitimacy_PML) %>% ggpairs

saveRDS(data, file = 'data/data_reduced.RDS')

table1 <-
  data %>%
  group_by(wave) %>%
  summarize(across(c(stop, handcuff, beat, duty1:rule3, theft, disorder, drugs, violent, exp1:exp4, 
                     homevict_seen, homevict_assault, famfr_robbed, famfr_murdered, famfr_arrest, 
                     peer_theft, peer_disorder, peer_violent, peer_drugs, bullying, bullied, 
                     male, white, privsch), mean, na.rm = TRUE)) %>%
  t %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  flextable()
  #kable(format = "pipe") %>%
  #cat
