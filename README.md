{\rtf1\ansi\ansicpg1252\cocoartf2761
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Police violence and legal socialisation\
\
This repository contains the code to replicate all analyses included the the article:\
  \
  > Oliveira, Thiago R. and Jon Jackson. Police violence, criminal offending, and the legal socialization of adolescents.\
\
Unfortunately, raw data can only be made available with data agreements. A reduced version of the dataset is available for replication purposes.\
\
### Structure\
\
-   `data` is a directory containing the (hidden) raw datasets, which is then populated with the `data_reduced.RDS` file once `cleaning.R` runs\
-   `plots` is an empty directory which is populated when `PanelMatch.R` and `rasch.R` run\
-   `cleaning.R` loads the raw survey data and produced `data_reduced.RDS`, which is a clean data set used across all analyses\
-   `PanelMatch.R` produces results obtained using Imai et al's (2023) matching methods for panel data\
-   `rasch.R` produces results obtained using a hybrid, longitudinal specification of a multinomial Rasch model\
-   `PoliceViolence_LegalSocialisation.Rproj` is an R project\
\
}