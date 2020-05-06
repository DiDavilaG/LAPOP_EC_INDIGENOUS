# 2004 

# Questions with indigenous content
# CP4A - help from indigenous movement (dichotomous)
#B40 - Trust (cofianza) in indigenous movement. Answer 1-7 (nada - mucho / 8 doesn't know)
#NEWTOL6 - Indigenous population maintains their own culture 1 yes / no (country wide culture)
#PC20 - Corruption indigneous leaders - 1 corrupts - 10 - non-corrupt / 88 NA
#VB3 - Presidential candidates: 1 - Lucio / 2 -PRIAN / 3 - Leon Roldos / 4 - Borja 
#VB3 - / 5 - Xavier Neira / 6 - Jacobo / 7 - Jacinto Velazquez / 8 - Ivonne Juez 
#VB3 - / 9 - Cesar Alarcon / 10 - Osvaldo Hurtado / 11 - Antonio Vargas / 12 -Nulo
# ETID / Self-id (1 blanco) (2 mestizo) (3 indigena) (4 Negro) (5 mulato ) (6 otro) (8 ns)
# LENG1 - Lenguage spoken since childhood (1 castellano) (2 quichua) (3 other)
# VB7 - supported PK ( option 9) 
# First import dataset fro 2004 

library(tidyverse)
# Explore data 
str(LAPOP2004)

# Create smaller database 
IND2004LAPOP<- LAPOP2004%>%
  filter(etid == "3")
# Only 27 respondents for language / only 25 speak quechua 
LENG2004LAPOP<- LAPOP2004%>%
  filter(leng1 %in% c (2,3))
#drop na 

df<- IND2004LAPOP%>%
  drop_na(vb3)

df$vb3<- as.factor(df$vb3)
## Table for percetange of vote for PK candidate 
summary(df$vb3)
dfvote2004<- df%>%
  count(vb3) %>%
  mutate(prop = prop.table(n))

## Percentage of legislative 
dfvote2004leg<- IND2004LAPOP%>%
  count(vb7)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)



## Indigenous percentage in LAPOP 2004 

dfpop2004<- LAPOP2004%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)



## DATA from 2006  - before CORREA 

#Questions
#B40 - trust in indigenous movement (1  nada / 7 mucho | 8 no sabe)
#NEWTOL10 - does it matter that an indigenous candidate could become presidennt?
# 1 (preocupa), 2(no es importante), 8 (NS)
# ECUVB3 / VB3 votes for president. (Same values 2004)
#ETID - same 2004 
#ETIDA - father was (1 mestizo) (2 indigena ) (3 negro) (4 mulata) (5 otra)
#LENG1 - same 2004 

IND2006LAPOP<- LAPOP2006%>%
  filter(etid == "3")
# Only 20 respondents for language 
LENG2006LAPOP<- LAPOP2006%>%
  filter(leng1 %in% c (2,3))
#drop na 

df2006<- IND2006LAPOP%>%
  drop_na(ecuvb3)


## Table for percetange of vote for PK candidate 

dfvote2006<- df2006%>%
  count(ecuvb3) %>%
  mutate(prop = prop.table(n))

# Indigenous percentage in LAPOP 2006
dfpop2006<- LAPOP2006%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# legislative vote 
dfvote2006leg<- IND2006LAPOP%>%
  count(ecuvb7)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

## LAPOP 2008 
# Questions: 
# B40 - equal to previous. 
# ETID - (1 blanca) (2 mestiza) (3 indigena) (4 afro) (5 mulato) (7 other ) ( 8 n/a)
# VB3 - 0906 Macas 
#VB11 - suporter of a party - 907 

IND2008LAPOP<- LAPOP2008%>%
  filter(etid == "3")

## Table for percetange of vote for PK candidate 

dfvote2008<- IND2008LAPOP%>%
  count(vb3) %>%
  mutate(prop = prop.table(n))



# Supporter of party
supporterpar2008<- IND2008LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

allpar2008<- LAPOP2008%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# Indigenous percentage in LAPOP 2006
dfpop2008<- LAPOP2008%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

## LAPOP 2010 
# Questions: 
# B40 - same. 
# ETID - (1 blanca) (2 mestiza) (3 indigena) (4 afro) (5 mulato) (7 other ) ( 8 n/a)
# ECUETID2 - indigenous identity 
#ECUETIDA - mother 
# LENG 1 - mother tongue 
# INDAOJ1 - indigenous justice 
# IND1 - support for democracy by indigenous peoples.
#IND2 - impact of indigenous peoples in new laws. 
#VB3 - president
# VB11  - support for party 


IND2010LAPOP<- LAPOP2010%>%
  filter(etid == "3")

## Table for percetange of vote for PK candidate 

dfvote2010<- IND2010LAPOP%>%
  count(vb3) %>%
  mutate(prop = prop.table(n))

# Indigenous percentage in LAPOP 2010
dfpop2010<- LAPOP2010%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)
# self-id pueblos 
dfpoppue2010<- LAPOP2010%>%
  count(ecuetid2) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# Supporter of party
supporterpar2010<- IND2010LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# all suporters of PK 2010 

allpar2010<- LAPOP2010%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# Indigenous percentage in LAPOP 2010
dfpop2010<- LAPOP2010%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

## LAPOP 2012
#Questions
#B40 
# RAC2A - positive discrimination (education)
# VB53 - dark skin bad administrators 
# ETID 
# VB11 
#VB3 


IND2012LAPOP<- LAPOP2012%>%
  filter(etid == "3")

## Table for percetange of vote for PK candidate  - 

dfvote2012<- IND2012LAPOP%>%
  count(vb3) %>%
  mutate(prop = prop.table(n))

# Indigenous percentage in LAPOP 2010
dfpop2012<- LAPOP2012%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# Supporter of party
supporterpar2012<- IND2012LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# all suporters of PK 2010 

allpar2012<- LAPOP2012%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# Indigenous percentage in LAPOP 2010
dfpop2010<- LAPOP2010%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)


## LAPOP 2014 

# Questions 
# B40 
# ETID 
#VB11 
#vb3n 


IND2014LAPOP<- LAPOP2014%>%
  filter(etid == "3")

## Table for percetange of vote for PK candidate  - 

dfvote2014<- IND2014LAPOP%>%
  count(vb3n) %>%
  mutate(prop = prop.table(n))

# Indigenous percentage in LAPOP 2010
dfpop2014<- LAPOP2014%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# Supporter of party
supporterpar2014<- IND2014LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

# all suporters of PK 2010 

allpar2014<- LAPOP2014%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

## LAPOP 2016 
# Questions 
# ETID 
#VB11 
# vb3b  / same as 2014 


IND2016LAPOP<- LAPOP2016%>%
  filter(etid == "3")

## Table for percetange of vote for PK candidate  - 

dfvote2016<- IND2016LAPOP%>%
  count(vb3n) %>%
  mutate(prop = prop.table(n))

# Indigenous percentage in LAPOP 2016
dfpop2016<- LAPOP2016%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# Supporter of party
supporterpar2016<- IND2016LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

## LAPOP 2018 

#ind10 - government should consult indigenous population. 
#ETID 
#VB11
#vb11neg - dilike party 
#vb3 


IND2018LAPOP<- LAPOP2018%>%
  filter(etid == "3")


# Indigenous percentage in LAPOP 2018
dfpop2018<- LAPOP2018%>%
  count(etid) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien = prop*100)

# Supporter of party
supporterpar2018<- IND2018LAPOP%>%
  count(vb11)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)

supporterpar2018neg<- IND2018LAPOP%>%
  count(vb11neg)%>%
  mutate(prop= prop.table(n))%>%
  mutate( propcen =prop*100)
  
dfvote2018<- IND2018LAPOP%>%
  count(vb3n) %>%
  mutate(prop = prop.table(n))

dfvote2018<- IND2018LAPOP%>%
  count(vb3n) %>%
  mutate(prop = prop.table(n))
  
dfvote2018all<- LAPOP2018%>%
  count(vb3n) %>%
  mutate(prop = prop.table(n))%>%
  mutate(propcien= prop*100)
  


