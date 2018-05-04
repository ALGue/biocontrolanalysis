# Script

library(tidyverse)


# control -----------------------------------------------------------------

data_path.control <- "data/exp-control"

filename.control <- list.files(data_path.control, pattern="^[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names <- c(
  # init.params
  "infection.rate",
  "proportion",
  "agregation",
  "proba.overwintering",
  "init.nb.adults",
  "proba.birth",
  # time
  "year",
  "tick",
  # outputs
  "visit.count",
  "dyn.inf",
  "dyn.inf.non.occ",
  "dyn.inf.occ",
  "dyn.adults",
  "tot.inf.cur",
  "dyn.curation")

data.dyn.control <- tibble(filename.control) %>% 
  # load whole data
  mutate(file_contents = map(filename.control, ~ read_delim(file.path(data_path.control, .), delim = " ", col_names = column.names))) %>% 
  unnest()%>% 
  # select data we are interested in
  select(- filename.control, - proba.birth, -proba.overwintering, - init.nb.adults, - visit.count, - tot.inf.cur, - dyn.curation) %>% 
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% # tot crop patches in the landscape
  mutate(tick.full.th = ceiling(stock.crops/infection.rate)) %>% # tick = full landscape without pest control
  mutate(fill.level = dyn.inf*100/stock.crops) %>% # infection fill level in the landscape 
  mutate(fill.level.non.occ = dyn.inf.non.occ*100/stock.crops) %>% # infection fill level in the landscape 
  filter(year == 10) %>%
  filter(agregation == 1) %>%
  filter(proportion %in% c(1,30,50,70,90)) %>%
  filter(infection.rate %in% c(1,5,7,10,15,20)) %>% 
  filter(tick %in% seq(1620, 1769, 1)) %>% 
  ggplot() +
  # geom_point(aes(x = tick, y = dyn.inf, colour = "dyn.inf")) +
  geom_point(aes(x = tick, y = fill.level, colour = "fill.level"), 
             size = 1) +
  geom_point(aes(x = tick, y = fill.level, colour = "fill.level.non.occ"),
             size = 0.5) +
  facet_grid(proportion ~ infection.rate, labeller = label_both) + 
  ggtitle("")
  
# crop-loss control

filename.control <- list.files(data_path.control, pattern="^totals-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names <- c(
  # par.
  "infection.rate",
  "proportion",
  "agregation",
  # time
  "year",
  # outputs
  "crop.loss.1st",
  "crop.loss",
  "crop.save")

control.crop.loss <- tibble(filename.control) %>% 
  # load whole data
  mutate(file_contents = map(filename.control, ~ read_delim(file.path(data_path.control, .), delim = " ", col_names = column.names))) %>% 
  unnest() %>% 
  select(- filename.control, - crop.loss.1st, - crop.save) %>%
  # crop.loss * 100 pour normaliser en %
  mutate(crop.loss = crop.loss) %>% 
  # tot crop patches in the landscape
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  # normalisation de crop.loss selon le nb de patchs crops dans le paysage
  mutate(norm.crop.loss = crop.loss*100/stock.crops) %>% 
  # numeric -> factor (for ggplot)
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

control.crop.loss %>% 
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  ggplot() +
  # crop.loss
  geom_point(mapping = aes(x = proportion, y = crop.loss, colour= "crop.loss"), size=3) +
  # norm.crop.loss = crop.loss / stock of crop patches
  geom_point(mapping = aes(x = proportion, y = norm.crop.loss * (max(control.crop.loss$crop.loss)) / 100, colour= "norm.crop.loss"), size=3) +
  # axes y
  scale_y_continuous(name = expression("Total crop loss in the landscape "), 
                     sec.axis = sec_axis(~ . / (max(control.crop.loss$crop.loss)) * 100 , name = "Normalized crop loss")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Controls for total crop loss & normalized crop loss")


# data path ---------------------------------------------------------------

data_path <- "data/exp8"

# dynamics ----------------------------------------------------------------

## files with dynamics

filename <- list.files(data_path, pattern="^[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names <- c(
  # init.params
  "infection.rate",
  "proportion",
  "agregation",
  "proba.overwintering",
  "init.nb.adults",
  "proba.birth",
  # time
  "year",
  "tick",
  # outputs
  "visit.count",
  "dyn.inf",
  "dyn.inf.non.occ",
  "dyn.inf.occ",
  "dyn.adults",
  "tot.inf.cur",
  "dyn.curation")

data.dyn <- tibble(filename) %>% 
  # load whole data
  mutate(file_contents = map(filename, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names))) %>% 
  unnest()%>% 
  # select data we are interested in
  select(- filename, - proba.birth, -proba.overwintering, - init.nb.adults, - visit.count, - tot.inf.cur, - dyn.curation) %>% 
  # tot crop patches in the landscape
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  # tick = full landscape without pest control
  mutate(tick.full.th = ceiling(stock.crops/infection.rate)) %>% 
  # total infection fill level in the landscape 
  mutate(fill.level = dyn.inf*100/stock.crops) %>%
  # % of landscape infected patches without predators (adults / juveniles)
  mutate(fill.level.non.occ = dyn.inf.non.occ*100/stock.crops) %>%
  # % of landscape infected crop patches with predators (adults / juveniles)
  mutate(fill.level.occ = dyn.inf.occ*100/stock.crops) %>%
  # numeric -> factor (for ggplot)
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

values.for.vline <- data.dyn %>%
  select(infection.rate, proportion, agregation, year, tick.full.th) %>%
  distinct() %>%
  mutate(tick.full.th = tick.full.th + 1621)


## files with crop.loss

filename <- list.files(data_path, pattern="^totals-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names <- c(
  # par.
  "infection.rate",
  "proportion",
  "agregation",
  # time
  "year",
  # outputs
  "crop.loss.1st",
  "crop.loss",
  "crop.save")

data.crop.loss <- tibble(filename) %>% 
  # load whole data
  mutate(file_contents = map(filename, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names))) %>% 
  unnest() %>% 
  select(- filename, - crop.loss.1st, - crop.save) %>%
  # crop.loss * 100 pour normaliser en %
  mutate(crop.loss = crop.loss) %>% 
  # tot crop patches in the landscape
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  # normalisation de crop.loss selon le nb de patchs crops dans le paysage
  mutate(norm.crop.loss = crop.loss*100/stock.crops) %>% 
  # numeric -> factor (for ggplot)
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

data.crop.loss %>% 
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  ggplot() +
  # crop.loss
  geom_point(mapping = aes(x = proportion, y = crop.loss, colour= "crop.loss"), size=3) +
  # norm.crop.loss = crop.loss / stock of crop patches
  geom_point(mapping = aes(x = proportion, y = norm.crop.loss * (max(control.crop.loss$crop.loss)) / 100, colour= "norm.crop.loss"), size=3) +
  # axes y
  scale_y_continuous(name = expression("Total crop loss in the landscape "), 
                     sec.axis = sec_axis(~ . / (max(control.crop.loss$crop.loss)) * 100 , name = "Normalized crop loss")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Total crop loss & normalized crop loss")

# on fusionne data.crop.loss et control.crop.loss

test1 <- data.crop.loss %>% 
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  mutate(type = factor('data')) %>% 
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

test2 <- control.crop.loss %>% 
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  mutate(type = factor('control')) %>% 
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

test <- test1 %>% 
  bind_rows(test2)

test %>% 
  ggplot() +
  geom_point(aes(x = proportion, y =  crop.loss, colour = "crop.loss", shape = type), size = 2) +
  geom_point(aes(x = proportion, y =  norm.crop.loss * (max(test$crop.loss)) / 100, colour = "norm.crop.loss", shape = type), size = 2) +
  # axes y
  scale_y_continuous(name = expression("Total crop loss in the landscape "), 
                     sec.axis = sec_axis(~ . / (max(test$crop.loss)) * 100 , name = "Normalized crop loss")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Total crop loss & normalized crop loss")
  
# crop-loss control/data only
test %>% 
  ggplot() +
  geom_point(aes(x = proportion, y =  crop.loss, colour = type, shape = type)) +
  # geom_point(aes(x = proportion, y =  norm.crop.loss * (max(test$crop.loss)) / 100, colour = "norm.crop.loss", shape = type, size = 1)) +
  # axes y
  # scale_y_continuous(name = expression("Total crop loss in the landscape "), 
  #                    sec.axis = sec_axis(~ . / (max(test$crop.loss)) * 100 , name = "Normalized crop loss")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Total crop loss")

# normalized crop-loss control/data only
test %>% 
  ggplot() +
  geom_point(aes(x = proportion, y =  norm.crop.loss, colour = type, shape = type)) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Normalized crop loss")

## ggplot crop-loss / normalized crop-loss together

data.crop.loss %>% 
  # filter
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  # on ajoute une colonne type pour distinguer data / control
  mutate(type = "data") %>% 
  # factor
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion)) %>% 
  # on bind les données issues du control
  bind_rows(control.crop.loss %>% 
              # filter
              filter(agregation == 1) %>% 
              filter(infection.rate %in% c(5,10,15,20)) %>% 
              filter(proportion %in% c(10,30,50,70,90)) %>% 
              filter(year == 10) %>% 
              # on ajoute une colonne type pour distinguer data / control
              mutate(type = "control") %>% 
              # factor
              mutate(infection.rate = factor(infection.rate)) %>% 
              mutate(proportion = factor(proportion))) %>% 
  # on factor la colonne type qui était character
  mutate(type = as.factor(type)) %>% 
  # plot sur 2 axes y : crop-loss / normalized crop loss
  ggplot() +
  geom_point(aes(x = proportion, y =  crop.loss, colour = "crop.loss", shape = type), size = 2) +
  geom_point(aes(x = proportion, y =  norm.crop.loss * (max(test$crop.loss)) / 100, colour = "norm.crop.loss", shape = type), size = 2) +
  # axes y
  scale_y_continuous(name = expression("Total crop loss in the landscape "), 
                     sec.axis = sec_axis(~ . / (max(test$crop.loss)) * 100 , name = "Normalized crop loss")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("Total crop loss & normalized crop loss")

## ggplot differences between crop-loss and normalized crop-loss

d <- data.totals %>% 
  # filter
  filter(agregation == 1) %>% 
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  filter(year == 10) %>% 
  # on ajoute une colonne type pour distinguer data / control
  mutate(type = "data") %>% 
  # factor
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion)) %>% 
  # on bind les données issues du control
  bind_rows(control.totals %>% 
              # filter
              filter(agregation == 1) %>% 
              filter(infection.rate %in% c(5,10,15,20)) %>% 
              filter(proportion %in% c(10,30,50,70,90)) %>% 
              filter(year == 10) %>% 
              # on ajoute une colonne type pour distinguer data / control
              mutate(type = "control") %>% 
              # factor
              mutate(infection.rate = factor(infection.rate)) %>% 
              mutate(proportion = factor(proportion))) %>% 
  # on factor la colonne type qui était character
  mutate(type = as.factor(type)) %>% 
  # réduction jeu de données
  select(- agregation, - year, - stock.crops)

d.control <- d %>% 
  filter(type == 'control') %>% 
  arrange(infection.rate, proportion)

d<- d %>% 
  filter(type == 'data') %>% 
  arrange(infection.rate, proportion) %>% 
  mutate(diff.cl = d.control$crop.loss - crop.loss) %>% 
  mutate(diff.ncl = d.control$norm.crop.loss - norm.crop.loss)

d %>% 
  ggplot() +
  geom_point(aes(x = proportion, y =  diff.cl, colour = "diff.cl"), size = 2) +
  geom_line(aes(x = proportion, y =  diff.cl, colour = "diff.cl", group = 1)) +
  geom_point(aes(x = proportion, y =  diff.ncl * (max(d$diff.cl)) / 100, colour = "diff.ncl"), size = 2) +
  geom_line(aes(x = proportion, y =  diff.ncl * (max(d$diff.cl)) / 100, colour = "diff.ncl", group = 1)) +
  # axes y
  scale_y_continuous(name = expression("diff.cl"), 
                     sec.axis = sec_axis(~ . / (max(d$diff.cl)) * 100 , name = "diff.ncl")) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  ggtitle("diff.cl and diff.ncl")



## ggplot dynamics

data.dyn %>% 
  # filtre
  filter(year == 10) %>%
  filter(agregation == 1) %>%
  filter(proportion %in% c(10,30,50,70,90)) %>%
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(tick %in% seq(1620, 1769, 1)) %>% 
  # ggplot
  ggplot() + 
  # dynamique du taux de remplissage du paysage par inf. -> courbe
  geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = fill.level.non.occ, colour = "fill.level.non.occ"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = fill.level.occ, colour = "fill.level.occ"),
             size = 0.5) + 
  # ligne verticale = tick th. auquel le paysage devrait être rempli si pas de contrôle
  geom_vline(data=values.for.vline %>% 
               # same filter than data.dyn
               filter(year == 10) %>%
               filter(agregation == 1) %>%
               filter(proportion %in% c(10,30,50,70,90)) %>%
               filter(infection.rate %in% c(5,10,15,20)) %>% 
               # pour éviter message d'erreur on filtre les valeurs hors limite (x -> tick)
               filter(tick.full.th < 1770),
             aes(xintercept=tick.full.th, colour="tick.full.th"), linetype="dashed", size=1) + 
  # dynamique pests/adults -> courbes
  # geom_point(mapping = aes(x = tick, y = dyn.inf * 100 / 2100, colour = "dyn.inf"),
  #            size = 0.5) + 
  # geom_point(mapping = aes(x = tick, y = dyn.inf.non.occ * 100 / 2100, colour = "dyn.inf.non.occ"),
  #            size = 0.5) + 
  # geom_point(mapping = aes(x = tick, y = dyn.inf.occ * 100 / 2100, colour = "dyn.inf.occ"),
  #            size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = dyn.adults * 100 / (max(dyn.adults)+10), colour = "dyn.adults"),
             size = 0.5) + 
  # crop loss -> segment / point
  geom_segment(data = data.crop.loss %>% 
                 # same filter than data.dyn
                 filter(year == 10) %>%
                 filter(agregation == 1) %>%
                 filter(proportion %in% c(10,30,50,70,90)) %>%
                 filter(infection.rate %in% c(5,10,15,20)),
               mapping = aes(x = 1769 , y = norm.crop.loss, yend= 0, xend = 1769), 
               colour="grey50") +
  geom_point(data = data.crop.loss %>% 
               # same filter than data.dyn
               filter(year == 10) %>%
               filter(agregation == 1) %>%
               filter(proportion %in% c(10,30,50,70,90)) %>%
               filter(infection.rate %in% c(5,10,15,20)),
             aes(x = 1769, y = norm.crop.loss, colour= "crop.loss"), size=3) +
  # x = ticks de l'année 10
  scale_x_continuous(name = expression("Tick"), 
                     breaks = c(1660, 1700, 1740),
                     labels = c("40","80","120"),
                     limits = c(1620, 1769)) + # les ticks qui nous intéressent
  # y = double axe -> à gauche : taux de remplissage / à droite : dyn. pests et adults
  scale_y_continuous(name = expression("Landscape fill level (%)"), 
                     sec.axis = sec_axis(~ . * (max(data.dyn$dyn.adults)+10) / 100 , name = "Dynamics of pests & adults (nb.)"), # right axis
                     limits = c(0, 100)) +
  # legends / axes
  scale_colour_manual(values = c("violet","blue", "orange", "cyan", "grey","red")# , 
                      # labels=c("Norm Crop loss","Nb. adults","Nb. infected crop","Occ","Non occ","Fill level by pests","Tick landsc. full")
                      ) +
  # facets
  facet_grid(infection.rate ~ proportion, labeller = label_both) + 
  # titre
  ggtitle("Dynamics of pests and adults + Taux de remplissage par pests : year 10, LA (1)")


# distribution events -----------------------------------------------------

## arrivals

data_path <- "data/exp8"

filename <- list.files(data_path, pattern="^distribution-arrival-[0-9]{1,2}-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt") # all filenames distribution arrival

column.names.arrival <- c("proportion","agregation", "infection.rate","year", "tick", "event","pxcor","pycor","t.pest.alone")

data.arrival <- tibble(filename) %>% 
  mutate(file_contents = map(filename, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.arrival))) %>% 
  unnest()

data.arrival %>%
  mutate(proportion = factor(proportion)) %>% # jointure nécessite même var.type entre les 2 tables
  mutate(infection.rate = factor(infection.rate)) %>% 
  # même filtrage que précédemment
  filter(year == 10) %>% 
  filter(agregation == 1) %>% 
  select(filename,proportion,infection.rate,tick) %>% 
  group_by(filename,proportion,infection.rate,tick) %>% 
  # comptage des occurences d'arrival par tick
  summarise(n.tick = n()) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = tick, y = n.tick), stat="identity", fill="white", colour="black") +
  scale_x_continuous(name = expression("Tick"), 
                     breaks = c(1621, 1695, 1769),
                     limits = c(1621, 1769)) +
  facet_grid(infection.rate ~ proportion, labeller = label_both)

## distribution time-since-infection

# nb absolu

data.arrival %>%
  select(filename, proportion, agregation, infection.rate, year, t.pest.alone) %>%
  filter(agregation == 1) %>%
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(year == 10) %>%
  group_by(filename, proportion, agregation, infection.rate,t.pest.alone) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  ggplot(aes(x=t.pest.alone, y=count.occ)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Time since infection, when adults arrive on the infected patch", 
                     breaks = c(6,10,15,20), 
                     limits = c(6,20)) +
  scale_y_continuous(name = "Occurences for each t (time-since-infection)") +
  #geom_hline(data = values.for.hline,
  #           aes(yintercept=count.occ, colour="count.occ.t.pest.alone"), 
  #           linetype="dashed", 
  #           size=1) + 
  facet_grid(infection.rate ~ proportion, labeller = label_both) +
  labs(colour = "Indicator") +
  scale_colour_manual(values = c("red"), labels=c("Max. occ.")) +
  ggtitle("Low agregation (1), Year 10 : Occurences of t (time-since-infection)") 


# % d'occurences

data.arrival %>%
  select(filename, proportion, agregation, infection.rate, year, t.pest.alone) %>%
  filter(agregation == 1) %>%
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(year == 10) %>%
  group_by(filename, proportion, agregation, infection.rate,t.pest.alone) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(filename, proportion, agregation, infection.rate) %>% 
  mutate(total.occ = sum(count.occ)) %>% 
  mutate(percentage.occ = count.occ*100/total.occ)  %>% 
  # vérification du total
  # summarize(sum_of = sum(percentage.occ))
  ungroup() %>% 
  ggplot(aes(x=t.pest.alone, y=percentage.occ)) +
  geom_bar(stat = "identity") + 
  facet_grid(infection.rate ~ proportion, labeller = label_both) +
  scale_x_continuous(name = "Time since infection, when adults arrive on the infected patch", 
                     breaks = c(6,10,15,20), 
                     limits = c(6,20)) +
  scale_y_continuous(name = "% occurences for each t (time-since-infection)") +
  #geom_hline(data = values.for.hline,
  #           aes(yintercept=percentage.occ, colour="count.occ.t.pest.alone"), 
  #           linetype="dashed", 
  #           size=1) +
  labs(colour = "Indicator") +
  scale_colour_manual(values = c("red"), labels=c("% Max. occ.")) +
  ggtitle("Low agregation (1), Year 10 : % of occurences for time-since-infection")

## AUC : ara under curve

library(DescTools)
AUC(x=c(1,3), y=c(1,1))

AUC(x=c(1,2,3), y=c(1,2,4), method="trapezoid")
AUC(x=c(1,2,3), y=c(1,2,4), method="step")

plot(x=c(1,2,2.5), y=c(1,2,4), type="l", col="blue", ylim=c(0,4))
lines(x=c(1,2,2.5), y=c(1,2,4), type="s", col="red")

x <- seq(0, pi, length.out=200)
AUC(x=x, y=sin(x)) 
AUC(x=x, y=sin(x), method="spline") 

######################################################

library(tidyverse)

data_path <- "/home/antoine/Documents/Git/Biological-Control/results/essai-time-for-crop-loss"


filename.arrivals <- list.files(data_path,
                                pattern="^distribution-arrival-[0-9]{1,2}-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt") 
column.names.arrivals <- c(
  # par.
  "proportion",
  "agregation", 
  "infection.rate",
  "nb.init",
  # time
  "year", 
  "tick", 
  # outputs
  "event",
  "pxcor",
  "pycor",
  "t.pest.alone",
  "t.for.crop.loss")

data.arrivals <- tibble(filename.arrivals) %>% 
  mutate(file_contents = map(filename.arrivals, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.arrivals))) %>% 
  unnest()

data.arrivals %>%
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  filter(year == 1) %>% 
  filter(proportion == 10) %>% 
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  facet_grid(nb.init ~ ., labeller = label_both) +
  ggtitle("Occurences time-for-crop-loss, scenario : SA, y1, inf.rate20, proportion10, various nb.init")

data.arrivals %>%
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  filter(year == 10) %>% 
  filter(proportion == 10) %>% 
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  facet_grid(nb.init ~ ., labeller = label_both) +
  ggtitle("Occurences time-for-crop-loss, scenario : SA, y10, inf.rate20, proportion10, various nb.init")

# scenario LA
data.arrivals %>%
  select(filename.arrivals, proportion, agregation, infection.rate, year, t.for.crop.loss) %>%
  #filter(agregation == 1) %>%
  #filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(year == 10) %>%
  group_by(filename.arrivals, proportion, agregation, infection.rate,t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  # x-axis
  scale_x_continuous(name = "Time since infection, when adults arrive on the infected patch", 
                     breaks = c(6,10,15,20), 
                     limits = c(6,20)) +
  scale_y_continuous(name = "Occurences for each t (time-since-infection)") +
  #geom_hline(data = values.for.hline,
  #           aes(yintercept=count.occ, colour="count.occ.t.pest.alone"), 
  #           linetype="dashed", 
  #           size=1) + 
  # facet_grid(infection.rate ~ proportion, labeller = label_both) +
  labs(colour = "Indicator") +
  scale_colour_manual(values = c("red"), labels=c("Max. occ.")) +
  ggtitle("LA (1, y10) : Distribution of occurences for time btw. infection / arrival")







#############################

data_path <- "/home/antoine/Documents/Git/Biological-Control/results/essai-time-for-crop-loss3"


filename.tforcroploss <- list.files(data_path,
                                pattern="^time-for-crop-loss-[0-9]{1,2}-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt") 
column.names.tforcroploss <- c(
  # par.
  "proportion",
  "agregation", 
  "infection.rate",
  "nb.init",
  # time
  "year", 
  # outputs
  "t.for.crop.loss")

data.tforcroploss <- tibble(filename.tforcroploss) %>% 
    mutate(file_contents = map(filename.tforcroploss, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.tforcroploss))) %>% 
    unnest() 

data.tforcroploss %>% 
  filter(proportion == 10) %>% 
  filter(nb.init == 10) %>% 
  filter(infection.rate == 80) %>% 
  filter(year == 10) %>% 
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity")

data.tforcroploss %>%
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  filter(year == 1) %>% 
  filter(proportion == 10) %>% 
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  facet_grid(nb.init ~ ., labeller = label_both)

#############################

data_path <- "data/exp9"


filename.tforcroploss <- list.files(data_path,
                                    pattern="^time-for-crop-loss-[0-9]{1,2}-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt") 
column.names.tforcroploss <- c(
  # par.
  "proportion",
  "agregation", 
  "infection.rate",
  "nb.init",
  # time
  "year", 
  # outputs
  "t.for.crop.loss")

data.tforcroploss <- tibble(filename.tforcroploss) %>% 
  mutate(file_contents = map(filename.tforcroploss, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.tforcroploss))) %>% 
  unnest()

# proportion 0 10 90
# nb.init 0 1 2 3 4 5 6 7 8 9 10 20
# year 1 5
# agregation 1 5

# à year = 5, on voit bien la différence entre proportion 10 et 90

data.tforcroploss %>%
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  # filter
  filter(year == 5) %>% 
  filter(agregation == 5) %>% 
  filter(proportion == 90) %>% 
  filter(infection.rate == 20) %>% 
  # group and count occ. for t.for.crop.loss
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  # facets according to nb.init
  facet_grid(nb.init ~ ., labeller = label_both) +
  ggtitle("Strong agreg., Year5, inf.rate20, proportion 90")

data.tforcroploss %>%
  select(proportion, agregation, nb.init, infection.rate, year, t.for.crop.loss) %>% 
  # filter
  filter(year == 5) %>% 
  filter(agregation == 5) %>% 
  filter(nb.init == 10) %>% 
  # filter(infection.rate == 20) %>% 
  # group and count occ. for t.for.crop.loss
  group_by(proportion, agregation, infection.rate, nb.init, t.for.crop.loss) %>% 
  mutate(count.occ = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  # filter for plot (reduce levels)
  filter(infection.rate %in% c(5,10,15,20)) %>% 
  filter(proportion %in% c(10,30,50,70,90)) %>% 
  # plot
  ggplot(aes(x=t.for.crop.loss, y=count.occ)) +
  geom_bar(stat = "identity") +
  # facets according to nb.init
  facet_grid(infection.rate ~ proportion, labeller = label_both) +
  ggtitle("")


######

## wd
data_path <- "data/exp10"

filename.totals <- list.files(data_path, 
                              pattern="^totals-[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names.totals <- c(
  # par.
  "infection.rate",
  "proportion",
  "agregation",
  "nb.init",
  # time
  "year",
  # outputs
  "crop.loss.1st",
  "crop.loss",
  "crop.save")

data.totals <- tibble(filename.totals) %>% 
  # load whole data
  mutate(file_contents = map(filename.totals, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.totals))) %>% 
  unnest()

data.totals %>% 
  # filter
  filter(agregation == 5) %>%
  filter(nb.init == 10) %>% 
  filter(year == 1) %>% 
  # data type (data = with predators, ow. control)
  mutate(type = factor('data')) %>% 
  # -> factors
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion)) %>% 
  # bind 'data' (with predators) with 'control'
  # bind_rows(control.totals %>% 
  #             # filter
  #             filter(agregation == 5) %>% 
  #             filter(infection.rate %in% c(5,10,15,20)) %>% 
  #             filter(proportion %in% c(10,30,50,70,90)) %>% 
  #             filter(year == 10) %>% 
  #             # data type (data = with predators, ow. control)
  #             mutate(type = factor('control')) %>% 
  #             # -> factor
  #             mutate(infection.rate = factor(infection.rate)) %>% 
  #             mutate(proportion = factor(proportion))) %>% 
  # plot
  ggplot() +
  geom_point(aes(x = proportion, y =  crop.loss, colour = type)) +
  # geom_line(aes(x = proportion, y =  crop.loss, colour = type)) +
  # facets
  facet_grid(infection.rate ~ ., labeller = label_both) +
  # title
  ggtitle("SA, y10 : Crop loss (total landscape) btw. control / with predators")
