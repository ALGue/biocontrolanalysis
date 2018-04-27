# Script

library(tidyverse)

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
  # infection fill level in the landscape 
  mutate(fill.level = dyn.inf*100/stock.crops) %>%
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
  # numeric -> factor (for ggplot)
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

## ggplot

data.dyn %>% 
  # filtre
  filter(year == 10) %>%
  filter(agregation == 1) %>%
  filter(proportion %in% c(1,3,5,7,9,20)) %>%
  filter(infection.rate %in% c(1,5,7,10,15,20)) %>% 
  filter(tick %in% seq(1620, 1769, 1)) %>% 
  # ggplot
  ggplot() + 
  # dynamique du taux de remplissage du paysage par inf. -> courbe
  geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level"),
             size = 0.5) + 
  # ligne verticale = tick th. auquel le paysage devrait être rempli si pas de contrôle
  geom_vline(data=values.for.vline %>% 
               # same filter than data.dyn
               filter(year == 10) %>%
               filter(agregation == 1) %>%
               filter(proportion %in% c(1,3,5,7,9,20)) %>%
               filter(infection.rate %in% c(1,5,7,10,15,20)) %>% 
               # pour éviter message d'erreur on filtre les valeurs hors limite (x -> tick)
               filter(tick.full.th < 1770),
             aes(xintercept=tick.full.th, colour="tick.full.th"), linetype="dashed", size=1) + 
  # dynamique pests/adults -> courbes
  geom_point(mapping = aes(x = tick, y = dyn.inf * 100 / 2100, colour = "dyn.inf"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = dyn.inf.non.occ * 100 / 2100, colour = "dyn.inf.non.occ"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = dyn.inf.occ * 100 / 2100, colour = "dyn.inf.occ"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = dyn.adults * 100 / 2100, colour = "dyn.adults"),
             size = 0.5) + 
  # crop loss -> segment / point
  geom_segment(data = data.crop.loss %>% 
                 # same filter than data.dyn
                 filter(year == 10) %>%
                 filter(agregation == 1) %>%
                 filter(proportion %in% c(1,3,5,7,9,20)) %>%
                 filter(infection.rate %in% c(1,5,7,10,15,20)),
               mapping = aes(x = 1769 , y = crop.loss * 100 / 2100, yend= 0, xend = 1769), 
               colour="grey50") +
  geom_point(data = data.crop.loss %>% 
               # same filter than data.dyn
               filter(year == 10) %>%
               filter(agregation == 1) %>%
               filter(proportion %in% c(1,3,5,7,9,20)) %>%
               filter(infection.rate %in% c(1,5,7,10,15,20)),
             aes(x = 1769, y = crop.loss * 100 / 2100, colour= "crop.loss"), size=3) +
  # x = ticks de l'année 10
  scale_x_continuous(name = expression("Tick"), 
                     breaks = c(1660, 1700, 1740),
                     labels = c("40","80","120"),
                     limits = c(1620, 1769)) + # les ticks qui nous intéressent
  # y = double axe -> à gauche : taux de remplissage / à droite : dyn. pests et adults
  scale_y_continuous(name = expression("Landscape fill level (%)"), 
                     sec.axis = sec_axis(~ . * 2100 / 100 , name = "Dynamics of pests & adults (nb.)"), # right axis
                     limits = c(0, 100)) +
  # legends / axes
  scale_colour_manual(values = c("violet","blue", "orange", "cyan", "grey","red", "green"), labels=c("Crop loss","Nb. adults","Nb. infected crop","Occ","Non occ","Fill level by pests","Tick landsc. full")) +
  # facets
  facet_grid(proportion ~ infection.rate, labeller = label_both) + 
  # titre
  ggtitle("Dynamics of pests and adults + Taux de remplissage par pests : year 10, LA (1)")
