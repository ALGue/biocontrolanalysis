data_path <- "data/exp9"


## dyn
filename.dyn <- list.files(data_path, 
                           pattern="^[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}.txt")

column.names.dyn <- c(
  # init.params
  "infection.rate",
  "proportion",
  "agregation",
  "proba.overwintering",
  "nb.init",
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

data.dyn <- tibble(filename.dyn) %>% 
  # load whole data
  mutate(file_contents = map(filename.dyn, ~ read_delim(file.path(data_path, .), delim = " ", col_names = column.names.dyn))) %>% 
  unnest()


# control dyn -----------------------------------------------------------------

control.dyn <- data.dyn %>% 
  filter(nb.init == 0)
control.dyn %>% 
  # select var.
  select(- filename.dyn, - proba.birth, -proba.overwintering, - nb.init, - visit.count, - tot.inf.cur, - dyn.curation) %>%
  # tot crop patches in the landscape
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  # compute tick -> full landscape without pest control
  mutate(tick.full.th = ceiling(stock.crops/infection.rate)) %>% 
  # infection fill level in the landscape 
  mutate(fill.level = dyn.inf*100/stock.crops) %>% 
  # infected patches without predators (= control) in % (fill.level)
  mutate(fill.level.non.occ = dyn.inf.non.occ*100/stock.crops) %>%  
  # filter
  filter(agregation == 5) %>%
  filter(infection.rate %in% c(20)) %>% 
  filter(proportion %in% c(0,10,90)) %>%
  filter(year == 5) %>%
  filter(tick %in% seq(720, 869, 1)) %>% 
  #filter(tick %in% seq(1620, 1769, 1)) %>% 
  # plot
  ggplot() +
  # geom_point(aes(x = tick, y = dyn.inf, colour = "dyn.inf")) +
  geom_point(aes(x = tick, y = fill.level, colour = "fill.level"), 
             size = 1) +
  geom_point(aes(x = tick, y = fill.level, colour = "fill.level.non.occ"),
             size = 0.5) +
  # facets
  facet_grid(proportion ~ infection.rate, labeller = label_both) + 
  ggtitle("LA, y10 : Dynamics for control (% of infected crop patches & infected without predators)")


# data dyn ----------------------------------------------------------------


data.dyn2 <- data.dyn %>% 
  # select data we are interested in
  select(- filename.dyn, - proba.birth, -proba.overwintering, - visit.count, - tot.inf.cur, - dyn.curation) %>%   
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

values.for.vline <- data.dyn2 %>%
  select(infection.rate, proportion, agregation, year, tick.full.th) %>%
  distinct() %>%
  mutate(tick.full.th = tick.full.th + 1621)

data.totals2 <- data.totals %>% 
  select(- filename.totals, - crop.loss.1st, - crop.save) %>%
  # tot crop patches in the landscape
  mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  # normalisation de crop.loss selon le nb de patchs crops dans le paysage
  mutate(norm.crop.loss = crop.loss*100/stock.crops) %>% 
  # numeric -> factor (for ggplot)
  mutate(infection.rate = factor(infection.rate)) %>% 
  mutate(proportion = factor(proportion))

## ggplot for scenario LA
data.dyn2 %>% 
  # filtre
  filter(agregation == 5) %>%
  filter(infection.rate == 20) %>% 
  filter(proportion %in% c(0,10,90)) %>%
  filter(year == 5) %>%
  filter(tick %in% seq(720, 869, 1)) %>% 
  # ggplot
  ggplot() + 
  # dynamique des data : taux de remplissage du paysage par inf. -> courbe
  geom_point(mapping = aes(x = tick, y = dyn.inf, colour = "fill.level"),
             size = 0.5) + 
  facet_grid(. ~ proportion, labeller = label_both)
  
  geom_point(mapping = aes(x = tick, y = fill.level.non.occ, colour = "fill.level.non.occ"),
             size = 0.5) + 
  geom_point(mapping = aes(x = tick, y = fill.level.occ, colour = "fill.level.occ"),
             size = 0.5) + 
  # dynamique du control
  # geom_point(data = control.dyn %>% 
  #              # select var.
  #             select(- filename.control.dyn, - proba.birth, -proba.overwintering, - init.nb.adults, - visit.count, - tot.inf.cur, - dyn.curation) %>%
  #             # tot crop patches in the landscape
  #             mutate(stock.crops = 1089 - floor(1089*proportion/100)) %>% 
  #             # compute tick -> full landscape without pest control
  #             mutate(tick.full.th = ceiling(stock.crops/infection.rate)) %>% 
  #             # infection fill level in the landscape 
  #             mutate(fill.level = dyn.inf*100/stock.crops) %>% 
  #             # infected patches without predators (= control) in % (fill.level)
#             mutate(fill.level.non.occ = dyn.inf.non.occ*100/stock.crops) %>%  
#             # filter
#             filter(agregation == 1) %>%
#             filter(proportion %in% c(10,30,50,70,90)) %>%
#             filter(infection.rate %in% c(5,10,15,20)) %>%
#             filter(year == 10) %>%
#             filter(tick %in% seq(1620, 1769, 1)),
# mapping = aes(x = tick, y = fill.level, colour = "fill.level.control"),
# size = 0.5) +
# ligne verticale = tick th. auquel le paysage devrait être rempli si pas de contrôle
geom_vline(data=values.for.vline %>% 
             # same filter than data.dyn
             filter(agregation == 5) %>%
             filter(infection.rate == 20) %>% 
             filter(proportion %in% c(0,10,90)) %>%
             filter(year == 5) %>%
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
  geom_segment(data = data.totals2 %>% 
                 # same filter than data.dyn
                 filter(agregation == 5) %>%
                 filter(infection.rate == 20) %>% 
                 filter(proportion %in% c(0,10,90)) %>%
                 filter(year == 5),
               mapping = aes(x = 1769 , y = crop.loss, yend= 0, xend = 1769), 
               colour="grey50") +
  geom_point(data = data.totals2 %>% 
               # same filter than data.dyn
               filter(agregation == 5) %>%
               filter(infection.rate == 20) %>% 
               filter(proportion %in% c(0,10,90)) %>%
               filter(year == 5),
             aes(x = 1769, y = crop.loss, colour= "crop.loss"), size=3) +
  # x = ticks de l'année 10
  scale_x_continuous(name = expression("Tick"), 
                     breaks = c(1660, 1700, 1740),
                     labels = c("40","80","120"),
                     limits = c(1620, 1769)) + # les ticks qui nous intéressent
  # y = double axe -> à gauche : taux de remplissage / à droite : dyn. pests et adults
  scale_y_continuous(name = expression("Fill levels (%)"), #left axis
                     sec.axis = sec_axis(~ . * (max(data.dyn$dyn.adults)+10) / 100 , name = "Dynamics of adults (nb.)"), # right axis
                     limits = c(0, 100)) +
  # legends / axes
  scale_colour_manual(values = c("violet","blue", "orange", "cyan", "grey","red", "green")# , 
                      # labels=c("Norm Crop loss","Nb. adults","Nb. infected crop","Occ","Non occ","Fill level by pests","Tick landsc. full")
  ) +
  # facets
  # facet_grid(infection.rate ~ proportion, labeller = label_both) + 
  facet_grid(. ~ proportion, labeller = label_both) + 
  # titre
  ggtitle("LA (1, y10) :Dynamics of inf. (fill levels) / adults (nb) + crop-loss (total landscape)")

##### raw
  
data.dyn2 %>% 
    # filtre
    filter(agregation == 5) %>%
    filter(infection.rate == 20) %>% 
    filter(proportion == 10) %>%
    filter(year == 1) %>%
    #filter(tick %in% seq(720, 869, 1)) %>% 
    # ggplot
    ggplot() + 
    # dynamique des data : taux de remplissage du paysage par inf. -> courbe
    #geom_point(mapping = aes(x = tick, y = dyn.inf, colour = "dyn.inf"),
    #           size = 0.5) +
    geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level"),
             size = 0.5) + 
    geom_point(mapping = aes(x = tick, y = fill.level.non.occ, colour = "fill.level.non.occ"),
           size = 0.5) + 
    geom_point(mapping = aes(x = tick, y = fill.level.occ, colour = "fill.level.occ"),
             size = 0.5) + 
    facet_grid(. ~ nb.init, labeller = label_both)

## control

data.dyn2 %>% 
  filter(nb.init == 0) %>% 
  # filter(proportion == 0) %>% 
  filter(agregation == 5) %>%
  # filter(infection.rate == 20) %>% 
  filter(year == 1) %>% 
  ggplot() +
  # control
  geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level.c"),
             size = 0.5) + 
  facet_grid(infection.rate ~ proportion, labeller = label_both)

## control + fill level data

data.dyn2 %>% 
  filter(nb.init == 0) %>% 
  # filter(proportion == 0) %>% 
  filter(agregation == 5) %>%
  # filter(infection.rate == 20) %>% 
  filter(year == 1) %>% 
  ggplot() +
  # control
  geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level.c"),
                      size = 0.5) + 
  # sélection nb.init
  geom_point(data = data.dyn2 %>% 
                # filtre
               filter(agregation == 5) %>%
               # filter(infection.rate == 20) %>% 
               filter(year == 1) %>%
               filter(nb.init == 10),
               mapping = aes(x = tick, y = fill.level, colour = "fill.level.d"),
             size = 0.5) + 
  facet_grid(infection.rate ~ proportion, labeller = label_both)

## control + fill level occ / non occ

data.dyn2 %>% 
  filter(nb.init == 0) %>% 
  # filter(proportion == 0) %>% 
  filter(agregation == 5) %>%
  # filter(infection.rate == 20) %>% 
  filter(year == 1) %>% 
  ggplot() +
  # control
  geom_point(mapping = aes(x = tick, y = fill.level, colour = "fill.level.c"),
             size = 0.5) + 
  # sélection nb.init
  geom_point(data = data.dyn2 %>% 
               # filtre
               filter(agregation == 5) %>%
               # filter(infection.rate == 20) %>% 
               filter(year == 1) %>%
               filter(nb.init == 10),
             mapping = aes(x = tick, y = fill.level.non.occ, colour = "fill.level.d.non.occ"),
             size = 0.5) + 
  geom_point(data = data.dyn2 %>% 
               # filtre
               filter(agregation == 5) %>%
               # filter(infection.rate == 20) %>% 
               filter(year == 1) %>%
               filter(nb.init == 10),
             mapping = aes(x = tick, y = fill.level.occ, colour = "fill.level.d.occ"),
             size = 0.5) + 
  facet_grid(infection.rate ~ proportion, labeller = label_both)
