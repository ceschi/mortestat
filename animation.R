# ajout des informations pour l'animation
# ---------------------------------------

library(gganimate)
library(broom)

dc <- bind_rows(dc_jour,dc_insee_jour,deces_ecdc,deces_insee_provisoires)
# on démarre du fichier dc utilisé pour le graphique surmortalité
#
# correction année et mois pour les données de 2020
dc <- dc %>%
  mutate(jour = ifelse(annee==2020, day(date_fictive),jour),
         mois = ifelse(annee==2020, month(date_fictive),mois))
dc <- dc %>%
  arrange(annee,mois,jour) %>%
  filter(categorie !="ecdc") # on enleve ecdc pour l'instant
# 
# dc %>% 
#   select(annee,mois,jour,date_fictive,N) %>% 
#   ggplot(aes(date_fictive,N,group=annee)) +
#   geom_line() +
#   scale_x_date(expand = expansion(add=c(2,2))) +
#   coord_polar()
# 
# df <- tibble(jour = seq(ymd("2001-01-01"), ymd("2004-01-30"), by ="day") ,
#              valeur = 10 + rnorm(1125)) %>%
#   mutate(valeur = valeur + row_number()/100 )
# 
# 
# df <- df %>%
#   mutate(annee = year(jour),
#          annee_fictive = 2020,
#          restant = str_sub(jour,5,10),
#          date_fictive = ymd(glue("{annee_fictive}{restant}")))
# 
# df %>%
#   ggplot(aes(jour,valeur)) +
#   geom_path() +
#   coord_polar() 




# ralentir 2020
# 4 fois plus lent

dc <- bind_rows( dc %>% filter(annee != 2020) %>% mutate(repetition = 0),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 1),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 2),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 3),
                 dc %>% filter(annee == 2020) %>% mutate(repetition = 4) ) %>%
  arrange(annee,mois,jour) %>%
  mutate(groupage = as.numeric(str_sub(annee,3,4))  ) %>%
  group_by(annee) %>%
  mutate(numero_frame = row_number()) %>%
  ungroup() %>%
  mutate(numero_frame = 366*(groupage-1) + numero_frame) %>%
  mutate(texte = annee)


# pour éviter que le label de l'année bouge dans tous les sens
# on le trace sur la courbe "loess"
# 
dc <- dc %>%
  left_join( dc %>% group_by(annee) %>% 
               do(augment(loess(N ~ numero_frame, .,span=.1))) %>% 
               ungroup() %>%
               select(annee,numero_frame,.fitted) ,
             by=c("annee","numero_frame"))

# dc <- dc %>%
#   mutate(numero_frame = (max(dc$numero_frame) + row_number())) %>%
#   mutate(texte = annee)


# graphique animé
# ---------------
p <- dc %>%
  #filter(annee %in% c(2001,2002,2003,2019)) %>% 
  
  mutate(couleur = case_when(annee == 2003 ~ "navyblue",
                             annee == 2020 ~ "firebrick1",
                             TRUE ~ "gray"),
         couleur_texte = ifelse(annee<2020,"black","firebrick1"),
         transparence = case_when(annee %in% c(2003,2020) ~ 1,
                                  TRUE ~ .5)) %>%
  #na.omit() %>%
  ggplot(aes(date_fictive, N, group = annee, 
             color=I(couleur), alpha=I(transparence)) )+
  #geom_smooth(data = . %>% filter(annee==2020) , se=F, span = .15) +
  geom_ribbon(data = . %>% filter(annee == 2019) ,
              aes(ymin = mini, ymax = maxi), 
              alpha=.5, fill="gray") +
  geom_line(size=.5, aes(group=annee)) +
  geom_point(color="black") +
  # geom_text_repel(aes(date_fictive,.fitted,
  #                     group=annee,
  #               label = texte, 
  #               color = I(couleur_texte)), 
  #               box.padding= 1,
  #           nudge_x = 5, alpha=1 ) +
  geom_text(aes(date_fictive,.fitted,
                group=annee,
                label = texte,
                color = I(couleur_texte)),
            size=6,
            nudge_x = 25, alpha=1 ) +
  
  # geom_line(data = dc_insee_jour, aes(date_fictive, N),
  #           color= "firebrick1") + 
  # geom_point(data = dc_insee_jour, 
  #            aes(date_fictive, N),
  #            color= "firebrick1") +
  # geom_text(data = dc_insee_jour,
  #           aes(date_fictive, N, label = texte),
  #           color= "firebrick1", nudge_x = 5) +
  # scale_x_date(date_labels = "%b",
  #              minor_breaks = NULL) +
coord_polar() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand=expansion(add=c(0,-26))) +
  scale_y_continuous(limits = c(0,3655),
                     breaks = c(0,500,1000,1500,2000,2500),
                     minor_breaks = NULL,
                     expand=expansion(add=c(0,-800))) +
  labs(title = "Nombre quotidien de décès en France, 2001-2020",
       subtitle = "En rouge, l'année 2020, en bleu 2003, en gris les années 2001 à 2019.",
       y=NULL,x=NULL,
       caption = "Sources : Fichier des décès sur data.gouv.fr et Fichier des décès sur insee.fr | Graphique : B. Coulmont") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot") +
  transition_reveal(along = numero_frame, keep_last = FALSE)

#p
animate(p, nframes = 10,fps=1)
#animate(p, nframes = 1000,width=800,height=500,fps=5, end_pause = 73)
animate(p , nframes= 8442, fps= 60,
        width=1200,height=1200, end_pause = 200,
        res = 130,
        renderer = av_renderer(glue("~/Desktop/output-deces-{today()}.mp4"), codec = "libx264"))

# animate(p , nframes= 245, fps=6,
#         width=1200,height=676, end_pause = 200,
#         res = 130,
#         renderer = av_renderer("~/Desktop/output-deces-2.mp4", codec = "libx264"))
# 