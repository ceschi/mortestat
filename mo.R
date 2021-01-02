library(tidyverse)
library(lubridate)
library(gganimate)
library(tictoc)

m_wide <- readr::read_csv("./comuni_giornaliero_31ottobre.csv", 
                          na = 'n.d.', 
                          # just a chunk for testing
                          # n_max = 100,
                          locale = locale(encoding = "ISO-8859-1"))

m_wide <- rename_with(m_wide, .fn = tolower)

m_wide <- m_wide %>% 
  mutate(ch = nchar(ge),
         d = substr(ge, ch-1, ch),
         m = substr(ge, 1, ch-2)) %>% 
  select(-ge, -ch)

mlong <- m_wide %>% 
  pivot_longer(cols = 9:26, 
               values_drop_na = T,
               names_to = c('gen', 'year'), 
               names_sep = '_', 
               values_to = 'count') %>% 
  filter(count != 0) %>% 
  filter(!(d == '29' &
             m == '02' & 
             !(year %in% c('16','20')))
         ) %>% 
  mutate(#year = (as.integer(year) + 2000),
         year = as.character(year),
         d = as.character(d),
         m = as.character(m),
         date = paste(d, m, year, sep = '/'),
         date = lubridate::dmy(date),
         d = lubridate::day(date),
         m = lubridate::month(date),
         y = lubridate::year(date),
         yd = lubridate::yday(date),
         dm = paste0(d, '-', m)) %>% 
  select(-year)

rm(m_wide)
gc()

# smaller, for plotting
conti <- mlong %>% 
  filter(gen == 't') %>%
  select(-gen) %>% 
  group_by(date) %>% 
  mutate(tot = sum(count, na.rm = T)) %>%
  # group_by(date, cl_eta) %>% 
  # mutate(tot_age = sum(count, na.rm = T)) %>% 
  # select(date, d, m, y, dm, yd, tot, cl_eta, tot_age) %>% 
  # ungroup() %>% 
  select(date, d, m, y, dm, yd, tot) %>% 
  distinct() %>% 
  arrange(date) 
  # mutate(col = case_when(y == 2020 ~ 'firebrick1',
  #                        y != 2020 ~ 'grey'),
  #        col = factor(col)) 

write_csv(mlong, './decessi_long_agg.csv')

temp <- conti %>% 
  filter(y != 2020) %>% 
  group_by(dm) %>% 
  mutate(AVG = mean(tot, na.rm = T),
         Min = min(tot, na.rm = T),
         Max = max(tot, na.rm = T)) %>% 
  # group_by(dm, cl_eta) %>% 
  # mutate(avg20_age = mean(tot_age, na.rm = T)) %>% 
  ungroup()

final <- full_join(by = c("date", 
                          "d",
                          "m",
                          "y",
                          "yd",
                          'dm',
                          "tot",
                          'col'),
                   x = conti,
                   y = temp) %>% 
  arrange(date, cl_eta) %>% 
  mutate(frame = row_number())

rm(temp, mlong)
gc()


future::plan("multisession")
# base for animation
plt <- final %>% 
  arrange(date) %>% 
  ggplot(aes(x = yd,
             y = tot,
             group = interaction(dm))) +
  geom_line(aes(colour = I(col)),
            size = 1) + 
  # scale_alpha(range = c(.2, 1)) +
  geom_line(aes(x = dm, 
                y = avg20), 
            size = 0.5,
            colour = 'black') + 
  geom_point(aes(x = dm),
             size = 3,
             colour = 'black') +
  geom_text(aes(label = y),
            size = 3,
            colour = 'black',
            nudge_x = 15) +
  theme_minimal() +
  # coord_polar() +
  labs(title = 'Daily total deaths in Italy, 2015:01-2020:10',
       subtitle = 'Grey 2015 to 19; black 2015-19 daily avg; red 2020',
       caption = 'Source: ISTAT',
       x = 'Day of the year',
       y = NULL)

gc(' ')

# rendered
plt_rend <- plt + transition_reveal(frame)

tic('Rendering')
gc()
# plt_anim <- animate(plt_rend,
#                     res = 500,
#                     fps = 24,
#                     height = 5,
#                     # height = 10,
#                     width = 9,
#                     unit = 'in',
#                     duration = 20,
#                     end_pause = 100,
#                     # renderer = av_renderer()
#                     # renderer = ffmpeg_renderer()
#                     # renderer = gifski_renderer(loop = T)
#                     )

plt_anim <- animate(plt_rend,
                    res = 200,
                    fps = 30,
                    nframes = max(plottable$frame),
                    height = 5,
                    # height = 10,
                    width = 9,
                    unit = 'in',
                    duration = 20,
                    end_pause = 100,
                    # renderer = av_renderer()
                    # renderer = ffmpeg_renderer()
                    # renderer = gifski_renderer(loop = T)
)


anim_save(filename = 'agg_daily.gif', 
          animation = plt_anim)
toc()


# by age class, gender
# conti_full <- mlong %>% 
#   select(date, yd, d, m, y, cl_eta, gen, count) %>% 
#   distinct() %>% 
#   group_by(yd, gen, cl_eta) %>% 
#   mutate(tot_sa = sum(count),
#          avg20_sa = case_when(y != 2020 ~ mean(count))
#          )

########## all long format #####################################################

top <- conti %>%
  filter(y != 2020) %>% 
  select(date, dm, yd, y, tot) %>% 
  mutate(y = as.character(y)) %>% 
  arrange(date)


mid <- conti %>% 
  filter(y != 2020) %>% 
  group_by(dm) %>% 
  mutate(AVG = mean(tot, na.rm = T),
         Min = min(tot, na.rm = T),
         Max = max(tot, na.rm = T)) %>% 
  ungroup() %>% 
  select(-y, -tot) %>%
  pivot_longer(c(avg20, miin, maax),
               names_to = 'y',
               values_to = 'tot') %>%
  select(-date, -d,-m) %>% 
  distinct() %>%
  left_join(y = .,
            x = conti %>%
              filter(y == 2016) %>%
              select(date, dm, yd),
            by = c('dm', 'yd')
            ) %>% 
  mutate(y = as.character(y)) %>% 
  arrange(y,date)

bottom <- conti %>% 
  select(date, dm, yd, y, tot) %>% 
  filter(y == 2020) %>% 
  mutate(y = as.character(y)) %>%  
  arrange(date)

plottable <- bind_rows(top, mid, bottom, bottom) %>% 
  ungroup() %>% 
  mutate(lincol = case_when(y %in% as.character(2015:2019) ~ 'grey',
                            y == '2020' ~ 'firebrick1',
                            y == 'AVG' ~ 'black',
                            y %in% c('miin', 'maax') ~ 'navyblue'),
         alpa = case_when(y %in% as.character(2015:2019) ~ 1,
                          y == '2020' ~ 1,
                          y == 'AVG' ~ 1,
                          y %in% c('miin', 'maax') ~ .2),
         sizze = case_when(y %in% as.character(2015:2019) ~ .5,
                           y == '2020' ~ 1,
                           y == 'AVG' ~ 1,
                           y %in% c('Min', 'Max') ~ .5),
         frame = row_number(),
         plt_date = as.Date(dm, format = '%d-%m')) %>% 
  # group_by(y) %>% 
  mutate(ma14 = data.table::frollmean(tot, 14))


plt <- 
plottable %>% 
  ggplot(aes(x = plt_date,
             y = tot,
             group = y)) +
  geom_line(aes(colour = I(lincol),
                size = I(sizze),
                alpha = I(alpa))) + 
  scale_x_date(breaks = '1 month',date_labels = "%b", expand=expansion(add=c(0,-6.5))) +
  geom_point(size = 3,
             colour = 'black') +
  geom_text(aes(y = ma14,
                label = y),
            size = 3,
            colour = 'black',
            nudge_x = 7) +
  theme_minimal() +
  # coord_polar() +
  labs(title = 'Daily total deaths in Italy, 2015:01-2020:10',
       subtitle = 'Grey 2015 to 19; black 2015-19 daily avg; red 2020',
       caption = 'Source: ISTAT',
       x = NULL,
       y = NULL)
