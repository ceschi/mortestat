library(tidyverse)
library(data.table)
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
  # drop zero recordings
  filter(count != 0) %>%
  # drop artefacts for leap years
  filter(!(d == '29' &
           m == '02' & 
           !(year %in% c('16','20')))
         ) %>% 
  mutate(year = as.character(year),
         d = as.character(d),
         m = as.character(m),
         date = paste(d, m, year, sep = '/'),
         date = lubridate::dmy(date),
         y = lubridate::year(date),
         # key for grouping and plotting
         dm = paste0(d, '-', m)) %>% 
  select(-year, -d, -m)

rm(m_wide)
gc()

# aggregate nation-wide
agg <- mlong %>% 
  filter(gen == 't') %>%
  select(-gen) %>% 
  group_by(date) %>% 
  mutate(tot = sum(count, na.rm = T)) %>%
  select(date, y, dm, tot) %>% 
  distinct() %>% 
  arrange(date) 

data.table::fwrite(mlong, './decessi_long_agg.csv')
rm(mlong)

top <- agg %>%
  filter(y != 2020) %>% 
  select(date, dm, y, tot) %>% 
  mutate(y = as.character(y)) %>% 
  arrange(date)


mid <- agg %>% 
  filter(y != 2020) %>% 
  group_by(dm) %>% 
  mutate(AVG = mean(tot, na.rm = T),
         Min = min(tot, na.rm = T),
         Max = max(tot, na.rm = T)) %>% 
  ungroup() %>% 
  select(-y, -tot) %>%
  pivot_longer(c(AVG, Min, Max),
               names_to = 'y',
               values_to = 'tot') %>%
  select(-date,) %>% 
  distinct() %>%
  left_join(y = .,
            x = agg %>%
              filter(y == 2016) %>%
              select(date, dm),
            by = c('dm')) %>% 
  mutate(y = as.character(y)) %>% 
  arrange(y,date)

bottom <- agg %>% 
  select(date, dm, y, tot) %>% 
  filter(y == 2020) %>% 
  mutate(y = as.character(y)) %>%  
  arrange(date)

plottable <- bind_rows(top, mid, bottom) %>% 
  ungroup() %>% 
  filter(!(y %in% c('Min', 'Max'))) %>%
  mutate(lincol = case_when(y %in% as.character(2015:2019) ~ 'grey',
                            y == '2020' ~ 'firebrick1',
                            # y %in% c('Min', 'Max') ~ 'green'),
                            y == 'AVG' ~ 'black'),
         alpa = case_when(y %in% as.character(2015:2019) ~ 1,
                          y == '2020' ~ 1,
                          # y %in% c('Min', 'Max') ~ .2,
                          y == 'AVG' ~ 1),
         sizze = case_when(y %in% as.character(2015:2019) ~ .5,
                           y == '2020' ~ 1,
                           # y %in% c('Min', 'Max') ~ .5,
                           y == 'AVG' ~ 1),
         frame = row_number(),
         plt_date = as.Date(dm, format = '%d-%m'),
         ma14 = data.table::frollmean(tot, 14))

future::plan("multisession")

plt <- plottable %>% 
  filter(!(y %in% c('Min', 'Max'))) %>% 
  ggplot(aes(x = plt_date,
             y = tot,
             group = y)) +
  geom_line(aes(colour = I(lincol),
                size = I(sizze),
                alpha = I(alpa))) + 
  scale_x_date(breaks = '1 month',
               date_labels = "%b", 
               expand=expansion(add=c(0,-6.5))) +
  geom_point(aes(group = 1),
             size = 2,
             colour = 'black') +
  geom_text(aes(y = ma14,
                label = y,
                group = 1),
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

# rendered
plt_rend <- plt + transition_reveal(frame) #; plt_rend

rm(top, mid, bottom)
gc()

tic('Rendering')

# lowres for tests
# plt_anim <- animate(plt_rend,
#                     res = 10,
#                     fps = 15,
#                     nframes = max(plottable$frame),
#                     height = 5,
#                     width = 9,
#                     unit = 'in',
#                     duration = 20,
#                     end_pause = 100)

plt_anim <- animate(plt_rend,
                    res = 250,
                    fps = 30,
                    nframes = max(plottable$frame),
                    height = 5,
                    width = 9,
                    unit = 'in',
                    duration = 20,
                    # which is faster?
                    # renderer = av_renderer(),
                    # renderer = ffmpeg_renderer(),
                    # renderer = gifski_renderer(loop = T),
                    end_pause = 100
                    )

anim_save(filename = 'agg_daily.gif', 
          animation = plt_anim)
toc()