library(tidyverse)
library(data.table)
library(lubridate)
library(gganimate)
library(tictoc)

m_wide <- readr::read_csv("./comuni_giornaliero_30novembre.csv", 
                          na = 'n.d.', 
                          # just a chunk for testing
                          # n_max = 100,
                          locale = locale(encoding = "ISO-8859-1"))

m_wide <- rename_with(m_wide, .fn = tolower) %>% 
  # drop extra ids cols to shrink size
  select(-reg,
         -prov,
         -starts_with('nome_'),
         -tipo_comune)

m_wide <- m_wide %>% 
  mutate(ch = nchar(ge),
         d = substr(ge, ch-1, ch),
         m = substr(ge, 1, ch-2)) %>% 
  select(-ge, -ch)

mlong <- m_wide %>% 
  pivot_longer(cols = matches("[mtf]_[0-9]{2}"), 
               values_drop_na = T,
               names_to = c('gen', 'year'), 
               names_sep = '_', 
               values_to = 'count') %>% 
  # drop zero recordings
  filter(count != 0) %>%
  # drop artefacts for leap years
  # filter(!(d == '29' &
  #          m == '02' & 
  #          !(year %in% c('16','20')))
  #        ) %>% 
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
gc(' ')

# aggregate nation-wide
agg <- mlong %>% 
  filter(gen == 't') %>%
  select(-gen) %>% 
  group_by(date) %>% 
  mutate(tot = sum(count, na.rm = T)) %>%
  select(date, y, dm, tot) %>% 
  distinct() %>% 
  arrange(date) %>% 
  ungroup() %>% 
  # fix holes for leap years
  complete(dm, y)

data.table::fwrite(mlong, 
                   './decessi_long_agg.csv', 
                   verbose = T)
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

plo2 <- agg %>% 
  group_by(dm) %>% 
  mutate(avg = mean(tot[y!=2020], na.rm = T),
         min = min(tot[y!=2020], na.rm = T),
         max = max(tot[y!=2020], na.rm = T)) %>% 
  ungroup() %>% 
  arrange(date)

plottable <- bind_rows(top, mid, bottom) %>% 
  ungroup() %>% 
  filter(!(y %in% c('Min', 'Max'))) %>%
  mutate(lincol = case_when(y %in% as.character(2011:2019) ~ 'grey',
                            y == '2020' ~ 'firebrick1',
                            # y %in% c('Min', 'Max') ~ 'green'),
                            y == 'AVG' ~ 'black'),
         alpa = case_when(y %in% as.character(2011:2019) ~ 1,
                          y == '2020' ~ 1,
                          # y %in% c('Min', 'Max') ~ .2,
                          y == 'AVG' ~ 1),
         sizze = case_when(y %in% as.character(2011:2019) ~ .5,
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
                    duration = 30,
                    # which is faster?
                    # renderer = av_renderer(),
                    # renderer = ffmpeg_renderer(),
                    # renderer = gifski_renderer(loop = T),
                    end_pause = 100
                    )

anim_save(filename = 'agg_daily.gif', 
          animation = plt_anim)
toc()

##### excess deaths count ######################################################

#import back mlong

mlong_gen <- mlong %>% filter(gen == 't')

avg <- mlong_gen %>% 
  filter(y!=2020) %>% 
  group_by(y, dm) %>%
  mutate(tot = sum(count)) %>% 
  select(date, y, dm, tot) %>% 
  distinct() %>%
  group_by(dm) %>% 
  mutate(avg20 = mean(tot, na.rm =  T)) %>% 
  arrange(date) %>% 
  pivot_wider(id_cols = c('dm', 'avg20'),
              names_from = c('y'), 
              values_from = c('tot'))

solo20 <- mlong_gen %>% 
  filter(y == 2020) %>% 
  group_by(y, dm) %>% 
  mutate(tot = sum(count)) %>% 
  select(date, y, dm, tot) %>% 
  distinct() %>% 
  arrange(date) %>% 
  pivot_wider(id_cols = c('dm'),names_from = c('y'), values_from = c('tot'))

byear <- full_join(x = avg, y = solo20, by = c('dm'))


## moar data, eurostat

nuts3 <- read_tsv('https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk3_t.tsv.gz', na = ':', trim_ws = T)
nuts2 <- read_tsv('https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk2_ts.tsv.gz', na = ':', trim_ws = T)
euweekly <- read_tsv('https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mwk_ts.tsv.gz', na = ':', trim_ws = T)


n3_clean <- nuts3 %>% mutate(geo = str_remove(`unit,geo\\time`, 'NR,'), 
                 `unit,geo\\time` = NULL,
                 c = nchar(geo)) %>% 
  filter(c == 2) %>% 
  relocate(geo, c) %>%
  mutate(across(-c('geo', 'c'), .fns = ~str_remove(.x, pattern = ' p') %>% as.numeric)) %>% 
  pivot_longer(cols = -c('geo', 'c'), 
               names_to = c('year', 'w'),
               names_sep = 'W',
               values_to = 'tot') %>% 
  filter(w != 99) %>% 
  select(-c) %>% 
  arrange(geo, year, w) %>% 
  mutate(date = (ymd('2020-01-01') + weeks(as.numeric(w)-1))) %>% 
  group_by(geo) %>% 
  mutate(nframe = row_number(),
         colour = case_when(year == 2020 ~ 'red',
                            year != 2020 ~ 'grey'),
         alp = case_when(year == 2020 ~ 1,
                         year != 2020 ~ .5),
         sizze = case_when(year == 2020 ~ 1,
                           year != 2020 ~ .5))


plt_n3 <- n3_clean %>% 
  ggplot(aes(x = date, 
             y = tot, 
             group = interaction(geo, year)))+
  geom_line(aes(colour = I(colour),
                alpha = I(alp),
                size = I(sizze))) + 
  facet_wrap(.~geo,
             scales = 'free_y') +
  theme_minimal() +   
  scale_x_date(breaks = '1 month',
               date_labels = "%b")

plt_n3_anim <- plt_n3 +
  transition_reveal(along = nframe)


euweekly_clean <- euweekly %>% 
  separate(`sex,unit,geo\\time`, 
           into = c('sex', 'a', 'country'),
           sep = ',') %>% 
  mutate(a = NULL,
         sex = case_when(sex == 'M' ~ 'Male',
                         sex == 'F' ~ 'Female',
                         sex == 'T' ~ 'Total'),
         across(-c('sex', 'country'), 
                .fns = ~str_remove(.x, pattern = ' p') %>% 
                        as.numeric)
         ) %>% 
  pivot_longer(cols = -c(sex, country), 
               names_to = c('year', 'week'),
               values_to = 'count',
               names_sep = 'W') %>% 
  filter(week != 99) %>% 
  arrange(country, year, week, sex) %>% 
  mutate(date = (ymd('2020-01-01') + weeks(as.numeric(week)-1)),
         colour = case_when(year == 2020 ~ 'red',
                            year != 2020 ~ 'grey'),
         alp = case_when(year == 2020 ~ 1,
                         year != 2020 ~ .5),
         sizze = case_when(year == 2020 ~ 1,
                           year != 2020 ~ .5)) %>% 
  group_by(country, sex) %>% 
  mutate(nframe = row_number())


plt_euweek <- euweekly_clean %>%
  ggplot(aes(x = date,
             y = count,
             group = interaction(country, year, sex)))+
  geom_line(aes(colour = I(colour),
                alpha = I(alp),
                size = I(sizze))) +
  facet_wrap(.~ country + sex, scale = 'free_y')


plt_euweek_anim <- plt_euweek + transition_reveal(nframe)


euweekly_clean %>% 
  filter(sex == 'Total') %>%
  group_by(country, year) %>%
  mutate(sums_y = cumsum(count)) %>% 
  distinct() %>%
  ggplot(aes(x = count, group = interaction(year))) + 
  geom_density(aes(fill = I(colour))) + 
  facet_wrap(.~country, scale = 'free')
