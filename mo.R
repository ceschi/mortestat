library(tidyverse)
library(lubridate)
library(gganimate)


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
  filter(!(d == '29' &
             m == '02' & 
             !(year %in% c('16','20')))
         ) %>% 
  mutate(#year = (as.integer(year) + 2000),
         year = as.character(year),
         d = as.character(d),
         m = as.character(m),
         date = paste(d, m, year, sep = '/'),
         date = lubridate::dmy(date)) %>% 
  select(-year,-d,-m) %>% 
  mutate(d = lubridate::day(date),
         m = lubridate::month(date),
         y = lubridate::year(date),
         yd = lubridate::yday(date))

rm(m_wide)
gc()

# smaller, for plotting
conti <- mlong %>% 
  filter(gen == 't') %>%
  group_by(date) %>% 
  mutate(tot = sum(count, na.rm = T)) %>%
  group_by(date, cl_eta) %>% 
  mutate(tot_age = sum(count, na.rm = T)) %>% 
  select(date, d, m, y, yd, gen, tot, cl_eta, tot_age) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(date) %>% 
  mutate(col = case_when(y == 2020 ~ 'firebrick1',
                         y != 2020 ~ 'grey'),
         col = factor(col)) 

temp <- conti %>% 
  filter(y != 2020) %>% 
  group_by(yd) %>% 
  mutate(avg20 = mean(tot, na.rm = T),
         miin = min(tot, na.rm = T),
         maax = max(tot, na.rm = T)) %>% 
  group_by(yd, cl_eta) %>% 
  mutate(avg20_age = mean(tot_age, na.rm = T)) %>% 
  ungroup()

final <- full_join(by = c("date", 
                          "d",
                          "m",
                          "y",
                          "tot",
                          "yd",
                          'col',
                          'cl_eta',
                          'tot_age',
                          'gen'),
                   x = conti,
                   y = temp) %>% 
  arrange(date, gen, cl_eta) %>% 
  mutate(frame = row_number())

rm(temp, mlong)
gc()


future::plan("multisession")
# base for animation
plt <- final %>% 
  ggplot(aes(x = yd,
             y = tot,
             group = y)) +
  geom_line(aes(colour = I(col)),
            size = 2) + 
  # scale_alpha(range = c(.2, 1)) +
  geom_line(aes(x = yd, 
                y = avg20), 
            size = 1.5,
            colour = 'black') + 
  geom_point(size = 3,
             colour = 'black') +
  geom_text(aes(label = y),
            size = 3,
            colour = 'black',
            nudge_x = 25) +
  theme_minimal() +
  labs(title = 'Daily total deaths in Italy, 2015:01-2020:10',
       subtitle = 'Grey 2015 to 19; black 2015-19 daily avg; red 2020',
       caption = 'Source: ISTAT',
       x = NULL,
       y = NULL)

gc(' ')

# rendered
plt_rend <- plt + transition_reveal(frame)

animate(plt_rend,
        res = 1000,
        fps = 24,
        height = 6,
        width = 10,
        unit = 'in',
        duration = 60,
        end_pause = 200,
        renderer = av_renderer(file = './agg_daily.mp4')
        # renderer = gifski_renderer(file = './agg_daily.gif',
        #                            loop = T)
        )


# by age class, gender
# conti_full <- mlong %>% 
#   select(date, yd, d, m, y, cl_eta, gen, count) %>% 
#   distinct() %>% 
#   group_by(yd, gen, cl_eta) %>% 
#   mutate(tot_sa = sum(count),
#          avg20_sa = case_when(y != 2020 ~ mean(count))
#          )
  