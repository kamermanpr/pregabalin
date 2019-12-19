# Load packages
library(tidyverse)
library(scales)

# Pregabalin
## Import data
other_gabapentin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0&format=csv') %>%
    select(date, items) %>%
    rename(other = items)
neup_gabapentin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0407030AD&format=csv') %>%
    select(date, items) %>%
    rename(neup = items)

## Join data
gabapentin <- other_gabapentin %>%
    left_join(neup_gabapentin)

## Sum drugs
gabapentin <- gabapentin %>%
    mutate(total = other + neup)

## Plot
### Total
t <- gabapentin %>%
    select(date, total) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total'),
                        labels = c('Total'),
                        ordered = TRUE))

pp_t <- ggplot(data = t) +
    aes(x = date,
        ymax = value,
        y = value,
        fill = key,
        colour = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(t$value) + 30000,
             ymax = max(t$value) + 90000,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(tl2g$value) + 60000,
             x = as.Date("2019-04-01"),
             size = 6) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9')) +
    scale_colour_manual(values = c('#0072B2')) +
    labs(subtitle = 'Total number of prescribed items of gabapentin',
         y = 'Number of items',
         x = 'Date', 
         caption = 'C: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'none',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15))

ggsave('gabapenton-grand-total.png', pp_t, width = 9.1, height = 7.86)
