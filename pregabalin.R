# Load packages
library(tidyverse)
library(scales)

# Pregabalin
## Import data
total_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AE&format=csv') %>%
    select(date, items) %>%
    rename(total = items)
generic_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAA&format=csv') %>%
    select(date, items) %>%
    rename(generic = items)
lyrica_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBB&format=csv') %>%
    select(date, items) %>%
    rename(lyrica = items)
rewisca_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBC&format=csv') %>%
    select(date, items) %>%
    rename(rewisca = items)
lecaent_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBD&format=csv') %>%
    select(date, items) %>%
    rename(lecaent = items)
alzain_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBE&format=csv') %>%
    select(date, items) %>%
    rename(alzain = items)
axalid_pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBF&format=csv') %>%
    select(date, items) %>%
    rename(axalid = items)

## Join data
pregabalin <- total_pregabalin %>%
    left_join(generic_pregabalin) %>%
    left_join(lyrica_pregabalin) %>%
    left_join(rewisca_pregabalin) %>%
    left_join(lecaent_pregabalin) %>%
    left_join(alzain_pregabalin) %>%
    left_join(axalid_pregabalin)

## Sum "other" drugs
pregabalin <- pregabalin %>%
    mutate(other = rewisca + lecaent + alzain + axalid,
           generic_grand = generic + other,
           lyrica_2 = generic + lyrica,
           lyrica_3 = generic_grand + lyrica,
           other_2 = other + lyrica_2,
           alzain_2 = lyrica_2 + alzain,
           axalid_2 = alzain_2 + axalid,
           lecaent_2 = axalid_2 + lecaent,
           rewisca_2 = lecaent_2 + rewisca)

## Plot
### Total
t <- pregabalin %>%
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
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9')) +
    scale_colour_manual(values = c('#0072B2')) +
    labs(subtitle = 'Total number of prescribed items of pregabalin',
         y = 'Number of items',
         x = 'Date') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15))

ggsave('grand-total.png', pp_t, width = 9.1, height = 7.86)

### Total, generic
tg <- pregabalin %>%
    select(date, total, generic) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total', 'generic'),
                        labels = c('Total', 'Generics (without named brands*)'),
                        ordered = TRUE))

pp_tg <- ggplot(data = tg) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9', '#E18E4C')) +
    scale_colour_manual(values = c('#0072B2', '#D55E00')) +
    labs(subtitle = 'Total items of pregabalin prescribed as generics (without named brands)',
         y = 'Number of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  ') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('generics-excluding-named-brands.png', pp_tg, width = 9.1, height = 7.86)

### Total, generic_grand
tgg <- pregabalin %>%
    select(date, total, generic_grand) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total', 'generic_grand'),
                        labels = c('Total', 'Generics (with named brands*)'),
                        ordered = TRUE))

pp_tgg <- ggplot(data = tgg) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9', '#E18E4C')) +
    scale_colour_manual(values = c('#0072B2', '#D55E00')) +
    labs(subtitle = 'Total items of pregabalin prescribed as generics (with named brands*)',
         y = 'Number of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  ') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('generics-with-named-brands.png', pp_tgg, width = 9.1, height = 7.86)

### Total, generic, lyrica_2
tl2g <- pregabalin %>%
    select(date, total, generic, lyrica_2) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total', 'lyrica_2', 'generic'),
                        labels = c('Total', 'Lyrica', 'Generics (without named brands*)'),
                        ordered = TRUE))

pp_tl2g <- ggplot(data = tl2g) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9', '#70BB70', '#E18E4C')) +
    scale_colour_manual(values = c('#0072B2', '#339F34', '#D55E00')) +
    geom_vline(xintercept = as.numeric(as.Date("2015-03-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2017-07-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(tl2g$value) + 30000,
             ymax = max(tl2g$value) + 90000,
             xmin = as.Date("2015-01-01"),
             xmax = as.Date("2015-05-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "A",
             y = max(tl2g$value) + 60000,
             x = as.Date("2015-03-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(tl2g$value) + 30000,
             ymax = max(tl2g$value) + 90000,
             xmin = as.Date("2017-05-01"),
             xmax = as.Date("2017-09-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "B",
             y = max(tl2g$value) + 60000,
             x = as.Date("2017-07-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(tl2g$value) + 30000,
             ymax = max(tl2g$value) + 90000,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(tl2g$value) + 60000,
             x = as.Date("2019-04-01"),
             size = 6) +
    labs(subtitle = 'Total items of pregabalin prescribed as generics (without named brands*) and Lyrica',
         y = 'Number of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  \nA: Start of Lyrica prescription restrictions     \nB: End of Lyrica prescription restrictions      \nC: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('lyrica-generics-excluding-named-brands.png', pp_tl2g, width = 9.1, height = 7.86)

### Total, generic, lyrica_2, other_2
to2l2g <- pregabalin %>%
    select(date, total, generic, lyrica_2, other_2) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total', 'other_2', 'lyrica_2', 'generic'),
                        labels = c('Total', 'Named generics*', 'Lyrica', 'Other generics'),
                        ordered = TRUE))

pp_to2l2g <- ggplot(data = to2l2g) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9', '#939393', '#70BB70', '#E18E4C')) +
    scale_colour_manual(values = c('#0072B2', '#666666', '#339F34', '#D55E00')) +
    geom_vline(xintercept = as.numeric(as.Date("2015-03-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2017-07-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(to2l2g$value) + 30000,
             ymax = max(to2l2g$value) + 90000,
             xmin = as.Date("2015-01-01"),
             xmax = as.Date("2015-05-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "A",
             y = max(to2l2g$value) + 60000,
             x = as.Date("2015-03-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(to2l2g$value) + 30000,
             ymax = max(to2l2g$value) + 90000,
             xmin = as.Date("2017-05-01"),
             xmax = as.Date("2017-09-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "B",
             y = max(to2l2g$value) + 60000,
             x = as.Date("2017-07-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(to2l2g$value) + 30000,
             ymax = max(to2l2g$value) + 90000,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(to2l2g$value) + 60000,
             x = as.Date("2019-04-01"),
             size = 6) +
    labs(subtitle = 'Total items of pregabalin prescribed as named generics*, other generics, and Lyrica',
         y = 'Number of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  \nA: Start of Lyrica prescription restrictions     \nB: End of Lyrica prescription restrictions      \nC: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('lyrica-generics-and-named-brands.png', pp_to2l2g, width = 9.1, height = 7.86)

### Total, generic_grand, lyrica_3
tl3gg <- pregabalin %>%
    select(date, total, generic_grand, lyrica_3) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('total', 'lyrica_3', 'generic_grand'),
                        labels = c('Total', 'Lyrica', 'Generics (with named brands*)'),
                        ordered = TRUE))

pp_tl3gg <- ggplot(data = tl3gg) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0)) +
    scale_y_continuous(labels = number) +
    scale_fill_manual(values = c('#4C9CC9', '#70BB70', '#E18E4C')) +
    scale_colour_manual(values = c('#0072B2', '#339F34', '#D55E00')) +
    geom_vline(xintercept = as.numeric(as.Date("2015-03-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2017-07-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(tl3gg$value) + 30000,
             ymax = max(tl3gg$value) + 90000,
             xmin = as.Date("2015-01-01"),
             xmax = as.Date("2015-05-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "A",
             y = max(tl3gg$value) + 60000,
             x = as.Date("2015-03-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(tl3gg$value) + 30000,
             ymax = max(tl3gg$value) + 90000,
             xmin = as.Date("2017-05-01"),
             xmax = as.Date("2017-09-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "B",
             y = max(tl3gg$value) + 60000,
             x = as.Date("2017-07-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(tl3gg$value) + 30000,
             ymax = max(tl3gg$value) + 90000,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(tl3gg$value) + 60000,
             x = as.Date("2019-04-01"),
             size = 6) +
    labs(subtitle = 'Total items of pregabalin prescribed as generics (with named brands*), and Lyrica',
         y = 'Number of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  \nA: Start of Lyrica prescription restrictions     \nB: End of Lyrica prescription restrictions      \nC: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('lyrica-generics-grand.png', pp_tl3gg, width = 9.1, height = 7.86)


### Percent of total generic, lyrica_2, other_2
pct <- pregabalin %>%
    select(date, total, generic, lyrica, other) %>%
    mutate(generic_p = 100 * (generic/total),
           lyrica_p = 100 * (lyrica/total),
           other_p = 100 * (other/total)) %>%
    select(date, ends_with('_p')) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('generic_p', 'lyrica_p', 'other_p'),
                        labels = c('Other generics', 'Lyrica', 'Named generics*'),
                        ordered = TRUE))
pp_pct <- ggplot(data = pct) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0), alpha = 0.6) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    scale_fill_manual(values = c('#E18E4C', '#70BB70', '#939393')) +
    scale_colour_manual(values = c('#D55E00', '#339F34', '#666666')) +
    geom_vline(xintercept = as.numeric(as.Date("2015-03-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2017-07-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 20,
             ymax = max(pct$value) + 10,
             xmin = as.Date("2015-01-01"),
             xmax = as.Date("2015-05-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "A",
             y = max(pct$value) + 15,
             x = as.Date("2015-03-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 20,
             ymax = max(pct$value) + 10,
             xmin = as.Date("2017-05-01"),
             xmax = as.Date("2017-09-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "B",
             y = max(pct$value) + 15,
             x = as.Date("2017-07-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 10,
             ymax = max(pct$value) + 20,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(pct$value) + 15,
             x = as.Date("2019-04-01"),
             size = 6) +
    labs(subtitle = 'Percentage of pregabalin prescribed as named generics*, other generics, and Lyrica',
         y = 'Percentage of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  \nA: Start of Lyrica prescription restrictions     \nB: End of Lyrica prescription restrictions      \nC: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('percent-lyrica-generics-and-other-brands.png', pp_pct, width = 9.1, height = 7.86)

### Percent of total generic_grand, and lyrica
pct2 <- pregabalin %>%
    select(date, total, generic_grand, lyrica) %>%
    mutate(generic_p = 100 * (generic_grand/total),
           lyrica_p = 100 * (lyrica/total)) %>%
    select(date, ends_with('_p')) %>%
    gather(key = key,
           value = value,
           -date) %>%
    mutate(key = factor(key,
                        levels = c('generic_p', 'lyrica_p'),
                        labels = c('Generics (with named brands)', 'Lyrica'),
                        ordered = TRUE))

pp_pct2 <- ggplot(data = pct2) +
    aes(x = date,
        ymax = value,
        y = value,
        colour = key,
        fill = key) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = 0), alpha = 0.6) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    scale_fill_manual(values = c('#E18E4C', '#70BB70', '#939393')) +
    scale_colour_manual(values = c('#D55E00', '#339F34', '#666666')) +
    geom_vline(xintercept = as.numeric(as.Date("2015-03-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2017-07-01")),
               linetype = 2) +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")),
               linetype = 2) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 20,
             ymax = max(pct$value) + 10,
             xmin = as.Date("2015-01-01"),
             xmax = as.Date("2015-05-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "A",
             y = max(pct$value) + 15,
             x = as.Date("2015-03-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 20,
             ymax = max(pct$value) + 10,
             xmin = as.Date("2017-05-01"),
             xmax = as.Date("2017-09-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "B",
             y = max(pct$value) + 15,
             x = as.Date("2017-07-01"),
             size = 6) +
    annotate(geom = 'rect',
             ymin = max(pct$value) + 10,
             ymax = max(pct$value) + 20,
             xmin = as.Date("2019-02-01"),
             xmax = as.Date("2019-06-01"),
             fill = '#FFFFFF',
             colour = '#000000') +
    annotate(geom = 'text',
             label = "C",
             y = max(pct$value) + 15,
             x = as.Date("2019-04-01"),
             size = 6) +
    labs(subtitle = 'Percentage of pregabalin prescribed as generics (with named brands*), and Lyrica',
         y = 'Percentage of items',
         x = 'Date',
         caption = '*Alzain, Axalid, Lecaent, Reswica                  \nA: Start of Lyrica prescription restrictions     \nB: End of Lyrica prescription restrictions      \nC: Gabapentinoids classified as Category C') +
    theme_bw(base_size = 18) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10))

ggsave('percent-lyrica-generics-with-other-brands.png', pp_pct2, width = 9.1, height = 7.86)

