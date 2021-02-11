library(tidyverse)
library(rio)
library(ggTimeSeries)  # devtools::install_github('Ather-Energy/ggTimeSeries')
library(gridExtra)

source("functions.r", encoding="UTF-8")

# Hämta data över presskonferenser, döda i covid-19 samt innehållsanalysen.
pk <- press_conferences()
covid_deaths_per_day <- covid_deaths_socialstyrelsen()
m <- content_analysis()


# Figur 1 övre del: dödsfall per dag.
plot_deaths <- covid_deaths_per_day %>% 
  ggplot(aes(date, n)) +
    geom_col(width = 1, color="#bababa", alpha=.2) +
    geom_smooth(method="loess", span=.1, color="red", se=FALSE, formula=y~x) +
    scale_x_date(breaks = "1 month", date_labels = "%b",
                 limits = c(as.Date("2020-03-01"), as.Date("2020-12-31"))) +
    scale_y_continuous(breaks = seq(0, 140, by=20), 
                       limits = c(0, 140), expand=c(0, 0)) +
    theme_classic() +
    theme(axis.text = element_text(color="black"),
          panel.grid.major.y = element_line(color="#f5f5f5")) +
    labs(title = "Antal dödsfall med covid-19 i Sverige under 2020",
         #caption = "Källa: Socialstyrelsen",
         x = NULL,
         y = NULL)


# Antal presskonferenser totalt (dagar).
sum(pk$presskonferens)


# Lägg på antal kodade frågor/turordningar för varje datum.
pk_kodad <- pk %>% 
  left_join(m, by="dag") %>% 
  group_by(dag) %>% 
  summarize(kodade_fragor = n() - 1) %>% 
  left_join(pk, by="dag") %>% 
  mutate(kodad = kodade_fragor > 1,
         kategori = as.factor(case_when(
           kodad == TRUE          ~ "Kodad presskonferens",
           presskonferens == TRUE ~ "Presskonferens",
           TRUE                   ~ "Ingen presskonferens")))

# Figur 1 nedre del: presskonferenser per dag.
plot_pk <- ggplot_calendar_heatmap(pk_kodad, 'dag', 'kategori',
                                   monthBorderSize = 1, dayBorderSize = .5) +
  scale_fill_manual(values = c("transparent", "red", "#999999")) +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black", hjust = 0),
        axis.text = element_text(color="black"),
        legend.position = "none",
        panel.border = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(), 
        plot.margin = unit(c(0, 8, 0, 5), "mm")
  ) +
  #scale_x_continuous(
  # breaks = 3:12,
  #labels = c("mar", "apr", "maj", "jun", "jul", "aug", "sep", "okt", "nov", "dec"),
  #expand = c(0, 0)
  #) +
  scale_y_continuous(
    trans = "reverse",
    breaks = c(1:7),
    labels = c("Mån", "Tis", "Ons", "Tor", "Fre", "Lör", "Sön"),
    expand = c(0, 0)
  ) +
  labs(title = "Folkhälsomyndighetens presskonferenser (färgade rutor) under 2020", 
       #subtitle = "Varje presskonferens markerad i grått"
       y = NULL, 
       x = NULL, 
       fill=NULL
  )


# Slå ihop övre och nedre del av figur 1.
plot_combined <- grid.arrange(plot_deaths, plot_pk, layout_matrix=rbind(1, 
                                                                        1,
                                                                        1,
                                                                        2,
                                                                        2)) 

ggsave(filename="plot_pk.pdf", plot_combined, width=190, height=120, units="mm")
ggsave(filename="plot_pk.png", plot_combined, width=190, height=120, units="mm")
