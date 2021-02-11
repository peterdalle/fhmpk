library(tidyverse)
library(furniture)  # remotes::install_github("tysonstanley/furniture")
library(scales)
library(ggrepel)
library(surveyutils) # devtools::install_github("peterdalle/surveyutils")
library(gridExtra)
library(tidytext)

source("functions.r", encoding="UTF-8")

# Hämta innehållsanalysen och lägg på kritiska frågor (omkodade).
m <- content_analysis()
m <- kritik_index(m)

# Tidigaste/senaste samplingdatum för respektive medium m.m.
m %>% 
  group_by(Medium) %>% 
  summarize(forsta_dag = min(dag),
            senaste_dag = max(dag),
            kritik_mean = mean(kritik_mean, na.rm=TRUE),
            n = n())

# Frekvens på varje variabel för lite känsla över materialet.
furniture::tableF(m, Datum)
furniture::tableF(m, dag)
furniture::tableF(m, Medium)
furniture::tableF(m, Tidskod)
furniture::tableF(m, IntervjuPlats)
furniture::tableF(m, Intervjuare)
furniture::tableF(m, Respondent)
furniture::tableF(m, Turordning)
furniture::tableF(m, Frågenummer)
furniture::tableF(m, Uppföljningsfråga)
furniture::tableF(m, Komplexitet)
furniture::tableF(m, Ämne, n = Inf)
furniture::tableF(m, Självreferens)
furniture::tableF(m, Rättframhet)
furniture::tableF(m, Rådsökande)
furniture::tableF(m, Spekulerande)
furniture::tableF(m, Upprepning)
furniture::tableF(m, Personlig)
furniture::tableF(m, TredjePart)
furniture::tableF(m, NegativForm)
furniture::tableF(m, Men)
furniture::tableF(m, JaNej)
furniture::tableF(m, Ansvarsutkrävande)
furniture::tableF(m, Fientlig)
furniture::tableF(m, Ton)
furniture::tableF(m, PratarIMun)


# Tabell 1.
furniture::table1(m,
    "Respondent" = fct_collapse(Respondent, 
                   Tegnell = c("FHM: Tegnell"),
                   Tegmark = c("FHM: Tegmark Wisell"),
                   Carlsson = c("FHM: Carlsson"),
                   Giesecke = c("FHM: Giesecke"),
                   FHM = c("FHM: Annan"),
                   SocAlexandersson = c("Socialstyrelsen: Alexandersson"),
                   SocSandwall = c("Socialstyrelsen: Sandwall"),
                   Soc = c("Socialstyrelsen: Annan"),
                   MSB = c("MSB"),
                   Ovriga = c("KI", "Övrig", "Oklart", 
                              "Läkemedelsverket")),
    Rådsökande, 
    "Spekulerande" = Spekulerande,
    "Tredje part" = TredjePart,
    "Intervjuplats" = IntervjuPlats,
    "Personlig" = fct_collapse(Personlig,
                               Ja = c("Ja"),
                               Nej = c("Nej", "Oklart")),
    "Komplexitet" = fct_collapse(Komplexitet, 
                     Hog = c("Hög", "Blandad"),
                     Lag = c("Låg", "Ej relevant", "N/A")),
    "Konflikt" = fct_collapse(Fientlig, 
                     Ja = c("Söker svar", "Inledning korrekt", "Kombination"),
                     Nej = c("Nej", "Vet ej")),
    "Ansvarsutkrävande frågor" = fct_collapse(Ansvarsutkrävande, 
                     Ja = c("Varför", "Hur", "Kombination"),
                     Nej = c("Nej", "Oklart")),
    "Ledande frågor" = fct_collapse(JaNej,
                     Ja = c("Ja"),
                     Nej = c("Nej", "Oklart")),
    "Initiativ" = fct_collapse(Uppföljningsfråga,
                     Ja = c("Ja"),
                     Nej = c("Nej", "N/A")),
    "Direkt" = fct_collapse(NegativForm,
                     Ja = c("Ja"),
                     Nej = c("Nej")),
    "Ton" = fct_collapse(Ton,
                     Positiv = c("Positiv"),
                     Neutral = c("Neutral", "N/A"),
                     Negativ = c("Negativ")),
    splitby = ~Medium,
    total = TRUE,
    test=TRUE,
    digits=0,
    rounding_perc=0,
    output = "pandoc")

# Plocka ut några exmepel på kritiska frågor.
m %>% 
  filter(Ansvarsutkrävande == "Varför" & 
           #Rättframhet == "Förmåga" & 
           #Fientlig == "Söker svar" & 
           Men == "Ja" &
           NegativForm == "Ja" 
           #Uppföljningsfråga == "Nej"
         ) %>% 
  pull(dag, Transkribering) %>% 
  head(10)

# Kritiska frågor i procent.
kritik_percent <- kritiska_fragor_procent(m)

# Genomsnitt andel kritiska frågor (i procent) för respektive medium.
kritik_percent %>% 
  group_by(medium) %>% 
  summarize(mean_kritik = mean(y))


# Figur 2. Andel kritiska frågor över tid.
plot_kritik <- over_time_index_plot(kritik_percent, 
                                    title="Kritiska frågor under 2020")
plot_kritik
ggsave("plot_kritik.pdf", plot_kritik, width=190, height=100, units="mm")
ggsave("plot_kritik.png", plot_kritik, width=190, height=100, units="mm")


# Figur 3. Andel kritiska frågor per intervjuare.
plot_intervjuare <- kritiska_fragor_intervjuare_plot(m)
plot_intervjuare
ggsave("plot_intervjuare.pdf", plot_intervjuare, width=170, height=140, units="mm")
ggsave("plot_intervjuare.png", plot_intervjuare, width=190, height=140, units="mm")


# Plocka ut andelen topp-ämnen per dag och turordningar/frågor.
topics_per_day <- m %>% 
  mutate(topic = case_when(
    Ämne == "Covid-19" ~ "Själva smittan",
    Ämne == "Smittspridning" ~ "Själva smittan",
    Ämne == "Statistik, antal fall/döda" ~ "Själva smittan",
    
    Ämne == "Grupp: Patienter" ~ "Drabbade grupper",
    Ämne == "Grupp: Äldre" ~ "Drabbade grupper",
    Ämne == "Grupp: Yngre" ~ "Drabbade grupper",
    Ämne == "Grupp: Barn" ~ "Drabbade grupper",
    Ämne == "Grupp: Män" ~ "Drabbade grupper",
    Ämne == "Grupp: Kvinnor" ~ "Drabbade grupper",
    Ämne == "Grupp: Invandrare" ~ "Drabbade grupper",
    Ämne == "Grupp: Körer" ~ "Drabbade grupper",
    Ämne == "Grupp: Vårdpersonal" ~ "Drabbade grupper",
    
    Ämne == "Respons: Testning" ~ "Hantera smittan",
    Ämne == "Respons: Tvätta händerna" ~ "Hantera smittan",
    Ämne == "Respons: Socialt avstånd" ~ "Hantera smittan",
    Ämne == "Respons: Immunitet" ~ "Hantera smittan",
    Ämne == "Respons: Munskydd" ~ "Hantera smittan",
    Ämne == "Respons: Vaccination" ~ "Hantera smittan",
    Ämne == "Respons: Smittspårning" ~ "Hantera smittan",
    Ämne == "Respons: Övrigt" ~ "Hantera smittan",
    Ämne == "Politik: Tvångsåtgärder/restriktioner" ~ "Hantera smittan",
    
    Ämne == "Plats: Folksamlingar" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Resor, kollektivtrafik" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Äldreboenden" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Sjukhus/vård" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Arbetsplats/hemarbete" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Skolan" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Restaurang/handel" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Idrottsrelaterat" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Sthlm" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Göteborg / Västra Götaland" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Skåne" ~ "Drabbade\nsamhällssektorer",
    Ämne == "Plats: Övrig kommun/region" ~ "Drabbade\nsamhällssektorer",
    
    Ämne == "Plats: Nordiska länder" ~ "Andra länder",
    Ämne == "Plats: Övriga världen" ~ "Andra länder",
    
    Ämne == "Org: WHO" ~ "Organisationer",
    Ämne == "Org: MSB" ~ "Organisationer",
    Ämne == "Org: FHM" ~ "Organisationer",
    Ämne == "Org: SKR" ~ "Organisationer",
    Ämne == "Org: Socialstyrelsen" ~ "Organisationer",
    Ämne == "Org: Läkemedelsverket" ~ "Organisationer",
    Ämne == "Org: Apoteken" ~ "Organisationer",
    Ämne == "Org: Regeringen/staten" ~ "Organisationer",
    
    Ämne == "Politik: Jobb m.m." ~ "Politik",
    Ämne == "Politik: Politiker/partier" ~ "Politik",
    Ämne == "Politik: Juridik/lagar" ~ "Politik",
    Ämne == "Politik: Jämlikhet m.m." ~ "Politik",
    
    Ämne == "Medierapportering" ~ "Övrigt",
    Ämne == "\"Sveriges strategi\"" ~ "Övrigt",
    Ämne == "Övrig" ~ "Övrigt",
    TRUE ~ as.character(Ämne))) %>% 
  filter(Medium == "FHM presskonferens") %>% 
  group_by(dag, topic) %>% 
  summarize(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100)) 
  #arrange(desc(percent))


# Figur 3. Vad journalisternas frågor handlar om.
topics_plot <- topics_per_day_plot(topics_per_day)
plot(topics_plot)
ggsave("plot_topic.png", plot=topics_plot, width=250, height=120, scale=0.9, units="mm")
ggsave("plot_topic.pdf", plot=topics_plot, width=250, height=120, scale=0.9, units="mm")


# Exempel på fråga om politik aug/sep 2020.
m %>% 
  filter(Medium == "FHM presskonferens",
         dag > as.Date("2020-08-01"),
         dag < as.Date("2020-10-31"),
         grepl("politik", Ämne, ignore.case=TRUE)) %>% 
  select(id, Ämne, Transkribering)

# Plocka ut intressant citat.
m %>% 
  filter(id == 602) %>% 
  select(Transkribering, Intervjuare, dag, Respondent) %>% 
  t()


# Kolla de vanligaste orden i frågorna. Förutom "Stockholm", "smittspridning",
# "Sverige" och småord ligger "munskydd" högt upp.
tokens_sorted <- m %>% 
  unnest_tokens(word, Transkribering, token="ngrams", n=1) %>% 
  count(word, sort=TRUE)
tokens_sorted

# Hur stor andel av frågorna nämner munskydd (3,13%).
m %>% 
  mutate(munskydd = grepl("munskydd", Transkribering, ignore.case=TRUE)) %>% 
  group_by(munskydd) %>% 
  summarize(n = n()) %>% 
  mutate(percent = (n / sum(n)) * 100)
