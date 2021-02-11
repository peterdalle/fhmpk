#' Returnera data frame med presskonferenser (alla datum + samplade datum)
#'
#' @return
#' @export
#'
#' @examples
press_conferences <- function() {
  df <- rio::import("../Data/Dagar.xlsx")
  df <- df %>% 
    mutate(veckodag = as.factor(veckodag),
           presskonferens = case_when(
           presskonferens == 1 ~ TRUE,
           TRUE                ~ FALSE))
  weekdays <- c("måndag", "tisdag", "onsdag", "torsdag", "fredag", 
                "lördag", "söndag")
  df$veckodag <- factor(df$veckodag,levels = weekdays, ordered=TRUE)
  return(df)
}


#' Returnera data frame med döda i covid-19 per dag från Soc
#'
#' @return
#' @export
#'
#' @examples
covid_deaths_socialstyrelsen <- function() {
  # Data från Socialstyrelsen, sammanställd av Adam Altmejd
  url <- "https://github.com/adamaltmejd/covid/blob/master/data/Socialstyrelsen_latest.csv?raw=true"
  covid <- read.csv(url)
  covid_deaths_per_day <- covid %>% 
    filter(!(is.na(date) | date == "")) %>% 
    mutate(date = as.Date(date)) %>% 
    #filter(date <= as.Date("2020-12-31")) %>% 
    group_by(date) %>% 
    mutate(n = sum(dead_n)) %>% 
    select(date, n)
  return(covid_deaths_per_day)
}


#' Returnera data frame med innehållsanalysen (från SPSS-fil)
#'
#' @return
#' @export
#'
#' @examples
content_analysis <- function() {
  filename <- "../Data/covid-210201_1.sav"
  content <- rio::import(filename)
  content <- rio::factorize(content)
  content$id <- seq.int(1, NROW(content))
  content$dag <- as.Date(content$Datum)
  return(content)
}


#' Gör ett över-tid-index över kritiska frågor för specifikt medium
#' 
#' Alla fem indikatorer är dikotoma (0/1).
#'
#' @param m data frame med innehållsanalys.
#' @param medium character med vilket medium som ska väljas ut.
#' @param group_average boolean om medelvärdet på alla 5 indikatorer ska tas
#' (annars får du alla 5 indikatorer separat).
#'
#' @return data frame med 5 indikatorer (separat eller medelvärde).
#' @export
#'
#' @examples
over_tid_index <- function(m, medium="FHM presskonferens", group_average=FALSE) {
  if (!(medium %in% levels(m$Medium))) {
    stop(paste0("'", medium, "' hittades inte. Medium måste vara en av följande: ", 
                paste0(levels(m$Medium), collapse=", "), "."), call. = FALSE)
  }
  
  # 1. konflikt
  m_konflikt <- m %>% 
    filter(Medium == medium) %>% 
    mutate(konflikt = fct_collapse(Fientlig, 
                                   Ja = c("Söker svar", "Inledning korrekt", "Kombination"),
                                   Nej = c("Nej", "Vet ej"))) %>% 
    select(dag, konflikt) %>% 
    group_by(dag) %>% 
    count(konflikt) %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(konflikt=="Ja") %>% 
    mutate(var = "1. Konflikt")

    # 2. ansvarsutkrävande
  m_ansvar <- m %>% 
    filter(Medium == medium) %>% 
    mutate(ansvar = fct_collapse(Ansvarsutkrävande, 
                                 Ja = c("Varför", "Hur", "Kombination"),
                                 Nej = c("Nej", "Oklart"))) %>% 
    select(dag, ansvar) %>% 
    group_by(dag) %>% 
    count(ansvar) %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(ansvar=="Ja") %>% 
    mutate(var = "2. Ansvar")
  
  # 3. ledande
  m_ledande <- m %>% 
    filter(Medium == medium) %>% 
    mutate(ledande = fct_collapse(JaNej,
                                  Ja = c("Ja"),
                                  Nej = c("Nej", "Oklart"))) %>% 
    select(dag, ledande) %>% 
    group_by(dag) %>% 
    count(ledande) %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(ledande=="Ja") %>% 
    mutate(var = "3. Ledande")
  
  # 4. initiativ
  m_initiativ <- m %>% 
    filter(Medium == medium) %>% 
    mutate(initiativ = fct_collapse(Uppföljningsfråga,
                                    Ja = c("Ja"),
                                    Nej = c("Nej", "N/A"))) %>% 
    select(dag, initiativ) %>% 
    group_by(dag) %>% 
    count(initiativ) %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(initiativ=="Ja") %>% 
    mutate(var = "4. Initiativ")
  
  # 5. direkthet
  m_direkt <- m %>% 
    filter(Medium == medium) %>% 
    mutate(direkt = fct_collapse(NegativForm,
                                 Ja = c("Ja"),
                                 Nej = c("Nej"))) %>% 
    select(dag, direkt) %>% 
    group_by(dag) %>% 
    count(direkt) %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(direkt=="Ja") %>% 
    mutate(var = "5. Direkt")
  
  # skapa long format av alla 5 indikatorer
  m_total <- rbind(m_konflikt, m_ansvar, m_ledande, m_initiativ, m_direkt)
  
  # skapa medelvärde för alla 5 indikatorer?
  if (group_average) {
    m_total <- m_total %>% 
      group_by(dag) %>% 
      summarize(percent = mean(percent),
                percent_median = median(percent)) %>% 
      mutate(var = "Group average")
  }
  
  # glöm inte att vi vill veta vilket medium detta kommer ifrån
  m_total$medium <- medium
  
  return(m_total)
}


#' Lägg till index kritiska frågor för hela data frame.
#'
#' @param m data frame med innehållsanalys.
#'
#' @return data frame med index för 5 indikatorer.
#' @export
#'
#' @examples
kritik_index <- function(m) {
  # Dikotomisera variabelnivåer.
  m <- m %>% 
    mutate(konflikt = fct_collapse(Fientlig, 
                      Ja = c("Söker svar", "Inledning korrekt", "Kombination"),
                      Nej = c("Nej", "Vet ej")),
           ansvar = fct_collapse(Ansvarsutkrävande, 
                      Ja = c("Varför", "Hur", "Kombination"),
                      Nej = c("Nej", "Oklart")),
           ledande = fct_collapse(JaNej,
                      Ja = c("Ja"),
                      Nej = c("Nej", "Oklart")),
           initiativ = fct_collapse(Uppföljningsfråga,
                      Ja = c("Ja"),
                      Nej = c("Nej", "N/A")),
           direkt = fct_collapse(NegativForm,
                      Ja = c("Ja"),
                      Nej = c("Nej")))
  
  # Använd 0/1 dikotomier.
  m <- m %>% mutate(konflikt  = if_else(konflikt  == "Ja", 1, 0),
                    ansvar    = if_else(ansvar    == "Ja", 1, 0),
                    ledande   = if_else(ledande   == "Ja", 1, 0),
                    initiativ = if_else(initiativ == "Ja", 1, 0),
                    direkt    = if_else(direkt    == "Ja", 1, 0))
  
  # Ta medelvärde radvis.
  m <- m %>% 
    rowwise() %>% 
    mutate(kritik_mean = mean(c(konflikt, ansvar, ledande, initiativ, direkt))) %>% 
    mutate(kritik_median = median(c(konflikt, ansvar, ledande, initiativ, direkt))) %>% 
    mutate(kritik_total = sum(c(konflikt, ansvar, ledande, initiativ, direkt))) %>% 
    ungroup()

  return(m)
}


#' Kritiska frågor (i procent) för respektive medium över tid.
#'
#' @param m data frame med innehållsanalys.
#'
#' @return data frame med genomsnitt kritiska frågor per dag.
#' @export
#'
#' @examples
kritiska_fragor_procent <- function(m) {
  # andel kritiska frågor, alla indikatorer sammanvägda
  m_total_avg_svt <- over_tid_index(m, medium="SVT intervju", group_average=TRUE)
  m_total_avg_ab  <- over_tid_index(m, medium="AB intervju", group_average=TRUE)
  m_total_avg_fhm <- over_tid_index(m, medium="FHM presskonferens", group_average=TRUE)
  
  # positioner för geom_text_repel
  m_total_avg_svt$x_pos <- as.Date("2020-11-15")
  m_total_avg_ab$x_pos  <- as.Date("2020-08-01")
  m_total_avg_fhm$x_pos <- as.Date("2020-12-01")
  m_total_avg_svt$y_pos <- .52
  m_total_avg_ab$y_pos  <- .74
  m_total_avg_fhm$y_pos <- .18
  
  m_total_avg <- rbind(m_total_avg_svt, m_total_avg_ab, m_total_avg_fhm)
  
  # y används som D.V. i graferna
  m_total_avg$y <- m_total_avg$percent
  
  return(m_total_avg)
}


#' Skapa figur med kritiska frågor över tid för respektive medium.
#'
#' @param df data frame med innehållsanalys.
#' @param title title på figuren.
#' @param subtitle undertiteln på figuren.
#'
#' @return ggplot-figur.
#' @export
#'
#' @examples
over_time_index_plot <- function(df, title=NULL, subtitle=NULL) {
  x <- df %>% 
    mutate(medium = case_when(medium == "SVT intervju" ~ "Intervju i SVT",
                              medium == "AB intervju" ~ "Intervju i Aftonbladet",
                              medium == "FHM presskonferens" ~ "Folkhälsomyndighetens\npresskonferens")) %>% 
    ggplot(aes(dag, y, group=medium, color=medium, linetype=medium)) +
    #geom_point(alpha=.3, size=2) +
    geom_smooth(formula=y~x, method="loess", se=FALSE, fullrange=FALSE, 
                level=0.95, span=0.6, alpha=.1, size=1.2) +
    geom_text_repel(aes(x=x_pos, y=y_pos, label=if_else(dag==min(dag), medium, NULL),
                        fontface="plain"), direction="both", force=.1, seed=42) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(limits = c(0, 1), breaks=seq.int(0, 1, .2), 
                       expand=c(0, 0), labels = scales::label_percent()) +
    theme_classic() +
    theme(axis.text = element_text(color="black"),
          panel.grid.major.y = element_line(color="#f5f5f5"),
          #panel.grid.minor.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          legend.position="none") +
    labs(title = title,
         subtitle = subtitle, 
         x = NULL,
         y = NULL,
         color = NULL,
         fill = NULL,
         linetype = NULL,
         shape = NULL)
  return(x)
}


#' Skapa figur med andel kritiska frågor, uppdelat per indikator.
#'
#' @param m data frame med innehållsanalys.
#'
#' @return ggplot-figur.
#' @export
#'
#' @examples
kritiska_fragor_per_indikator_plot <- function(m) {
  m_total_svt <- over_tid_index(m, medium="SVT intervju")
  m_total_ab  <- over_tid_index(m, medium="AB intervju")
  m_total_fhm <- over_tid_index(m, medium="FHM presskonferens")
  m_total <- rbind(m_total_svt, m_total_ab, m_total_fhm)
  graph <- m_total %>%
    ggplot(aes(dag, percent, color=var, shape=var, linetype=var)) +
    geom_point(alpha=.3, size=2) +
    geom_smooth(formula=y~x, method="loess", se=FALSE, fullrange=TRUE, 
                level=0.95, span=0.6, alpha=.2, size=1.3) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(limits = c(0, 1), breaks=seq.int(0, 1, .2), 
                       expand=c(0, 0), labels = scales::label_percent()) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="top") +
    labs(title = "Kritiska frågor under 2020",
         subtitle = "Andel turordningar som innehåller kritiska frågor (procent)", 
         x = NULL,
         y = NULL,
         color = NULL,
         fill = NULL,
         linetype = NULL,
         shape = NULL) +
    facet_wrap(~medium, nrow = 3)
  return(graph)
}


#' Skapa figur med andel (i procent) kritiska frågor per intervjuare.
#'
#' @param m data frame med innehållsanalys.
#'
#' @return ggplot-figur.
#' @export
#'
#' @examples
kritiska_fragor_intervjuare_plot <- function(m) {
  # Turordningar per intervjuare under presskonferens.
  topp_intervjuare <- m %>% 
    filter(Medium == "FHM presskonferens") %>% 
    mutate(kritik = if_else(kritik_mean > median(kritik_mean), 1, 0),
           Intervjuare = case_when(
             Intervjuare == "Vetenskapsradion" ~ "SR Vetenskapsradion",
             Intervjuare == "Frilans" ~ "Frilansjournalist",
             Intervjuare == "SvD" ~ "Svenska Dagbladet",
             Intervjuare == "SVT" ~ "Sveriges Television",
             Intervjuare == "DN" ~ "Dagens Nyheter",
             Intervjuare == "EXP" ~ "Expressen",
             Intervjuare == "AB" ~ "Aftonbladet",
             Intervjuare == "Nyhetsbyråer" ~ "Nyhetsbyrå",
             Intervjuare == "SR" ~ "Sveriges Radio",
             Intervjuare == "Ekot" ~ "Sveriges Radio Ekot",
             Intervjuare == "GP" ~ "Göteborgs-Posten",
             Intervjuare == "TV4 Nyheterna" ~ "TV4",
             Intervjuare == "TV4" ~ "TV4",
             Intervjuare == "Anger ej" ~ "Angav ej tillhörighet",
             TRUE ~ as.character(Intervjuare)
           )) %>% 
    group_by(Intervjuare, kritik) %>% 
    summarize(n = n()) %>% 
    mutate(intervjuare_n = sum(n),
           proportion = n / sum(n),
           prop_kritik = case_when(
             kritik == 1 ~ proportion,
             kritik == 0 ~ 1-proportion
           ))
  
  # Skapa figur relativ.
  plot_intervjuare <- topp_intervjuare %>% 
    mutate(kritik = case_when(
      kritik == 1 ~ "Kritisk fråga",
      kritik == 0 ~ "Ej kritisk fråga"),
      Intervjuare = paste0(Intervjuare, " (n = ", intervjuare_n, ")")) %>%
    ggplot(aes(reorder(Intervjuare, prop_kritik), proportion, fill=as.factor(kritik))) +
    geom_bar(stat="identity") +
    geom_hline(yintercept = c(.25, .50, .75), alpha=0.1) +
    scale_fill_manual(values=c('#559CD6', '#FF9697'),
                      breaks = c("Kritisk fråga", "Ej kritisk fråga")) +
    scale_y_continuous(labels = scales::percent, 
                       expand = c(0, 0, 0, .02)) +
    coord_flip() +
    labs(title="Andel kritiska frågor per intervjuare/turordning",
         x = NULL,
         y = NULL,
         fill = NULL) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text = element_text(color="black"),
          panel.grid.major.y = element_blank())
  return(plot_intervjuare)
}


#' Skapa figur med frågornas ämne per dag över tid.
#'
#' @param topics_per_day data frame med innehållsanalys.
#'
#' @return ggplot-figur.
#' @export
#'
#' @examples
topics_per_day_plot <- function(topics_per_day) {
  # Sätt X/Y position på labels. Vill få dem i slutet av varje linje
  # (geom_repel_text fungerar dåligt här, orkar inte felsöka det).
  topics_per_day$x_pos <- as.Date("2020-12-31")
  topics_per_day$y_pos <- .4
  topics_per_day$y_pos[topics_per_day$topic == "Hantera smittan"] <- .56
  topics_per_day$y_pos[topics_per_day$topic == "Drabbade\nsamhällssektorer"] <- .25
  topics_per_day$y_pos[topics_per_day$topic == "Politik"] <- .11
  topics_per_day$y_pos[topics_per_day$topic == "Själva smittan"] <- .07
  topics_per_day$y_pos[topics_per_day$topic == "Andra länder"] <- .02
  
  topics_per_day$label <- topics_per_day$topic
  topics_per_day$label[topics_per_day$dag < max(topics_per_day$dag)] <- ""
  
  # Linjen för drabbade grupper slutar lite tidigt så vi får försöka placera den manuellt.
  topics_per_day$label[topics_per_day$topic == "Drabbade grupper" & topics_per_day$dag == as.Date("2020-08-27")] <- "Drabbade\ngrupper"
  topics_per_day$y_pos[topics_per_day$topic == "Drabbade grupper" & topics_per_day$dag == as.Date("2020-08-27")] <- .22
  topics_per_day$x_pos[topics_per_day$topic == "Drabbade grupper" & topics_per_day$dag == as.Date("2020-08-27")] <- as.Date("2020-11-07")
  
  figure <- topics_per_day %>% 
    filter(topic != "Övrigt") %>% 
    filter(topic != "Organisationer") %>% 
    mutate(topic = as.factor(topic)) %>% 
    ggplot(aes(dag, percent/100, group=topic, color=topic, linetype=topic)) +
    #geom_point(alpha=.2) +
    geom_smooth(size=1.3, se=FALSE, formula=y~x, method="loess",
                span=.7, fullrange=TRUE) +
    geom_text(aes(x=x_pos, y=y_pos, label=label, fontface="plain"), hjust=-.1) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       limits=c(0, .7), breaks = seq.int(0, 1, by=.1),
                       expand=c(0, 0, 0, .01)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 limits = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
                 expand = c(0, 0, 0, 0)) +
    labs(title="Vad journalisternas frågor handlar om under presskonferensen 2020",
         x = NULL, 
         y = NULL,
         color = "Ämne",
         linetype = "Ämne") +
    theme_classic() +
    theme(axis.text = element_text(color="black"),
          panel.grid.major.y = element_line(color="#f5f5f5"),
          #panel.grid.minor.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          legend.position="none",
          plot.margin = unit(c(1, 7, 1, 1), "lines")) 
  
  # Detta inkl hjust=-.1 + plot.margin ovanm gör att man kan plotta utanför diagrammet.
  gt <- ggplotGrob(figure)
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  #grid::grid.draw(gt)
  return(gt)
}
