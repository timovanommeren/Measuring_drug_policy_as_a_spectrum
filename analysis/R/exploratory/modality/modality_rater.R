library(tidyverse)
library(shiny)

source(here::here("analysis", "R", "00_load_and_clean.R"))

# Rate the modality of each party x drug distribution (9 answer options), shown in random order.
# Keys: 1 = unimodal, 2 = bimodal, 3 = trimodal, u = undo last rating.
# Every rating is appended to the CSV at once; restarting resumes with the unrated pairs.
# Run: source this file in RStudio, or Rscript -e "shiny::runApp(shiny::shinyAppFile('analysis/R/modality_rater.R'))"

csv_path <- Sys.getenv("MODALITY_CSV",
                       here::here("analysis", "ratings", "modality_ratings.csv"))

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

# parties as in raw_distributions.R: small parties (n < 30, except Volt) and "Anders" together,
# non-voters and blank together
party_n <- count(df, vote_recall_rec)
small <- setdiff(as.character(party_n$vote_recall_rec[party_n$n < 30]), "Volt")
no_vote <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen")

dist <- df |>
  mutate(party = as.character(vote_recall_rec) |>
           case_match(no_vote ~ "Did not vote / blank",
                      c(small, "Anders") ~ "Other party",
                      .default = as.character(vote_recall_rec))) |>
  filter(!is.na(party)) |>
  select(party, w, all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
  pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "x") |>
  filter(!is.na(x)) |>
  mutate(x = as.integer(x)) |>
  summarise(w = sum(w), n = n(), .by = c(party, drug, x)) |>
  complete(nesting(party, drug), x = 1:9, fill = list(w = 0, n = 0)) |>
  mutate(p = w / sum(w), n_drug = sum(n), .by = c(party, drug))

pairs <- distinct(dist, party, drug)
labels <- c("1" = "unimodal", "2" = "bimodal", "3" = "trimodal")

read_ratings <- function() {
  if (file.exists(csv_path)) {
    read_csv(csv_path, col_types = cols(.default = "c"))
  } else {
    tibble(party = character(), drug = character(), n = character(), modality = character(), time = character())
  }
}

plot_pair <- function(pr, dr, blind) {
  dd <- filter(dist, party == pr, drug == dr)
  title <- if (blind) sprintf("n = %d", dd$n_drug[1]) else
    sprintf("%s — %s (n = %d)", pr, drug_labels[[dr]], dd$n_drug[1])
  ggplot(dd, aes(x, p, fill = factor(x))) +
    geom_col(width = 0.8) +
    geom_text(aes(label = if_else(p >= 0.02, scales::percent(p, 1), "")), vjust = -0.4, size = 4) +
    scale_fill_viridis_d(option = "viridis", end = 0.9, guide = "none") +
    scale_x_continuous(breaks = 1:9, expand = expansion(add = 0.3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, max(dd$p) + 0.06), expand = expansion(0)) +
    labs(title = title, x = "Preferred policy (1 = ban … 9 = free sale)", y = "Respondents (weighted %)") +
    theme_minimal(base_size = 15) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
}

ui <- fluidPage(
  tags$script(HTML(
    "document.addEventListener('keydown', function(e) {
       if (['1', '2', '3', 'u'].includes(e.key)) Shiny.setInputValue('key', e.key, {priority: 'event'});
     });")),
  titlePanel("Modality of party × drug distributions"),
  p(strong("1"), "= unimodal,", strong("2"), "= bimodal,", strong("3"), "= trimodal,", strong("u"), "= undo"),
  checkboxInput("blind", "Blind (hide party and drug)", value = FALSE),
  textOutput("progress"),
  plotOutput("plot", height = "480px"),
  textOutput("last")
)

server <- function(input, output, session) {
  rated <- read_ratings()
  todo <- anti_join(pairs, rated, by = c("party", "drug"))
  queue <- reactiveVal(todo[sample(nrow(todo)), ])
  history <- reactiveVal(tibble(party = character(), drug = character()))
  last_msg <- reactiveVal("")

  observeEvent(input$key, {
    if (input$key == "u") {
      h <- history()
      if (nrow(h) == 0) return()
      last <- h[nrow(h), ]
      r <- read_ratings()
      drop <- max(which(r$party == last$party & r$drug == last$drug))
      write_csv(r[-drop, ], csv_path)
      queue(bind_rows(last, queue()))
      history(h[-nrow(h), ])
      last_msg(sprintf("Undone: %s — %s", last$party, drug_labels[[last$drug]]))
    } else {
      q <- queue()
      if (nrow(q) == 0) return()
      cur <- q[1, ]
      row <- tibble(party = cur$party, drug = cur$drug,
                    n = as.character(filter(dist, party == cur$party, drug == cur$drug)$n_drug[1]),
                    modality = labels[[input$key]], time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      write_csv(row, csv_path, append = file.exists(csv_path))
      history(bind_rows(history(), select(cur, party, drug)))
      queue(q[-1, ])
      last_msg(sprintf("Saved: %s — %s = %s", cur$party, drug_labels[[cur$drug]], labels[[input$key]]))
    }
  })

  output$progress <- renderText({
    sprintf("%d of %d rated", nrow(pairs) - nrow(queue()), nrow(pairs))
  })

  output$plot <- renderPlot({
    q <- queue()
    if (nrow(q) == 0) {
      ggplot() + annotate("text", 0, 0, label = "All done", size = 10) + theme_void()
    } else {
      plot_pair(q$party[1], q$drug[1], input$blind)
    }
  })

  output$last <- renderText(last_msg())
}

shinyApp(ui, server)
