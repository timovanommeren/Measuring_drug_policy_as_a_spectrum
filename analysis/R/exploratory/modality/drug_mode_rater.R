library(tidyverse)
library(shiny)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Mark the local maxima of each party's answers per drug (1 = ban ... 9 = free sale), shown in random
# order; the per-drug camp maps in party_map_drug_modes.R are built from them. Parties as on the party
# map (party_map_scores.R).
# Keys: 1-9 = toggle a maximum, Enter = save and next (at least one maximum), u = back to the last saved.
# Every rating is written to the CSV at once; restarting resumes with the unrated histograms.
# Run: source this file in RStudio, or Rscript -e "shiny::runApp(shiny::shinyAppFile('analysis/R/drug_mode_rater.R'))"

csv_path <- Sys.getenv("DRUG_MODES_CSV",
                       here::here("analysis", "ratings", "drug_mode_ratings.csv"))
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)

hists <- answers |>
  filter(!is.na(vote), !vote %in% excluded) |>
  rename(party = vote) |>
  pivot_longer(all_of(names(drug_names)), names_to = "drug", values_to = "a") |>
  filter(!is.na(a)) |>
  summarise(p = list(as.numeric(tapply(w, factor(a, levels = 1:9), sum, default = 0)) / sum(w)), n = n(),
            .by = c(party, drug))

items <- select(hists, party, drug)

read_ratings <- function() {
  if (file.exists(csv_path)) {
    read_csv(csv_path, col_types = cols(.default = "c"))
  } else {
    tibble(party = character(), drug = character(), n = character(), maxima = character(), time = character())
  }
}

plot_item <- function(pr, dr, selected, blind) {
  h <- filter(hists, party == pr, drug == dr)
  dd <- tibble(option = 1:9, p = h$p[[1]], selected = option %in% selected)
  title <- if (blind) sprintf("n = %d", h$n) else sprintf("%s — %s (n = %d)", pr, drug_names[[dr]], h$n)
  ggplot(dd, aes(option, p, fill = selected)) +
    geom_col(width = 0.8) +
    # marked maxima without a bar still show
    geom_point(data = filter(dd, selected), aes(y = 0), shape = 17, size = 4, colour = "#0072B2") +
    geom_text(aes(label = if_else(p >= 0.01, scales::percent(p, 1), "")), vjust = -0.4, size = 4) +
    scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey70"), guide = "none") +
    scale_x_continuous(breaks = 1:9, expand = expansion(add = 0.3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, max(dd$p) + 0.06),
                       expand = expansion(mult = c(0.03, 0))) +
    labs(title = title, x = "Preferred policy (1 = ban … 9 = free sale)", y = "Respondents (weighted %)") +
    theme_minimal(base_size = 15) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
}

ui <- fluidPage(
  tags$script(HTML(
    "document.addEventListener('keydown', function(e) {
       if (['1','2','3','4','5','6','7','8','9','u','Enter'].includes(e.key)) {
         e.preventDefault();
         Shiny.setInputValue('key', e.key, {priority: 'event'});
       }
     });")),
  titlePanel("Local maxima per party and drug"),
  p(strong("1–9"), "= toggle a local maximum (several allowed),", strong("Enter"), "= save and next,",
    strong("u"), "= back to the last saved"),
  checkboxInput("blind", "Blind (hide party and drug)", value = FALSE),
  textOutput("progress"),
  plotOutput("plot", height = "480px"),
  textOutput("selected"),
  textOutput("last")
)

server <- function(input, output, session) {
  rated <- read_ratings()
  todo <- anti_join(items, rated, by = c("party", "drug"))
  queue <- reactiveVal(todo[sample(nrow(todo)), ])
  history <- reactiveVal(tibble(party = character(), drug = character(), maxima = character()))
  selected <- reactiveVal(integer())
  last_msg <- reactiveVal("")

  observeEvent(input$key, {
    k <- input$key
    if (k %in% as.character(1:9)) {
      s <- selected()
      v <- as.integer(k)
      selected(if (v %in% s) setdiff(s, v) else sort(c(s, v)))
    } else if (k == "Enter") {
      q <- queue()
      if (nrow(q) == 0 || length(selected()) == 0) return()
      cur <- q[1, ]
      maxima <- paste(selected(), collapse = ";")
      row <- tibble(party = cur$party, drug = cur$drug,
                    n = as.character(filter(hists, party == cur$party, drug == cur$drug)$n),
                    maxima = maxima, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      write_csv(row, csv_path, append = file.exists(csv_path))
      history(bind_rows(history(), tibble(party = cur$party, drug = cur$drug, maxima = maxima)))
      queue(q[-1, ])
      selected(integer())
      last_msg(sprintf("Saved: %s — %s = %s", cur$party, drug_names[[cur$drug]], maxima))
    } else if (k == "u") {
      h <- history()
      if (nrow(h) == 0) return()
      last <- h[nrow(h), ]
      r <- read_ratings()
      drop <- max(which(r$party == last$party & r$drug == last$drug))
      write_csv(r[-drop, ], csv_path)
      queue(bind_rows(select(last, party, drug), queue()))
      history(h[-nrow(h), ])
      selected(as.integer(str_split_1(last$maxima, ";")))
      last_msg(sprintf("Back to: %s — %s (was %s)", last$party, drug_names[[last$drug]], last$maxima))
    }
  })

  output$progress <- renderText(sprintf("%d of %d rated", nrow(items) - nrow(queue()), nrow(items)))

  output$plot <- renderPlot({
    q <- queue()
    if (nrow(q) == 0) {
      ggplot() + annotate("text", 0, 0, label = "All done", size = 10) + theme_void()
    } else {
      plot_item(q$party[1], q$drug[1], selected(), input$blind)
    }
  })

  output$selected <- renderText(sprintf("Selected maxima: %s",
                                        if (length(selected())) paste(selected(), collapse = ", ") else "none"))
  output$last <- renderText(last_msg())
}

shinyApp(ui, server)
