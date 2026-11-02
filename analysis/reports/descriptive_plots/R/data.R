# =============================================================================
# data.R
# -----------------------------------------------------------------------------
# The data layer of descriptive_plots.Rmd: reads the Kieskompas delivery of the
# main study (22 October 2026, analysis/data/) and the questionnaire
# (analysis/assets/main_study_10_05_fixed.qsf), and builds `pilot` (one row per
# completed interview) plus the item lists, scales and labels the sections use.
#
# The sections were written for the pilot reports, hence the object name
# `pilot` and the pilots' variable names (beleid_<substance>, emo_<topic>_*).
#
# Code 99 is "geen mening" on the 1-7 matrices and the policy rules and is set
# to NA; on the 0-100 involvement sliders 99 is a real position.
# =============================================================================

library(here)
source(here("analysis", "reports", "descriptive_plots", "R", "plot_helpers.R"))

DELIVERY <- list(
  completed   = here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.csv"),
  in_progress = here("analysis", "data", "NLOZ26-10_Drugsbeleid_in progress_gedeeld_261022.csv"),
  sav         = here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.sav"),   # value labels
  label       = "full main study, delivered 22 October 2026")
QSF_PATH <- here("analysis", "assets", "main_study_10_05_fixed.qsf")

SCALE_MIN <- 1
SCALE_MAX <- 7


# ---- Constants ----------------------------------------------------------------
# export column -> substance key
POLICY_RAW <- c(beleid_voorkeur_alcohol = "alcohol", beleid_voorkeur_tabak_en_vapes = "nicotine",
                beleid_voorkeur_cannabis = "cannabis", beleid_voorkeur_psychedelica = "psychedelica",
                beleid_voorkeur_mdma = "mdma", beleid_voorkeur_cocaine = "cocaine",
                beleid_voorkeur_speed = "speed", beleid_voorkeur_heroine = "heroine",
                beleid_voorkeur_pijnstillers = "pijnstillers")
SUBSTANCE_LABELS <- c(alcohol = "Alcohol", nicotine = "Nicotine", cannabis = "Cannabis",
                      psychedelica = "Psychedelica", mdma = "MDMA", cocaine = "Cocaine / crack",
                      speed = "Speed", heroine = "Heroïne",
                      pijnstillers = "Zware pijnstillers / slaappillen")
LEGAL_SUBSTANCES   <- c("alcohol", "nicotine")
MDMA_SUPPORT_MIN   <- 40   # the "MDMA-regulering" split, on the 0-100 slider

# Beliefs and behaviour. Anti items (agreeing favours prohibition) get a
# reverse-scored copy, <item>_scored, so that high always means liberal.
BELIEF_META <- data.frame(
  variable = c(paste0("beliefs_society_", 1:5), paste0("beliefs_addiction_", 1:5),
               paste0("beliefs_regulate_", 1:4), paste0("beliefs_crime_", 1:4)),
  wording = c(
    "Mensen mogen zelf beslissen of ze schadelijke middelen gebruiken.",
    "Drugsgebruik is slecht voor de normen en waarden van onze samenleving.",
    "Nederland moet zich houden aan het internationale verbod op drugs.",
    "Het is verkeerd om geld te verdienen aan middelen waar mensen verslaafd aan kunnen raken.",
    "We streven naar een samenleving waarin niemand verslavende middelen gebruikt.",
    "Als een middel legaal te koop is, raken meer mensen eraan verslaafd.",
    "Verslaafden zoeken eerder hulp als drugs niet verboden zijn.",
    "Mensen die verslaafd zijn hebben dat aan zichzelf te danken.",
    "Het gebruik van softdrugs leidt vaak tot het gebruik van harddrugs.",
    "Verboden harddrugs zijn ongezonder dan alcohol en tabak.",
    "Als een middel verboden is, gebruiken minder mensen het.",
    "Het reguleren van de verkoop van drugs levert Nederland veel geld op.",
    "Drugsgebruik is minder gevaarlijk als de verkoop wordt toegestaan.",
    "Een verbod werkt goed om jongeren te beschermen tegen de gevaren van verslavende middelen.",
    "Er zou in Nederland minder criminaliteit zijn als de verkoop van drugs wordt toegestaan.",
    "We moeten meer politie hebben om problemen met drugs aan te pakken.",
    "Drugs hebben voor eigen gebruik moet niet strafbaar zijn.",
    "Drugsgebruikers zijn medeverantwoordelijk voor drugscriminaliteit."),
  direction = c("Pro", "Anti", "Anti", "Anti", "Anti", "Anti", "Pro", "Anti", "Anti", "Anti",
                "Anti", "Pro", "Pro", "Anti", "Pro", "Anti", "Pro", "Anti"))
BELIEF_META$raw      <- BELIEF_META$variable
BELIEF_META$subscale <- sub("^beliefs_([a-z]+)_[0-9]+$", "\\1", BELIEF_META$variable)
GEDRAG_META <- data.frame(
  raw = paste0("gedrag_", 1:3), variable = paste0("gedrag_beleid_", 1:3),
  wording = c("Ik zou drugsdealers altijd melden bij de politie.", "Ik vermijd drugsgebruikers.",
              "Als iemand een drug heeft gebruikt die ik niet ken, zou ik ernaar vragen."),
  direction = c("Anti", "Anti", "Pro"))

# Own use: export 1-4 shifted to 0-3 (nooit, niet laatste jaar, wel eens, vaak).
USE_RAW <- c(gebruik_1 = "gebruik_alcohol", gebruik_2 = "gebruik_tabak",
             gebruik_3 = "gebruik_soft", gebruik_4 = "gebruik_hard")
USE_NEVER <- 0L
USE_OFTEN <- 3L
# the use group of each policy slider (painkillers have none)
USE_OF_SUBSTANCE <- c(alcohol = "gebruik_alcohol", nicotine = "gebruik_tabak",
                      cannabis = "gebruik_soft", psychedelica = "gebruik_hard", mdma = "gebruik_hard",
                      cocaine = "gebruik_hard", speed = "gebruik_hard", heroine = "gebruik_hard")

# Emotions: gevoelens_1_<topic>_<answer> in the order of QSF question Q28.
EMOTION_TOPICS <- c("emo_alc", "emo_tab", "emo_soft", "emo_hard", "emo_wod", "emo_leg")
EMOTION_TOPIC_LABELS <- setNames(c("alcohol", "tabak", "softdrugs", "harddrugs", "war on drugs",
                                   "legalisering van drugs"), EMOTION_TOPICS)
EMOTION_ANSWERS <- c("boos", "bang_bezorgd", "blij", "geen_van_deze")
EMOTION_NONE    <- "geen_van_deze"
EMOTION_TYPES   <- setdiff(EMOTION_ANSWERS, EMOTION_NONE)
EMOTION_LABELS  <- c(boos = "boos", bang_bezorgd = "bang / bezorgd", blij = "blij",
                     geen_van_deze = "geen van deze")
EMOTION_GRID <- expand.grid(answer = seq_along(EMOTION_ANSWERS), topic = seq_along(EMOTION_TOPICS))
EMOTION_GRID$raw      <- sprintf("gevoelens_1_%d_%d", EMOTION_GRID$topic, EMOTION_GRID$answer)
EMOTION_GRID$variable <- paste0(EMOTION_TOPICS[EMOTION_GRID$topic], "_", EMOTION_ANSWERS[EMOTION_GRID$answer])

# Involvement: five 0-100 sliders. 1b and 2b are not in the QSF.
INVOLVEMENT_WORDING <- c(
  betrokkenheid_1  = "In mijn omgeving heb ik te maken met drugs of met criminaliteit door drugs.",
  betrokkenheid_1b = "betrokkenheid_1b",
  betrokkenheid_2  = "Over drugs of over drugscriminaliteit weet ik meer dan de meeste mensen.",
  betrokkenheid_2b = "betrokkenheid_2b",
  betrokkenheid_3  = "Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee bij mijn stemkeuze.")
INVOLVEMENT_RAW <- names(INVOLVEMENT_WORDING)

# demographics: the agency's recodes, labelled from the .sav
DEMOGRAPHICS <- c(age_7_rec = "leeftijd", sex_rec = "geslacht", edu_3_rec = "opleiding",
                  urb = "stedelijkheid", vote_recall_rec = "stemherinnering")


# ---- Helpers --------------------------------------------------------------------
# Questionnaire text is rich text: strip tags, decode entities, squash spaces.
qsf_text <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x)) return(NA_character_)
  x <- gsub("<[^>]*>", " ", x)
  named <- c("&nbsp;" = " ", "&amp;" = "&", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"",
             "&#39;" = "'", "&apos;" = "'", "&euro;" = "€", "&eacute;" = "é", "&euml;" = "ë",
             "&iuml;" = "ï", "&ouml;" = "ö", "&uuml;" = "ü", "&hellip;" = "…", "&ndash;" = "–",
             "&mdash;" = "—", "&rsquo;" = "’", "&lsquo;" = "‘", "&ldquo;" = "“", "&rdquo;" = "”")
  for (entity in names(named)) x <- gsub(entity, named[[entity]], x, fixed = TRUE)
  for (entity in unique(regmatches(x, gregexpr("&#[0-9]+;", x))[[1]]))
    x <- gsub(entity, intToUtf8(as.integer(gsub("[^0-9]", "", entity))), x, fixed = TRUE)
  trimws(gsub(" +([,.;:!?])", "\\1", gsub("[[:space:]]+", " ", x)))
}

# every question in the QSF, keyed by export tag (entries without choices dropped)
qsf_questions <- function(path = QSF_PATH) {
  qs <- Filter(function(e) identical(e$Element, "SQ") && !is.null(e$Payload$DataExportTag) &&
                 (length(e$Payload$Choices) > 0 || length(e$Payload$Answers) > 0),
               jsonlite::fromJSON(path, simplifyVector = FALSE)$SurveyElements)
  setNames(lapply(qs, `[[`, "Payload"), vapply(qs, function(e) e$Payload$DataExportTag, ""))
}

# code -> label of one question: a matrix's Answers, a multiple choice's Choices
qsf_scale <- function(tag, questions = qsf_questions()) {
  p <- questions[[tag]]
  if (is.null(p)) stop("no question tagged '", tag, "' in the .qsf", call. = FALSE)
  options <- switch(p$QuestionType, Matrix = p$Answers, MC = p$Choices, NULL)
  if (!length(options)) return(data.frame(code = character(0), label = character(0)))
  data.frame(code = names(options), label = vapply(options, function(o) qsf_text(o$Display), ""),
             row.names = NULL)
}

# the sub-items of a matrix or slider, named by export column; the export numbers
# columns by position in ChoiceOrder, not by choice key
qsf_items <- function(tag, questions = qsf_questions()) {
  p <- questions[[tag]]
  if (is.null(p)) stop("no question tagged '", tag, "' in the .qsf", call. = FALSE)
  order <- if (length(p$ChoiceOrder)) as.character(unlist(p$ChoiceOrder)) else names(p$Choices)
  setNames(vapply(order, function(k) qsf_text(p$Choices[[k]]$Display), ""),
           paste0(tag, "_", seq_along(order)))
}

sav_value_labels <- function(variables)
  lapply(setNames(variables, variables), function(v) attr(haven::read_sav(DELIVERY$sav, n_max = 1)[[v]], "labels"))

# codes -> factor labelled from the .sav; a code without a label stops the build
label_codes <- function(x, labels) {
  present <- sort(unique(x[!is.na(x)]))
  if (length(missing <- setdiff(present, unname(labels))))
    stop("no label in the .sav for code(s) ", paste(missing, collapse = ", "), call. = FALSE)
  factor(x, levels = present, labels = names(labels)[match(present, unname(labels))])
}

# the emotion binaries as an item block, topic-major, without "geen van deze"
emotion_item_columns <- function(topics = EMOTION_TOPICS, emotions = EMOTION_TYPES)
  as.vector(t(outer(topics, emotions, paste, sep = "_")))
emotion_item_labels <- function(emotion_cols, wording, topics = EMOTION_TOPICS,
                                emotions = EMOTION_TYPES, sep = " - ") {
  stopifnot(length(emotion_cols) == length(topics))
  topic_text <- sub("^.*? - ", "", gsub("[\r\n]+", " ", unname(wording[emotion_cols])))
  as.vector(t(outer(topic_text, unname(EMOTION_LABELS[emotions]), paste, sep = sep)))
}
# the largest phi two binary items with base rates p and q can reach
phi_max <- function(p, q) {
  lo <- pmin(p, q); hi <- pmax(p, q)
  sqrt((lo * (1 - hi)) / (hi * (1 - lo)))
}


# ---- Read the delivery ------------------------------------------------------------
read_delivery <- function(f) read.csv(f, na.strings = c("", " ", "NA"), fileEncoding = "UTF-8-BOM",
                                      check.names = FALSE)
raw  <- read_delivery(DELIVERY$completed)
open <- read_delivery(DELIVERY$in_progress)
stopifnot(!any(open$InterviewId %in% raw$InterviewId))
open[setdiff(names(raw), names(open))] <- NA
raw <- rbind(cbind(raw, InterviewState = "Completed"), cbind(open[names(raw)], InterviewState = "InProgress"))

no_answer <- function(x) replace(x, x == 99, NA)
all_rows <- data.frame(ResponseId = raw$InterviewId, state = raw$InterviewState,
                       complete = raw$InterviewState == "Completed",
                       duration_sec = raw$InterviewDuration, duration_min = raw$InterviewDuration / 60)
for (v in names(POLICY_RAW)) all_rows[[paste0("beleid_", POLICY_RAW[[v]])]] <- no_answer(raw[[v]])
likert <- rbind(BELIEF_META[c("raw", "variable", "direction")], GEDRAG_META[c("raw", "variable", "direction")])
for (i in seq_len(nrow(likert))) all_rows[[likert$variable[i]]] <- no_answer(raw[[likert$raw[i]]])
for (i in seq_len(nrow(likert))) {
  x <- all_rows[[likert$variable[i]]]
  all_rows[[paste0(likert$variable[i], "_scored")]] <- if (likert$direction[i] == "Anti") (SCALE_MIN + SCALE_MAX) - x else x
}
for (v in names(USE_RAW)) all_rows[[USE_RAW[[v]]]] <- raw[[v]] - 1
for (i in seq_len(nrow(EMOTION_GRID))) all_rows[[EMOTION_GRID$variable[i]]] <- raw[[EMOTION_GRID$raw[i]]]
all_rows[INVOLVEMENT_RAW] <- raw[INVOLVEMENT_RAW]
all_rows$betrokkenheid_mean <- rowMeans(all_rows[INVOLVEMENT_RAW])

# substance ladder: the heaviest group a respondent has ever used
ever <- function(v) !is.na(all_rows[[v]]) & all_rows[[v]] >= 1
ladder <- ifelse(ever("gebruik_hard"), "hard drugs", ifelse(ever("gebruik_soft"), "cannabis",
          ifelse(ever("gebruik_alcohol") | ever("gebruik_tabak"), "legaal", "nooit")))
ladder[rowSums(!is.na(all_rows[unname(USE_RAW)])) == 0] <- NA
all_rows$substance_ladder <- factor(ladder, levels = c("nooit", "legaal", "cannabis", "hard drugs"))

# the sample: completed interviews; the in-progress ones are reported as dropped
status_scale <- data.frame(code = c("Completed", "InProgress"),
                           label = c("Completed", "InProgress (niet afgerond)"))
data_values <- data.frame(Status = all_rows$state)
blank_is_na <- function(x) { if (is.character(x)) x[x == ""] <- NA; is.na(x) }
n_genuine <- nrow(all_rows)


# ---- Emotions: the multi-select cell, rebuilt from the 0/1 boxes ---------------
questions <- qsf_questions(QSF_PATH)
emotion_scale <- qsf_scale("Q28", questions)
stopifnot(identical(gsub("[^a-z]+", "_", tolower(emotion_scale$label)), EMOTION_ANSWERS))
for (topic in EMOTION_TOPICS) {
  ticks <- as.matrix(all_rows[paste0(topic, "_", EMOTION_ANSWERS)])
  all_rows[[topic]] <- apply(ticks, 1, function(r)
    if (all(is.na(r))) NA_character_ else paste(emotion_scale$code[which(r == 1)], collapse = ","))
}
emotion_cols <- EMOTION_TOPICS


# ---- Policy: rule k at its position on the QSF's 0-100 slider -------------------
POLICY_COLUMNS <- setNames(paste0("beleid_", POLICY_RAW), POLICY_RAW)
policy_anchors <- read_slider_anchors(QSF_PATH, "beleid_voorkeur")
stopifnot(nrow(policy_anchors) == 9)
for (v in POLICY_COLUMNS) all_rows[[v]] <- policy_anchors$pos[all_rows[[v]]]


# ---- Demographics, labelled from the .sav ------------------------------------------
age_scale <- qsf_scale("leeftijd", questions)
demo_labels <- sav_value_labels(names(DEMOGRAPHICS))
# the agency's age_7_rec has the QSF's seven bands
stopifnot(identical(unname(demo_labels$age_7_rec), as.numeric(age_scale$code)),
          identical(gsub("[^0-9+]", "", names(demo_labels$age_7_rec)),
                    gsub("[^0-9+]", "", sub(" jaar of ouder", "+", age_scale$label))))
all_rows$leeftijd <- label_from_scale(raw$age_7_rec, age_scale)
for (v in names(DEMOGRAPHICS)[-1]) all_rows[[DEMOGRAPHICS[[v]]]] <- label_codes(raw[[v]], demo_labels[[v]])
demo_cols <- unname(DEMOGRAPHICS)


# ---- Wording and item metadata -----------------------------------------------------
belief_cols <- BELIEF_META$variable
gedrag_cols <- GEDRAG_META$variable
wording <- c(
  setNames(BELIEF_META$wording, belief_cols),
  setNames(GEDRAG_META$wording, gedrag_cols),
  setNames(qsf_items("beleid_voorkeur", questions), POLICY_COLUMNS),
  setNames(qsf_items("gebruik_ooit", questions), unname(USE_RAW)),
  INVOLVEMENT_WORDING,
  setNames(qsf_items("Q28", questions), EMOTION_TOPICS),
  leeftijd        = "Wat is je leeftijd? (age_7_rec)",
  geslacht        = "Geslacht (sex_rec)",
  opleiding       = "Opleiding (edu_3_rec)",
  stedelijkheid   = "Stedelijkheid (urb)",
  stemherinnering = "Stemherinnering (vote_recall_rec)")
stopifnot(identical(unname(wording[belief_cols]),
                    unname(unlist(lapply(c("beliefs_society", "beliefs_addiction", "beliefs_regulate",
                                           "beliefs_crime"), qsf_items, questions = questions)))))

# the shared answer scale of a set of columns, from the QSF
question_scale <- function(columns, multi = FALSE) {
  tags <- unique(sub("_[0-9]+[a-z]?$", "", columns))
  tags <- unique(ifelse(grepl("^gebruik_", tags), "gebruik_ooit", tags))
  scales <- lapply(tags, qsf_scale, questions = questions)
  stopifnot(all(vapply(scales, identical, logical(1), scales[[1]])))
  scales[[1]]
}

meta <- rbind(data.frame(variable = belief_cols, block = "beliefs", category = BELIEF_META$subscale,
                         direction = BELIEF_META$direction),
              data.frame(variable = gedrag_cols, block = "gedrag_beleid", category = "gedrag",
                         direction = GEDRAG_META$direction))
item_meta <- data.frame(variable = meta$variable, block = meta$block,
                        item_text_nl = unname(wording[meta$variable]), category = meta$category,
                        direction_itempool = meta$direction, direction_used = meta$direction,
                        reverse_coded = ifelse(meta$direction == "Anti", "yes", "no"), note = "",
                        row.names = meta$variable)


# ---- Column blocks and `pilot` -------------------------------------------------------
likert_cols  <- item_meta$variable
slider_cols  <- unname(POLICY_COLUMNS)
invol_cols   <- select_items("betrokkenheid", names(all_rows))
stopifnot(identical(invol_cols, INVOLVEMENT_RAW))
USE_COLUMNS  <- setNames(unname(USE_RAW), c("alcohol", "tabak", "soft", "hard"))
subst_cols   <- unname(USE_COLUMNS)
reverse_cols <- item_meta$variable[item_meta$reverse_coded == "yes"]
REQUIRED_COLS <- c(likert_cols, emotion_cols, invol_cols)
substance_scale <- question_scale(subst_cols)
substance_scale$code <- as.character(as.numeric(substance_scale$code) - 1)

binaries <- as.vector(t(outer(EMOTION_TOPICS, EMOTION_ANSWERS, paste, sep = "_")))
pilot <- all_rows[all_rows$complete, c("ResponseId", "duration_sec", "duration_min", demo_cols,
                                       likert_cols, paste0(likert_cols, "_scored"), slider_cols,
                                       invol_cols, "betrokkenheid_mean", subst_cols, emotion_cols,
                                       binaries, "substance_ladder")]
rownames(pilot) <- NULL
dropped <- all_rows[!all_rows$complete, ]
emotions_binary <- pilot[c("ResponseId", binaries)]
n_pilot <- nrow(pilot)
scored_items <- as.matrix(pilot[paste0(item_meta$variable, "_scored")])
colnames(scored_items) <- item_meta$variable
ANALYSIS_COLUMNS <- names(pilot)


# ---- The splits the descriptive charts offer -------------------------------------------
build_demographics <- function(pilot) {
  grouped <- function(label, column, mode = "grouped")
    list(label = label, mode = mode, groups = levels(column), assign = as.integer(column))
  list(
    politics  = grouped("Stemherinnering (vote_recall_rec)", pilot$stemherinnering),
    gender    = grouped("Geslacht (sex_rec)", pilot$geslacht),
    age       = grouped("Leeftijd", pilot$leeftijd, mode = "stacked"),
    substance = grouped("Middelengebruik", pilot$substance_ladder),
    opleiding = grouped("Opleiding (edu_3_rec)", pilot$opleiding, mode = "stacked"),
    stad      = grouped("Stedelijkheid (urb)", pilot$stedelijkheid),
    mdma      = grouped("MDMA-regulering",
                        factor(ifelse(pilot$beleid_mdma >= MDMA_SUPPORT_MIN, 2L, 1L),
                               levels = 1:2, labels = c("tegen", "voor"))))
}
