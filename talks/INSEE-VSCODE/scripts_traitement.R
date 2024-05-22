# Packages ----
library(tidyverse)
library(sf)
library(DT)
library(gt)
library(mapsf)
library(RPostgres)
library(gtsummary)
library(plotly)
library(data.table)

# Data Loading ----

con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "idea5", host = "localhost")

query <- "SELECT i.code, vi.valeur, vi.diagnostic_id FROM valeur_indicateur vi JOIN indicateur i ON vi.indicateur_id = i.id"

full_database <- DBI::dbGetQuery(con, query) |>
    tibble::as_tibble() |>
    dplyr::mutate(type_indicateur = ifelse(stringr::str_detect(code, "MTD_"), yes = "metadonnees", no = "items")) |>
    dplyr::select(farm_id = diagnostic_id, code, valeur, type_indicateur) |>
    dplyr::mutate(farm_id = as.character(farm_id)) |>
    unique()

## Data usages
organisme <- tibble(dbGetQuery(con, "SELECT * FROM organisme"))
usages <- tibble(dbGetQuery(con, "SELECT * FROM tl_user_usage"))
user <- tibble(dbGetQuery(con, "SELECT * FROM user_account"))
diagnostic <- tibble(dbGetQuery(con, "SELECT * FROM diagnostic"))
comptage_orga <- diagnostic |>
    group_by(organisme_id) |>
    summarise(n = n_distinct(id)) |>
    arrange(desc(n))

# Déconnexion
DBI::dbDisconnect(con)
rm(con)

items <- subset(full_database, type_indicateur == "items") %>%
    dplyr::select(farm_id, item = code, value = valeur) %>%
    dplyr::mutate(item = stringr::str_replace_all(item, "(?<=[:upper:])0", "")) %>% # Convert A01 to A1
    dplyr::mutate(item = stringr::str_remove(item, "IDEA_"))

metadonnees <- subset(full_database, type_indicateur == "metadonnees")

source("functions.R")

cleaned_data <- make_clean_usages(organisme, usages, user, diagnostic, comptage_orga)

load("data/group_list.RData")

IDEA_data <- group_list


# Graphes ----

N <- sum(cleaned_data$clean_organismes$n_calc)


## Diag/orga
cc <- cleaned_data$clean_organismes |>
    mutate(Type_N1 = paste0(Code_N1, " - ", Type_N1)) |>
    mutate(`Sous-Types` = paste0(Code_N2, " - ", Type_N2))

cc |>
    rowwise() |>
    mutate(Type_N1 = IDEATools:::wrapit(Type_N1, 50)) |>
    ungroup() |>
    mutate(Type_N1 = fct_rev(Type_N1)) |>
    group_by(Type_N1) |>
    summarise(n_diag = sum(n_calc)) |>
    ggplot(aes(x = Type_N1, y = n_diag)) +
    geom_col(color = "black", fill = "#f3b20a") +
    coord_flip() +
    IDEATools:::theme_idea(base_size = 18) +
    theme(axis.title.y = element_blank()) +
    labs(y = "Nombre de diagnostics", x = "Type", caption = paste0("N = ", N, " diagnostics")) +
    geom_label(aes(label = n_diag))

ggsave("./output/plots/ndiag_orga_n1.png", dpi = "retina", width = 10.5, height = 7.07)

cc |>
    rowwise() |>
    mutate(`Sous-Types` = IDEATools:::wrapit(`Sous-Types`, 75)) |>
    ungroup() |>
    mutate(`Sous-Types` = fct_rev(`Sous-Types`)) |>
    group_by(Type_N1, `Sous-Types`) |>
    summarise(n_diag = sum(n_calc)) |>
    ggplot(aes(x = `Sous-Types`, y = n_diag)) +
    geom_col(color = "black", fill = "#f3b20a") +
    coord_flip() +
    IDEATools:::theme_idea(base_size = 18) +
    labs(y = "Nombre de diagnostics", x = "Type", caption = paste0("N = ", N, " diagnostics")) +
    theme(axis.title.y = element_blank()) +
    theme(axis.text = element_text(size = 8)) +
    geom_label(aes(label = n_diag))

ggsave("./output/plots/ndiag_orga_n2.png", dpi = "retina", width = 10.5, height = 12)


## Typo orga

N_orga <- n_distinct(cleaned_data$clean_organismes$id_orga)
N_orga_sansC <- n_distinct(cleaned_data$clean_organismes |> filter(Code_N1 != "C") |> pull(id_orga))

cleaned_data$clean_organismes |>
    janitor::tabyl(region_name) |>
    mutate(percent = paste0(round(percent * 100, 1), "%")) |>
    mutate(region_name = fct_reorder(region_name, n)) |>
    ggplot(aes(x = region_name, y = n, label = percent)) +
    geom_col(color = "black", fill = "#7DC572") +
    # geom_label(aes(label = n), size = 6) +
    coord_flip() +
    theme_light(base_size = 16) +
    labs(y = "Nombre d'organismes") +
    theme(axis.title.y = element_blank()) +
    geom_label(aes(label = n)) +
    labs(caption = paste0("N = ", N_orga, " organismes"))

ggsave("./output/plots/loc_orga.png", dpi = "retina", width = 10.5, height = 7.07)



cleaned_data$clean_organismes |>
    group_by(Code_N1) |>
    mutate(ntot = n_distinct(id_orga)) |>
    ungroup() |>
    mutate(Type_N1 = paste0(Code_N1, " - ", Type_N1)) |>
    mutate(`Sous-Types` = paste0(Code_N2, " - ", Type_N2)) |>
    group_by(Type_N1) |>
    summarize(n = n_distinct(id_orga)) |>
    ungroup() |>
    rowwise() |>
    mutate(Type_N1 = factor(Type_N1, levels = unique(Type_N1))) |>
    mutate(Type_N1 = IDEATools:::wrapit(Type_N1, 50)) |>
    ungroup() |>
    mutate(Type_N1 = fct_rev(Type_N1)) |>
    ungroup() |>
    ggplot(aes(x = Type_N1, y = n)) +
    geom_col(color = "black", fill = "#7DC572") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea(base_size = 18) +
    labs(y = "Nombre d'organismes", caption = paste0("N = ", N_orga, " organismes")) +
    theme(axis.title.y = element_blank())

ggsave("./output/plots/orga_n1.png", dpi = "retina", width = 10.5, height = 7.07)



cleaned_data$clean_organismes |>
    group_by(Code_N1) |>
    filter(Code_N1 != "C") |>
    mutate(ntot = n_distinct(id_orga)) |>
    ungroup() |>
    mutate(Type_N1 = paste0(Code_N1, " - ", Type_N1)) |>
    mutate(`Sous-Types` = paste0(Code_N2, " - ", Type_N2)) |>
    group_by(Type_N1) |>
    summarize(n = n_distinct(id_orga)) |>
    ungroup() |>
    rowwise() |>
    mutate(Type_N1 = factor(Type_N1, levels = unique(Type_N1))) |>
    mutate(Type_N1 = IDEATools:::wrapit(Type_N1, 50)) |>
    ungroup() |>
    mutate(Type_N1 = fct_rev(Type_N1)) |>
    ungroup() |>
    ggplot(aes(x = Type_N1, y = n)) +
    geom_col(color = "black", fill = "#7DC572") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea(base_size = 18) +
    labs(y = "Nombre d'organismes", caption = paste0("N = ", N_orga_sansC, " organismes")) +
    theme(axis.title.y = element_blank())

ggsave("./output/plots/orga_n1_sansC.png", dpi = "retina", width = 10.5, height = 7.07)






cleaned_data$clean_organismes |>
    group_by(Code_N1) |>
    mutate(ntot = n_distinct(id_orga)) |>
    ungroup() |>
    mutate(Type_N1 = paste0(Code_N1, " - ", Type_N1)) |>
    mutate(`Sous-Types` = paste0(Code_N2, " - ", Type_N2)) |>
    group_by(Type_N1, `Sous-Types`) |>
    summarize(n = n_distinct(id_orga)) |>
    ungroup() |>
    mutate(`Sous-Types` = fct_rev(`Sous-Types`)) |>
    ggplot(aes(x = `Sous-Types`, y = n)) +
    geom_col(color = "black", fill = "#7DC572") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea() +
    labs(y = "Nombre d'organismes", caption = paste0("N = ", N_orga, " organismes")) +
    theme(axis.title.y = element_blank()) +
    theme(axis.text = element_text(size = 7))

ggsave("./output/plots/orga_n2.png", dpi = "retina", width = 10.5, height = 8.07)

## Utilisateurs
N_user <- n_distinct(cleaned_data$clean_utilisateurs$id)
cleaned_data$clean_utilisateurs |>
    mutate(Statut_lbl = case_when(
        Statut == "F1" ~ "Enseignant (lycée)",
        Statut == "F2" ~ "Enseignant chercheur",
        Statut == "F3" ~ "Chercheur",
        Statut == "F4" ~ "Conseiller",
        Statut == "F5" ~ "Animateur",
        Statut == "F6" ~ "Chargé de mission /projet",
        Statut == "F7" ~ "Directeur",
        Statut == "F8" ~ "Agriculteur /salarié exploitation agricole",
        Statut == "F9" ~ "Autre (permanent)",
        Statut == "0" ~ "Non renseigné",
        Statut == "S1" ~ "CDD",
        Statut == "S2" ~ "Stagiaire",
        Statut == "S3" ~ "Alternant /Apprenti",
        Statut == "S4" ~ "En thèse ou en post-doc",
        Statut == "S5" ~ "Autre (non permanent)"
    )) |>
    mutate(Statut = paste0(Statut, " - ", Statut_lbl)) |>
    janitor::tabyl(Statut) |>
    mutate(percent = paste0(round(percent * 100, 1), "%")) |>
    mutate(Statut = fct_rev(Statut)) |>
    ggplot(aes(x = Statut, y = n, label = percent)) +
    geom_col(color = "#000000", fill = "#11dd8f") +
    coord_flip() +
    theme_light(base_size = 16) +
    labs(y = "Nombre d'utilisateurs", caption = paste0("N = ", N_user, " utilisateurs")) +
    theme(axis.title.y = element_blank()) +
    geom_label(aes(label = n))

ggsave("./output/plots/users.png", dpi = "retina", width = 10.5, height = 7.07)

## Usages
n_usages <- nrow(cleaned_data$clean_usages)
n_usages_sans5et6 <- nrow(cleaned_data$clean_usages |> filter(Code_N1 != "USA5" & Code_N1 != "USA6"))

cleaned_data$clean_usages |>
    group_by(Code_N1) |>
    mutate(ntot = n()) |>
    ungroup() |>
    mutate(Usage_N1 = paste0(Code_N1, " - ", Usage_N1)) |>
    mutate(`Sous-Usages` = paste0(Code_N2, " - ", Usage_N2)) |>
    group_by(`Sous-Usages`) |>
    summarize(n = n()) |>
    ungroup() |>
    mutate(`Sous-Usages` = fct_rev(`Sous-Usages`)) |>
    ggplot(aes(x = `Sous-Usages`, y = n)) +
    geom_col(color = "black", fill = "#c00000") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea() +
    theme(axis.title = element_blank()) +
    labs(caption = paste0("N = ", n_usages, " usages"))

ggsave("./output/plots/usages_N2.png", dpi = "retina", width = 12.5, height = 7.75)


cleaned_data$clean_usages |>
    filter(Code_N1 != "USA5" & Code_N1 != "USA6") |>
    group_by(Code_N1) |>
    mutate(ntot = n()) |>
    ungroup() |>
    mutate(Usage_N1 = paste0(Code_N1, " - ", Usage_N1)) |>
    mutate(`Sous-Usages` = paste0(Code_N2, " - ", Usage_N2)) |>
    group_by(`Sous-Usages`) |>
    summarize(n = n()) |>
    ungroup() |>
    mutate(`Sous-Usages` = fct_rev(`Sous-Usages`)) |>
    ggplot(aes(x = `Sous-Usages`, y = n)) +
    geom_col(color = "black", fill = "#c00000") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea() +
    theme(axis.title = element_blank()) +
    labs(caption = paste0("N = ", n_usages_sans5et6, " usages"))

ggsave("./output/plots/usages_N2_sans5et6.png", dpi = "retina", width = 12.5, height = 7.75)



cleaned_data$clean_usages |>
    group_by(Code_N1) |>
    mutate(ntot = n()) |>
    ungroup() |>
    mutate(Usage_N1 = paste0(Code_N1, " - ", Usage_N1)) |>
    group_by(Usage_N1) |>
    summarize(n = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(`Usage_N1` = IDEATools:::wrapit(`Usage_N1`, 50)) |>
    ungroup() |>
    mutate(`Usage_N1` = fct_rev(`Usage_N1`)) |>
    ggplot(aes(x = `Usage_N1`, y = n)) +
    geom_col(color = "black", fill = "#c00000") +
    geom_label(aes(label = n)) +
    coord_flip() +
    IDEATools:::theme_idea(base_size = 18) +
    theme(axis.title = element_blank()) +
    labs(caption = paste0("N = ", n_usages, " usages"))

ggsave("./output/plots/usages_N1.png", dpi = "retina", width = 12, height = 7.07)

# Tableaux ----

cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    group_by(Code_N1) |>
    summarise(n = n_distinct(id))

cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    group_by(Code_N2) |>
    summarise(n = n_distinct(id))

# Usage orga ----
cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    group_by(Code_N1) |>
    summarise(n = n_distinct(id))

cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    group_by(Code_N2) |>
    summarise(n = n_distinct(id))

#  Tableaux croisés Usage / Type orga ----

cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    distinct(id, id_orga, Code_N1, Code_N2) -> user_usage

user_usage |>
    right_join(cleaned_data$clean_organismes, by = "id_orga")


group_by(Code_N1.y, Code_N1.x) |>
    summarise(n = n_distinct(id_orga)) |>
    pivot_wider(id_cols = Code_N1.y, names_from = Code_N1.x, values_from = n, values_fill = 0)


full_db <- cleaned_data$clean_usages |>
    inner_join(cleaned_data$clean_utilisateurs, by = "id") |>
    distinct(id, id_orga, Code_N1, Code_N2) |>
    left_join(cleaned_data$clean_organismes, by = "id_orga")
