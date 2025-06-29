library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(fmsb)
library(scales)
library(bslib)

# Définir une palette de couleurs
library(viridis)
color_palette <- viridis(5)  # Vous pouvez ajuster le nombre de couleurs en fonction de vos besoins



nba_base_equipe <- read_excel("Données/nba_base_equipe.xlsx")
loca_club <- read_excel("Données/base_loc (1).xlsx") # base de donnée carte club NBA

# Charger la base de Données
base_jeux <- read_excel("Données/nba_players.xlsx") # base de donnée jeux

nba_base_complete <- read_excel("Données/nba_base_complete.xlsx")
nba_base_complete <- nba_base_complete %>%
  arrange(Player, Season)

# Créer la nouvelle base de données "joueurs_unique_moy" avec arrondi à 2 décimales et les nouveaux noms
joueurs_unique_moy <- nba_base_complete %>%
  group_by(Player) %>%
  summarise(
    # Moyennes sur toutes les saisons (arrondi à 2 décimales)
    MP_moy_alltime = round(mean(MP_moy, na.rm = TRUE), 2),
    FG_Perc_alltime = round(mean(FG_Perc, na.rm = TRUE), 2),
    `3P_Perc_alltime` = round(mean(`3P_Perc`, na.rm = TRUE), 2),
    FT_Perc_alltime = round(mean(FT_Perc, na.rm = TRUE), 2),
    TRB_moy_alltime = round(mean(TRB_moy, na.rm = TRUE), 2),
    AST_moy_alltime = round(mean(AST_moy, na.rm = TRUE), 2),
    STL_moy_alltime = round(mean(STL_moy, na.rm = TRUE), 2),
    BLK_moy_alltime = round(mean(BLK_moy, na.rm = TRUE), 2),
    PTS_moy_alltime = round(mean(PTS_moy, na.rm = TRUE), 2),
    
    # Sommes des totaux : somme des stats sur toutes les saisons (renommée en "career")
    MP_career = sum(MP_tot, na.rm = TRUE),
    FG_career = sum(FG_tot, na.rm = TRUE),
    `3P_career` = sum(`3P_tot`, na.rm = TRUE),
    FT_career = sum(FT_tot, na.rm = TRUE),
    TRB_career = sum(TRB_tot, na.rm = TRUE),
    AST_career = sum(AST_tot, na.rm = TRUE),
    STL_career = sum(STL_tot, na.rm = TRUE),
    BLK_career = sum(BLK_tot, na.rm = TRUE),
    PTS_career = sum(PTS_tot, na.rm = TRUE),
    
    # Moyenne de "Win_Perc" sur toutes les saisons (arrondi à 2 décimales)
    Win_Perc_alltime = round(mean(Win_Perc, na.rm = TRUE), 2)
  )

# Sélection des statistiques pour le radar
selected_stats <- c("PTS_career", "Win_Perc_alltime", "3P_Perc_alltime",
                    "AST_moy_alltime", "TRB_moy_alltime", "STL_moy_alltime")

# Normalisation des données sur les statistiques sélectionnées
players_norm <- joueurs_unique_moy %>%
  select(Player, all_of(selected_stats)) %>%
  mutate(across(where(is.numeric), ~ . / max(., na.rm = TRUE)))

# Passage au format long
players_long <- players_norm %>%
  gather(key = "Stat", value = "Value", -Player)

# Fonction pour comparer les réponses
compare_players <- function(player_guess, player_target) {
  compare <- data.frame(
    Caractéristique = c("Nom", "Équipe", "Début de carrière", "Points en carrière", "Position", "Titres gagnés"),
    Valeur_entrée = c(
      player_guess$Nom,
      player_guess$`Équipe actuelle`,
      player_guess$`Début de carrière`,
      player_guess$`Points en carrière`,
      player_guess$Position,
      player_guess$`Titres gagnés`
    ),
    Résultat = c(
      ifelse(player_guess$Nom == player_target$Nom, "✔️ Correct", "❌ Faux"),
      ifelse(player_guess$`Équipe actuelle` == player_target$`Équipe actuelle`, "🟢 Correct", "⚪ Différent"),
      ifelse(player_guess$`Début de carrière` == player_target$`Début de carrière`, "🟢 Correct",
             ifelse(abs(player_guess$`Début de carrière` - player_target$`Début de carrière`) <= 2, "🟡 Proche",
                    ifelse(player_guess$`Début de carrière` < player_target$`Début de carrière`, "🔴 Trop tôt", "🟠 Trop tard"))),
      ifelse(player_guess$`Points en carrière` == player_target$`Points en carrière`, "🟢 Correct",
             ifelse(abs(player_guess$`Points en carrière` - player_target$`Points en carrière`) <= 2000, "🟡 Proche",
                    ifelse(player_guess$`Points en carrière` < player_target$`Points en carrière`, "🔴 Trop bas", "🟠 Trop élevé"))),
      ifelse(player_guess$Position == player_target$Position, "🟢 Correct", "⚪ Différent"),
      ifelse(player_guess$`Titres gagnés` == player_target$`Titres gagnés`, "🟢 Correct",
             ifelse(abs(player_guess$`Titres gagnés` - player_target$`Titres gagnés`) <= 1, "🟡 Proche",
                    ifelse(player_guess$`Titres gagnés` < player_target$`Titres gagnés`, "🔴 Trop bas", "🟠 Trop élevé")))
    )
  )
  
  return(compare)
}

theme <- bs_theme(version = 5)  # Reset toutes les couleurs
theme <- bs_theme(bootswatch = "minty")  # Appliquer Minty

base_1 <- nba_base_complete

base_jeux <- base_1 %>%
  group_by(Player) %>%
  summarise(
    `Points en carrière` = sum(PTS_tot, na.rm = TRUE)
  )

#####################################################    Équipe   ######################################
base_player_team <- base_1 %>%
  group_by(Player) %>%
  arrange(desc(Season)) %>%  # Trie par saison décroissante
  slice(1) %>%  # Garde uniquement la ligne la plus récente pour chaque joueur
  ungroup() %>%
  select(Player, Équipe = Franchise_Nom_US)  # Garde uniquement les colonnes nécessaires

# Fusion avec la base principale si nécessaire
base_1 <- base_1 %>%
  left_join(base_player_team, by = "Player")

base_jeux$`Équipe actuelle` <- base_player_team$Équipe

#####################################################    Début de carrière   ######################################

base_debut <- base_1 %>%
  group_by(Player) %>%
  slice(1) %>%  # Sélectionne la première apparition du joueur dans la base
  ungroup() %>%
  mutate(Début = First_Appearance)  # Ajoute la colonne Début avec la valeur de First_Appearance

# Extrait les 4 premiers caractères de 'First_Appearance' de base_debut
base_jeux$`Début de carrière` <- substr(base_debut$First_Appearance, 1, 4)

base_jeux$`Début de carrière` <- as.numeric(base_jeux$`Début de carrière`)

#####################################################    Position   ######################################

base_jeux$Position <- base_debut$Pos

#####################################################    Titres gagnés   ######################################

base_jeux$`Titres gagnés` <- base_debut$Titres_Nba_Tot

#####################################################    Nom   ######################################

base_jeux$Nom <- base_jeux$Player

# UI pour l'application
# UI pour l'application
ui <- navbarPage(
  title = tags$img(src = "h.png", height = "75px"),  # Assurez-vous que h.png est dans le dossier www
  
  # Ajouter du CSS pour personnaliser l'apparence de la page
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
        background-image: url(fond.png);
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
        background-repeat: no-repeat;
      }

      /* 🎯 Transparence pour les panneaux, barres et cartes */
      .well, .panel, .navbar, .card {
        background-color: rgba(255, 255, 255, 0.85) !important;
      }

      /* 🌟 Style du titre */
      .navbar-brand {
        font-size: 50px !important;
        display: flex;
        align-items: center;  /* Centrer verticalement le logo */
      }

      /* Style de la navbar */
      .navbar {
        background-color: #f7ac4b !important;  /* Couleur personnalisée */
        border-color: #f7ac4b;
        height: 95px;  /* Hauteur de la navbar */
        display: flex;  /* Utiliser flexbox pour aligner le logo et les onglets */
        align-items: center;  /* Aligner verticalement */
        justify-content: space-between;  /* Espacer le logo et les onglets */
        padding: 0 20px;  /* Ajouter de l'espace autour de la barre de navigation */
      }

      /* Alignement des onglets */
      .navbar-nav {
        margin-left: auto;  /* Déplacer les onglets vers la droite */
      }

      /* Augmenter la taille des noms des onglets */
      .navbar-nav > li > a {
        font-size: 20px !important;  /* Augmenter la taille des noms des onglets */
        padding: 15px 20px !important;
        color: white !important;  /* Changer la couleur des noms d'onglets */
      }

      /* Spécifier la couleur des liens au survol */
      .navbar-nav > li > a:hover {
        color: black !important;  /* Couleur des liens au survol */
      }

      /* Changer la couleur de l'onglet sélectionné en noir */
      .navbar-nav > .active > a {
        color: black !important;  /* Changer la couleur du texte de l'onglet sélectionné */
        background-color: transparent !important;  /* Enlever le fond */
      }
    /* Appliquer un fond blanc et un texte noir à tous les tableaux */
      table, th, td {
        background-color: white !important;
        color: black !important;
      }

      /* Si tu utilises DT, forcer le fond blanc pour la datatable */
      .dataTable {
        background-color: white !important;
      }

      /* Facultatif : ajouter des bordures pour améliorer la lisibilité */
      table, th, td {
        border: 1px solid #ccc !important;
      }
      /* Rendre le texte en blanc */
#selected_team, #team_conf_div, #nba_titles, label, .dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate {
  color: white !important;
}

/* S'assurer que les labels et les textes restent lisibles */
.dataTables_wrapper .dataTables_length label,
.dataTables_wrapper .dataTables_filter label {
  color: white !important;
}

/* Texte dans la pagination et les informations */
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color: white !important;
}

/* Texte dans le sélecteur dropdown pour la pagination */
.dataTables_length select {
  background-color: white !important;
  color: black !important;
}

/* Texte dans la barre de recherche */
.dataTables_filter input {
  background-color: white !important;
  color: black !important;
  border: 1px solid white !important;
}
/* Centrer le texte dans les cellules du tableau */
.dataTable tbody td {
  text-align: center !important;
  vertical-align: middle !important;
}

/* Centrer aussi les en-têtes du tableau */
.dataTable thead th {
  text-align: center !important;
}

    "))
  ),
  
  # Onglet Accueil
  tabPanel("Accueil",
           fluidPage(
             # Style pour justifier le texte et ajouter un contour noir
             tags$style(HTML("
             p {
               font-size: 20px;
               color: white;
               text-align: justify;
               text-shadow: 2px 2px 4px black
             }
             h1, h2 {
               font-size: 40px;
               color: white;
               text-align: center;
               text-shadow: 2px 2px 4px black

             }
             h3 {
                color: white
              }
             h1 {
             font-size: 36px;
             text-align: center;
           }
             h2 {
             font-size: 28px;
             margin-top: 30px;,
           }")),
             
             h1("Bienvenue sur notre site AirBall"),
             
             # Texte explicatif
             
             p("Notre objectif est de vous offrir une expérience interactive et complète pour découvrir le monde de la NBA, ses équipes, et ses joueurs. Grâce à des statistiques détaillées et des informations mises à jour régulièrement, nous vous permettons d'explorer les performances des franchises, de comparer les joueurs, et de suivre l'évolution de la saison."),
             
             p("Que vous soyez un fan de longue date ou un nouveau venu, notre plateforme est conçue pour offrir des informations claires et facilement accessibles, tout en mettant en avant les moments forts de chaque équipe. Découvrez les équipes par conférence et division, et plongez dans les statistiques individuelles des joueurs pour mieux comprendre leur impact sur le jeu."),
             
             p("Nous espérons que vous apprécierez cette expérience et que vous trouverez toutes les données dont vous avez besoin pour approfondir vos connaissances sur le basket NBA."),
             
             h2("Pourquoi nous avons créé ce site ?"),
             
             fluidRow(
               column(8,  # 8/12 de l'espace pour le texte
                      p("Depuis des décennies, la question de savoir qui est le plus grand joueur de tous les temps (*Greatest Of All Time - GOAT*) alimente les débats entre fans de la NBA. Michael Jordan, LeBron James, Kobe Bryant, Kareem Abdul-Jabbar, ou encore d'autres légendes, chacun a ses arguments."),
                      
                      p("Avec AirBall, nous avons voulu apporter des éléments concrets à cette discussion, en mettant à disposition des statistiques détaillées et des comparaisons objectives. Grâce aux performances historiques des joueurs et des équipes, nous permettons à chacun de se forger sa propre opinion sur le GOAT, tout en découvrant l'évolution du basket au fil des époques.")
               ),
               column(4,  # 4/12 de l'espace pour l'image
                      img(src = "GoatBranding_web-series.png", height = "200px", width = "100%", style = "border-radius: 10px;"))
             )
           )
  ),
  
  tabPanel(
    "Carte", fluidPage(
      titlePanel("Toutes les équipes de la NBA sur une carte des États-Unis"),
      selectInput("selected_team_map", "Sélectionnez une équipe :", choices = NULL),  # Menu déroulant pour sélectionner une équipe
      leafletOutput("map", height = "600px")
    )
  )
  ,
  
  # Onglet Équipe avec des sous-onglets
  tabPanel(
    "Équipes",
    fluidPage(
      titlePanel("Informations sur les équipes NBA"),
      selectInput("selected_team", "Sélectionnez une équipe :", choices = NULL),
      selectInput("selected_season", "Sélectionnez une saison :", choices = NULL),
      uiOutput("team_logo"),
      h2(textOutput("team_name")),
      h4(textOutput("team_conf_div")),
      h5(textOutput("nba_titles")),
      DTOutput("team_stats"),
      DTOutput("players_table")  # Nouveau tableau pour afficher les joueurs
    )
  ),
  
  tabPanel(
    "Joueurs",
    tabsetPanel(
      tabPanel("Liste des joueurs",
               DTOutput("table"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selected_variable", "Sélectionner une variable :",
                               choices = c("MP_moy", "FG_Perc", "FT_Perc", "TRB_moy", "AST_moy", "STL_moy", "BLK_moy", "PTS_moy", "Win_Perc"),
                               selected = "PTS_moy")
                 ),
                 mainPanel(
                   uiOutput("player_page")
                 )
               )
      ),
      tabPanel("Liste des joueurs par carrière",
               DTOutput("table_carriere"),
               mainPanel(
                 uiOutput("player_carriere")
               )
      )
    )
  ),
  
  tabPanel(
    "Comparaison",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("selected_players_comparison", "Sélectionner des joueurs pour la comparaison :",
                       choices = joueurs_unique_moy$Player,
                       selected = c("LeBron James", "Stephen Curry"),
                       multiple = TRUE,
                       options = list(
                         placeholder = 'Sélectionner des joueurs...',
                         maxItems = 5,
                         selectOnTab = TRUE,
                         highlight = TRUE
                       )),
        selectInput("selected_stat_comparison", "Sélectionner une statistique pour la comparaison :",
                    choices = c("PTS_moy", "AST_moy", "TRB_moy", "FG_Perc", "3P_Perc"),
                    selected = "PTS_moy")
      ),
      mainPanel(
        plotOutput("comparison_plot"),  # Graphique de comparaison
        plotlyOutput("radarPlot"),  # Radar plot
        plotlyOutput("densityPlot") # Density plot
      )
    )
  ),
  
  tabPanel(
    "Devinez le joueur NBA",
    tags$style(HTML("
    .sidebar {
      width: 25%;
    }
  ")),
    sidebarLayout(
      sidebarPanel(
        h4("Entrez vos suppositions :"),
        selectizeInput("player_name_guess", "Nom du joueur :", choices = base_jeux$Nom, options = list(
          placeholder = 'Commencez à taper un nom...',
          maxItems = 1,
          create = FALSE
        )),
        actionButton("submit", "Valider"),
        textOutput("attempts_left")  # Afficher le nombre de tentatives restantes
      ),
      mainPanel(
        h3("Résultats"),
        tableOutput("results"),
        br(),
        h3("Indice :"),
        textOutput("hint"),
        br(),
        br(),
        h4("Historique des réponses :"),
        div(style = "height: 300px; overflow-y: scroll;",
            tableOutput("history_table")
        )
      )
    )
  ),
  
  tabPanel("À propos",
           fluidPage(
             titlePanel("À propos d'AirBall"),
             
             # Affichage du logo
             div(style = "text-align: center;",
                 img(src = "h.png", height = "150px")),
             
             br(),
             
             # Auteur
             h3("Auteur(s)", style = "font-size: 18px;"),
             p("Hafadhui DANIEL & Arthur ERNOUL DE LA PROVÔTÉ"),
             # Nous contacter
             h3("Nous contacter", style = "font-size: 18px;"),
             p("Pour toute question, suggestion ou demande, veuillez nous contacter à l'adresse suivante :", style = "font-size: 14px;"),
             
             # Email avec fond noir et texte blanc
             tags$a(href = "mailto:airball@contact.com",
                    style = "font-size: 20px; color: white; background-color: #f7ac4b; padding: 10px 15px; border-radius: 5px; text-decoration: none;",
                    "airball@contact.com")
           ),
           br(),
           h3("Communauté", style = "font-size: 18px;"),
           p("Rejoignez notre communauté sur Discord pour débattre du GOAT NBA et partager votre passion pour le basket !",
             style = "font-size: 14px;"),
           tags$a(href = "https://discord.gg/goatdebate",
                  style = "font-size: 18px; color: white; background-color: #5865F2; padding: 8px 12px;
                               border-radius: 5px; text-decoration: none; display: inline-block;",
                  "Rejoindre le Discord"),
           
           br(),
           br(),
           # Ressources documentaires
           h3("Site pour se documenter", style = "font-size: 16px;"),
           tags$a(href = "https://www.parlons-basket.com/2020/05/18/nba-qui-est-le-goat-les-fans-tranche-dans-un-grand-sondage-despn/",
                  "Parlons Basket - Qui est le GOAT ?", style = "display: block;"),
           tags$a(href = "https://www.basketsession.com/NBA/franchises-nba-goat-2023-628825/",
                  "BasketSession - Franchises NBA GOAT 2023", style = "display: block;"),
           
           # Remerciements avec taille réduite
           h3("Remerciements", style = "font-size: 18px;"),
           p("Marie VAUGOYEAU, Aboubacar DOSSO", style = "font-size: 14px;"),
           
           br()
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Liste des équipes uniques
  teams_data <- nba_base_equipe %>%
    select(Equipe, Conf, Div, logo, Champion_Nba_Tot) %>%
    distinct()
  
  # Remplissage du menu déroulant avec les équipes disponibles
  observe({
    updateSelectInput(session, "selected_team", choices = teams_data$Equipe)
  })
  
  observeEvent(input$selected_team, {
    req(input$selected_team)  # Vérifie qu'une équipe est sélectionnée
    
    selected_team <- teams_data %>%
      filter(Equipe == input$selected_team) %>%
      group_by(logo) %>%
      summarise(
        Equipe = first(Equipe),
        Conf = first(Conf),
        Div = first(Div),
        Champion_Nba_Tot = first(Champion_Nba_Tot)
      )
    if (nrow(selected_team) == 0) return()  # Sécurité si aucune équipe trouvée
    
    # Mettre à jour les choix de saison dans l'ordre décroissant
    available_seasons <- nba_base_complete %>%
      filter(Franchise_Nom_US == input$selected_team) %>%
      select(Season) %>%
      distinct() %>%
      arrange(desc(Season)) %>%  # Trier les saisons dans l'ordre décroissant
      pull(Season)
    
    updateSelectInput(session, "selected_season", choices = available_seasons)
    
    # Affichage du logo et du trophée si l'équipe a remporté le titre
    output$team_logo <- renderUI({
      req(selected_team$logo, input$selected_season)
      
      # Vérifier si l'équipe a remporté le titre pour la saison sélectionnée
      is_champion <- nba_base_complete %>%
        filter(Franchise_Nom_US == input$selected_team, Season == input$selected_season) %>%
        pull(Champion_Nba) %>%
        any(. == 1)
      
      tags$div(
        tags$img(src = selected_team$logo, width = 100, height = 100, style = "display: block; margin: auto;"),
        if (is_champion) tags$img(src = "nba_trophy.png", width = 100, height = 100, style = "display: block; margin: auto;")
      )
    })
    
    # Affichage du nom de l'équipe, conférence, division et titres NBA
    output$team_name <- renderText({ selected_team$Equipe })
    output$team_conf_div <- renderText({ paste(selected_team$Conf, "-", selected_team$Div) })
    output$nba_titles <- renderText({ paste("Nombre de Titres NBA :", selected_team$Champion_Nba_Tot) })
    
    # Calcul des statistiques de l'équipe
    team_stats <- nba_base_equipe %>%
      filter(Equipe == input$selected_team) %>%
      group_by(Season) %>%
      summarise(
        W = max(W, na.rm = TRUE),
        L = max(L, na.rm = TRUE),
        Win_Perc = mean(Win_Perc, na.rm = TRUE),  # Moyenne correcte
        GB = max(GB, na.rm = TRUE),
        PS_per_G = mean(PS_per_G, na.rm = TRUE),
        PA_per_G = mean(PA_per_G, na.rm = TRUE),
        SRS = mean(SRS, na.rm = TRUE)  # Moyenne correcte
      ) %>%
      arrange(desc(Season))  # Trier les saisons du plus récent au plus ancien
    
    # Affichage du tableau des statistiques
    output$team_stats <- renderDT({
      datatable(team_stats, options = list(pageLength = 5))
    })
  })
  
  observeEvent(input$selected_season, {
    req(input$selected_season)  # Vérifie qu'une saison est sélectionnée
    
    # Filtrer les joueurs de l'équipe pour la saison sélectionnée
    players_data <- nba_base_complete %>%
      filter(Franchise_Nom_US == input$selected_team, Season == input$selected_season) %>%
      select(Player, Season, MP_moy, FG_Perc, FT_Perc, TRB_moy, AST_moy, STL_moy, BLK_moy, PTS_moy, Win_Perc)
    
    # Affichage du tableau des joueurs
    output$players_table <- renderDT({
      datatable(players_data, options = list(pageLength = 10))
    })
  })
  
  ####### tableau saison   ############################
  output$table <- renderDT({
    datatable(nba_base_complete, selection = "single")
  })
  
  output$player_page <- renderUI({
    req(input$table_rows_selected)
    joueur <- nba_base_complete[input$table_rows_selected, ]
    
    tagList(
      h3(paste("Statistiques de", joueur$Player)),
      plotOutput("player_plot")
    )
  })
  
  output$player_plot <- renderPlot({
    req(input$table_rows_selected)
    joueur <- nba_base_complete[input$table_rows_selected, ]
    
    # Filtrer les données pour le joueur sélectionné
    player_data <- nba_base_complete %>%
      filter(Player == joueur$Player) %>%
      arrange(Season)
    
    # Variable choisie
    selected_variable <- input$selected_variable
    
    # Créer le graphique
    ggplot(player_data, aes_string(x = "Season", y = selected_variable, group = 1)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(title = paste("Évolution de", selected_variable, "pour", joueur$Player),
           x = "Saison",
           y = selected_variable)
  })
  
  ####### tableau carrière   ############################
  output$table_carriere <- renderDT({
    datatable(joueurs_unique_moy, selection = "single")
  })
  
  output$player_carriere <- renderUI({
    req(input$table_carriere_rows_selected)
    joueur <- joueurs_unique_moy[input$table_carriere_rows_selected, ]
    
    tagList(
      h3(paste("Statistiques de", joueur$Player)),
      plotlyOutput("player_plot_carriere")
    )
  })
  
  output$player_plot_carriere <- renderPlotly({
    req(input$table_carriere_rows_selected)
    joueur <- joueurs_unique_moy[input$table_carriere_rows_selected, ]
    
    # Filtrer les données pour le joueur sélectionné
    player_data <- players_long %>%
      filter(Player == joueur$Player)
    
    # Créer le graphique en radar
    plot_obj <- plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = player_data$Value,
        theta = player_data$Stat,
        fill = 'toself',
        name = joueur$Player
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1))
        ),
        title = list(text = paste("Statistiques de carrière de", joueur$Player))
      )
    
    plot_obj
  })
  
  # Générer la carte dans l'onglet "Carte"
  output$map <- renderLeaflet({
    # Création des icônes personnalisées à partir des logos
    icons <- makeIcon(
      iconUrl = loca_club$logo,
      iconWidth = 50,  # ajustement icône
      iconHeight = 50
    )
    
    leaflet(loca_club) %>%
      addProviderTiles("OpenStreetMap")%>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        icon = icons,  # Utilisation des icônes personnalisées
        label = ~paste(texte)  # Tooltip au survol
      )
  })
  
  # Sélectionner un joueur aléatoire
  target_player <- reactiveVal(base_jeux %>% slice_sample(n = 1))
  
  # Historique des réponses de l'utilisateur
  history <- reactiveVal(data.frame(Nom = character(0), `Équipe actuelle` = character(0),
                                    `Début de carrière` = numeric(0), `Points en carrière` = numeric(0),
                                    Position = character(0), `Titres gagnés` = numeric(0),
                                    Résultat = character(0),
                                    Essai = numeric(0),  # Nouvelle colonne pour le compteur d'essais
                                    stringsAsFactors = FALSE))
  
  # Compteur d'échecs
  failure_count <- reactiveVal(0)
  
  # Gérer la soumission des réponses
  observeEvent(input$submit, {
    guessed_player <- base_jeux %>% filter(tolower(Nom) == tolower(input$player_name_guess))
    
    if (nrow(guessed_player) == 0) {
      output$results <- renderTable({ data.frame(Message = "Le Joueur que vous avez cherché est introuvable !") })
      output$hint <- renderText("")
    } else {
      # Vérifier si le joueur a déjà été deviné
      current_history <- history()
      if (any(tolower(current_history$Nom) == tolower(guessed_player$Nom))) {
        output$results <- renderTable({ data.frame(Message = "Ce n'est pas ce joueur.") })
        output$hint <- renderText("Veuillez essayer un autre joueur.")
      } else {
        comparison <- compare_players(guessed_player, target_player())
        output$results <- renderTable(comparison)
        
        # Fournir un indice si le joueur est incorrect
        output$hint <- renderText({
          if (guessed_player$Nom == target_player()$Nom) {
            "🎉 Félicitations ! Vous avez deviné correctement !"
          } else {
            "Continuez ! Regardez les couleurs pour affiner votre choix."
          }
        })
        
        # Ajouter la réponse à l'historique avec des couleurs et le compteur d'essais
        new_entry <- data.frame(
          Nom = guessed_player$Nom,
          `Équipe actuelle` = guessed_player$`Équipe actuelle`,
          `Début de carrière` = guessed_player$`Début de carrière`,
          `Points en carrière` = guessed_player$`Points en carrière`,
          Position = guessed_player$Position,
          `Titres gagnés` = guessed_player$`Titres gagnés`,
          Résultat = paste(comparison$Résultat, collapse = " | "),
          Essai = failure_count() + 1,  # Ajouter le compteur d'essais
          stringsAsFactors = FALSE
        )
        history(rbind(current_history, new_entry))
        
        # Incrémenter le compteur d'échecs si la réponse est incorrecte
        if (guessed_player$Nom != target_player()$Nom) {
          failure_count(failure_count() + 1)
        }
        
        # Vérifier si le compteur d'échecs a atteint 10
        if (failure_count() >= 10) {
          output$results <- renderTable({
            data.frame(
              Message = "Vous avez atteint le nombre maximum d'échecs.",
              `Nom du joueur` = target_player()$Nom,
              `Équipe actuelle` = target_player()$`Équipe actuelle`,
              `Début de carrière` = target_player()$`Début de carrière`,
              `Points en carrière` = target_player()$`Points en carrière`,
              Position = target_player()$Position,
              `Titres gagnés` = target_player()$`Titres gagnés`
            )
          })
          output$hint <- renderText("Le jeu est terminé. Réessayez !")
        }
      }
    }
  })
  
  # Afficher le nombre de tentatives restantes
  output$attempts_left <- renderText({
    paste("Tentatives restantes :", 10 - failure_count())
  })
  
  # Afficher l'historique avec couleurs et le compteur d'essais
  output$history_table <- renderUI({
    history_data <- history()
    
    # Appliquer des styles de couleur conditionnels
    result_colored <- sapply(history_data$Résultat, function(res) {
      if (grepl("✔️ Correct", res)) {
        paste('<span style="color:green;">', res, '</span>', sep="")
      } else if (grepl("🟡 Proche", res)) {
        paste('<span style="color:orange;">', res, '</span>', sep="")
      } else if (grepl("🔴 Trop bas", res) | grepl("🟠 Trop élevé", res)) {
        paste('<span style="color:red;">', res, '</span>', sep="")
      } else {
        res
      }
    })
    
    # Retourner l'historique avec les styles appliqués et le compteur d'essais
    HTML(paste(
      "<table class='table'><thead><tr><th>Nom</th><th>Équipe actuelle</th><th>Début de carrière</th><th>Points en carrière</th><th>Position</th><th>Titres gagnés</th><th>Résultat</th><th>Essai</th></tr></thead><tbody>",
      paste0("<tr><td>", history_data$Nom, "</td><td>", history_data$`Équipe.actuelle`, "</td><td>",
             history_data$`Début.de.carrière`, "</td><td>", history_data$`Points.en.carrière`, "</td><td>",
             history_data$Position, "</td><td>", history_data$`Titres.gagnés`, "</td><td>", result_colored, "</td><td>",
             history_data$Essai, "</td></tr>",  # Ajouter la colonne Essai
             collapse = ""),
      "</tbody></table>"
    ))
  })
  
  output$comparison_plot <- renderPlot({
    selected_players <- input$selected_players_comparison
    selected_variable <- input$selected_stat_comparison
    
    # Filtrer les données pour ne garder que les joueurs sélectionnés
    plot_data <- nba_base_complete %>%
      filter(Player %in% selected_players) %>%
      arrange(Player, Season)
    
    # Créer le graphique avec les traces pour chaque joueur sélectionné
    ggplot(plot_data, aes_string(x = "Season", y = selected_variable, group = "Player", color = "Player")) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = color_palette) +  # Appliquer la palette de couleurs
      theme_minimal() +
      labs(title = paste("Évolution de", selected_variable, "pour les joueurs sélectionnés"),
           x = "Saison",
           y = selected_variable) +
      theme(legend.position = "bottom")
  })
  
  
  output$radarPlot <- renderPlotly({
    selected_players <- input$selected_players_comparison
    
    # Filtrer les données pour ne garder que les joueurs sélectionnés
    plot_data <- players_long %>%
      filter(Player %in% selected_players)
    
    # Créer le graphique avec les traces pour chaque joueur sélectionné
    plot_obj <- plot_ly(type = 'scatterpolar', mode = 'lines')
    
    # Ajouter une trace pour chaque joueur sélectionné
    for (i in seq_along(selected_players)) {
      player <- selected_players[i]
      player_data <- plot_data %>%
        filter(Player == player)
      
      # Ajouter le premier point à la fin pour fermer le polygone
      r_values <- c(player_data$Value, player_data$Value[1])
      theta_values <- c(player_data$Stat, player_data$Stat[1])
      
      plot_obj <- plot_obj %>%
        add_trace(
          r = r_values,
          theta = theta_values,
          name = player,
          fill = 'none',  # Empêche le remplissage
          line = list(color = color_palette[i], width = 2)  # Appliquer la palette de couleurs
        )
    }
    
    # Appliquer le layout uniquement si des traces ont été ajoutées
    if (length(selected_players) > 0) {
      plot_obj <- plot_obj %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 1))
          ),
          showlegend = TRUE
        )
    }
    
    plot_obj
  })
  
  
  output$densityPlot <- renderPlotly({
    selected_stat <- input$selected_stat_comparison
    
    # Vérifier que la statistique sélectionnée existe et n'a pas que des NA
    if (all(is.na(joueurs_unique_moy[[selected_stat]]))) {
      return(NULL)  # Évite une erreur si la colonne est vide
    }
    
    # Filtrer les données et retirer les NA
    filtered_data <- joueurs_unique_moy %>%
      filter(!is.na(.data[[selected_stat]]))
    
    # Détecter si la statistique est un pourcentage (ex : Win_Perc_alltime)
    is_percentage <- selected_stat %in% c("Win_Perc_alltime", "3P_Perc_alltime")
    
    # Générer le graphique
    p <- ggplot(filtered_data, aes_string(x = selected_stat)) +
      geom_density(color = "black", fill = "gray", alpha = 0.4) +
      geom_vline(data = filtered_data %>% filter(Player %in% input$selected_players_comparison),
                 aes_string(xintercept = selected_stat, color = "Player"),
                 linetype = "dashed", size = 1) +
      theme_minimal()
    
    # Appliquer un format d'affichage adapté
    if (is_percentage) {
      p <- p + scale_x_continuous(labels = percent_format(accuracy = 1))
    } else {
      p <- p + scale_x_continuous(labels = comma_format())  # Format normal
    }
    
    ggplotly(p)
  })
  
  # Liste des équipes uniques
  teams_data <- nba_base_equipe %>%
    select(Equipe, Conf, Div, logo, Champion_Nba_Tot) %>%
    distinct()
  
  # Remplissage du menu déroulant avec les équipes disponibles
  observe({
    updateSelectInput(session, "selected_team_map", choices = teams_data$Equipe)
  })
  
  # Gérer le zoom sur la carte lorsqu'une équipe est sélectionnée
  observeEvent(input$selected_team_map, {
    team <- input$selected_team_map
    if (!is.null(team)) {
      selected_team_data <- loca_club %>%
        filter(Equipe == team)
      
      if (nrow(selected_team_data) > 0) {
        leafletProxy("map") %>%
          setView(lng = selected_team_data$Longitude,
                  lat = selected_team_data$Latitude,
                  zoom = 10)  # Zoom sur l'emplacement de l'équipe
      }
    }
  })
  
  # Générer la carte dans l'onglet "Carte"
  output$map <- renderLeaflet({
    # Création des icônes personnalisées à partir des logos
    icons <- makeIcon(
      iconUrl = loca_club$logo,
      iconWidth = 50,  # ajustement icône
      iconHeight = 50
    )
    
    leaflet(loca_club) %>%
      addProviderTiles("Stadia.OSMBright") %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        icon = icons,  # Utilisation des icônes personnalisées
        label = ~paste(texte)  # Tooltip au survol
      )
  })
  
}

# Lancer l'application
shinyApp(ui, server)