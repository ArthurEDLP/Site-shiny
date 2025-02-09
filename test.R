library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(plotly)
library(readxl)

# Lire les données CSV

loca_club <- read_excel("Données/base_loc (1).xlsx")

base_nba <- read_excel("Données/base_nba_mvp.xlsx")

base_nba <- base_nba |> 
  mutate(across(c(`3P_Perc`), ~ replace_na(.x, 0)))

base_nba$`3P_Perc` <-  as.numeric(base_nba$`3P_Perc`)

base_nba <- base_nba |> 
  mutate(across(c(STL_moy), ~ replace_na(.x, 0)))

base_nba$STL_moy <-  as.numeric(base_nba$STL_moy)

base_nba <- base_nba |> 
  mutate(across(c(BLK_moy), ~ replace_na(.x, 0)))

base_nba$BLK_moy <-  as.numeric(base_nba$BLK_moy)



# Charger la base de données
library(readxl)
nba_data <- read_excel("Données/nba_players.xlsx")


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


# UI : Interface utilisateur ----
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  "LeBron le G.O.A.T !!",
  tabPanel("Acceuil", "Cette application a été créée dans le cadre de la formation RShiny - Initiation."),
  tabPanel(
    "Équipe",
    sidebarLayout(
      sidebarPanel(
        # Ajouter un panneau pour afficher les détails de l'équipe sélectionnée
        uiOutput("team_details_ui")  # Cet élément affichera les détails de l'équipe
      ),
      mainPanel(
        leafletOutput("map", height = "600px")  # Afficher la carte ici
      )
    )
  ),
  tabPanel(
    "Joueurs", "mettre les joueurs ici" # Afficher liste des joueurs ici
    
  ),
  tabPanel(
    "Tableau"
  ),
  tabPanel(
    "Graphique"
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
        selectizeInput("player_name", "Nom du joueur :", choices = nba_data$Nom, options = list(
          placeholder = 'Commencez à taper un nom...',
          maxItems = 1,
          create = FALSE
        )),
        actionButton("submit", "Valider")
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
  )
)

# Serveur : Logique de l'application ----
server <- function(input, output) {
  
  # Générer la carte dans l'onglet "Carte"
  
  output$map <- renderLeaflet({
    # Création des icônes personnalisées à partir des logos
    icons <- makeIcon(
      iconUrl = loca_club$logo,  
      iconWidth = 50,  # ajustement icône
      iconHeight = 50  
    )
    
    leaflet(loca_club) |> 
      addProviderTiles("Stadia.OSMBright") |> 
      addMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        icon = icons,  # Utilisation des icônes personnalisées
        label = ~paste(texte)  # Tooltip au survol
      )
  })
  
  
  
  
  # Sélectionner un joueur aléatoire
  target_player <- reactiveVal(nba_data %>% slice_sample(n = 1))
  
  # Historique des réponses de l'utilisateur
  history <- reactiveVal(data.frame(Nom = character(0), `Équipe actuelle` = character(0), 
                                    `Début de carrière` = numeric(0), `Points en carrière` = numeric(0), 
                                    Position = character(0), `Titres gagnés` = numeric(0),
                                    Résultat = character(0),
                                    stringsAsFactors = FALSE))
  
  # Gérer la soumission des réponses
  observeEvent(input$submit, {
    guessed_player <- nba_data %>% filter(tolower(Nom) == tolower(input$player_name))
    
    if (nrow(guessed_player) == 0) {
      output$results <- renderTable({ data.frame(Message = "Joueur introuvable !") })
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
        
        # Ajouter la réponse à l'historique avec des couleurs
        new_entry <- data.frame(
          Nom = guessed_player$Nom, 
          `Équipe actuelle` = guessed_player$`Équipe actuelle`,
          `Début de carrière` = guessed_player$`Début de carrière`,
          `Points en carrière` = guessed_player$`Points en carrière`,
          Position = guessed_player$Position,
          `Titres gagnés` = guessed_player$`Titres gagnés`,
          Résultat = paste(comparison$Résultat, collapse = " | "),
          stringsAsFactors = FALSE
        )
        history(rbind(current_history, new_entry))
      }
    }
  })
  
  # Afficher l'historique avec couleurs
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
    
    # Retourner l'historique avec les styles appliqués
    HTML(paste(
      "<table class='table'><thead><tr><th>Nom</th><th>Équipe actuelle</th><th>Début de carrière</th><th>Points en carrière</th><th>Position</th><th>Titres gagnés</th><th>Résultat</th></tr></thead><tbody>",
      paste0("<tr><td>", history_data$Nom, "</td><td>", history_data$`Équipe actuelle`, "</td><td>", 
             history_data$`Début de carrière`, "</td><td>", history_data$`Points en carrière`, "</td><td>", 
             history_data$Position, "</td><td>", history_data$`Titres gagnés`, "</td><td>", result_colored, "</td></tr>", 
             collapse = ""),
      "</tbody></table>"
    ))
  })
}
shinyApp(ui = ui, server = server)
