# Module UI pour la page d'accueil
AccueilModuleUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    jumbotron(
      title = "Bienvenue sur cette application de régression logistique multinomiale!",
      status = "info",
      lead = "Cette application a été créée pour effectuer la régression logistique multinomiale sur les données.",
      "Cette application implémente la régression logistique multinomiale en prenant en compte des données mixtes.
          Le package est disponible sur CRAN et sur GitHub. Vous pouvez l'installer en utilisant la commande suivante :
          \t`install.packages('multiReglogisc')`.\nElle a été implémentée en utilisant la descente de gradient.",
      btnName = "Documentation",
      btnLink = "https://cran.r-project.org/web/packages/multiReglogisc/multiReglogisc.pdf"
    ),
    # User Boxes Section
    fluidRow(
      userBox(
        collapsible = FALSE,
        width = 4.5,
        title = userDescription(
          title = "ADJARO PATOUSSI Edina",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "purple",
        "Contact : adjaropatoussi@gmail.com"
      ),
      userBox(
        collapsible = FALSE,
        width = 4.5,
        title = userDescription(
          title = "joel",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "info",
        "Contact : john.doe@example.com"
      ),
      userBox(
        collapsible = FALSE,
        width = 4.5,

        title = userDescription(
          title = "Akrem",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "success",
        "Contact : jane.smith@example.com"
      )
    )
  )
}

# Module Server pour la page d'accueil
AccueilModuleServer <- function(input, output, session) {
  # Rien à ajouter ici si tout le contenu est statique
}
