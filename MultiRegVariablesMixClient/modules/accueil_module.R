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
          \t`remotes::install_github(\"https://github.com/akremjomaa/MultiRegVariablesMix.git\")`.\nElle a été implémentée en utilisant la descente de gradient.",
      btnName = "Documentation",
      href = "https://akremjomaa.github.io/MultiRegVariablesMix"

    ),
    # User Boxes Section
    fluidRow(
      userBox(
        collapsible = FALSE,
        width = 4.5,
        title = userDescription(
          title = "Edina Adjaro Patoussi",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "success",
        "Contact :  e.adjaro-patoussi@univ-lyon2.fr"
      ),
      userBox(
        collapsible = FALSE,
        width = 4.5,
        title = userDescription(
          title = "Akrem Jomaa",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "success",
        "Contact : akrem.jomaa@univ-lyon2.fr"
      ),
      userBox(
        collapsible = FALSE,
        width = 4.5,

        title = userDescription(
          title = "Joel Sollari ",
          subtitle = "Data Scientist, Maintainer",
          image = "https://www.gravatar.com/avatar/",
          type = 2
        ),
        status = "success",
        "Contact :  joel.sollari@univ-lyon2.fr"
      )
    )
  )
}

# Module Server pour la page d'accueil
AccueilModuleServer <- function(input, output, session) {
  # Rien à ajouter ici si tout le contenu est statique
}
