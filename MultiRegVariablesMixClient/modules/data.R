# Exemple de données pour régression logistique multinomiale
data <- data.frame(
  Classe = c("A", "B", "C", "A", "B", "C", "A", "B", "C"),
  Age = c(25, 32, 45, 28, 35, 50, 23, 38, 55),
  Salaire = c(30000, 45000, 52000, 32000, 47000, 60000, 29000, 48000, 70000),
  Ville = c("Paris", "Lyon", "Marseille", "Paris", "Lyon", "Marseille", "Bordeaux", "Nantes", "Toulouse"),
  Expérience = c(2, 5, 10, 3, 6, 15, 1, 8, 20)
)

# Sauvegarde en CSV
write.csv(data, "multinomial_data.csv", row.names = FALSE)

