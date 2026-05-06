# Documentation - Module EEIO

Chaque module EEIO vise à obtenir la partie domestique des empreintes des activités économiques.

La production des empreintes s'appuie sur la fonction compute_ghg_fpt.
Description des paramètres :
  - eeio_country :            code pays EEIO, en ISO2 (Code FIGARO)
  - z :                       matrice des entrées intermédiaires
  - main_aggregates :         agrégats économiques, table avec les colonnes code_eeio_industry, year, unit, x, p2, va
  - emissions_data :          code_eeio_industry, emissions
  - correspondences_figaro :  lien entre la nomenclature EEIO et la nomenclature FIGARO (pour étalonnage FIGARO)
  - year_i :                  année pour le calcul

Pour chaque module les éléments nécessaires :
  - métadonnées relatives à la nomenclature des activités économiques du modèle EEIO
  - table de passage entre la NACE A*732 et la nomenclature du modèle EEIO

Flags :
  "p"   pluriel/partiel
  "na"  non applicable

## Accuracy mapping

|--------|----------------------------------|-------------|
| Niveau | Description                      | Pondération |
|--------|----------------------------------|-------------|
| "1"    | Association exacte/très proche   | 5           |
| "2"    | Assocation proche                | 3           |
| "3"    | Granularité supérieure à FIGARO  | 2           |
| "4"    | Granularité équivalente à FIGARO | 1           |
| "5"    | Granularité inférieure à FIGARO  | NULL        |
|--------|----------------------------------|-------------|
