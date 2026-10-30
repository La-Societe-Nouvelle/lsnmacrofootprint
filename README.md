# lsnmacrofootprint

Scripts R de construction des comptes d'impacts directs observes, tendanciels
et cibles, puis des empreintes macroeconomiques a partir de FIGARO.
Le projet s'utilise avec `source()` depuis sa racine; il ne s'installe pas comme
un package R. Le chargement a ete verifie avec R 4.4.1.

## Installation

Depuis R ou RStudio, definir le dossier de travail:

Sur un nouveau poste, regler d'abord `install_missing_packages <- TRUE` dans
`config/config.R` pour installer les dependances manquantes au chargement.

```r
setwd("path/to/lsnmacrofootprint")
source("scripts/setup.R", encoding = "UTF-8")
```

`scripts/setup.R` contient la liste des dependances, leur verification et leur
installation. Les packages CRAN incluent `readsdmx`, `Rilostat` et
`rvest`. Si `do_clean_outliers <- TRUE` dans `config/config.R`, `fbi` est aussi
requis et installe depuis [son depot GitHub](https://github.com/cykbennie/fbi),
avec `remotes` installe au besoin. Avec `FALSE`, cette dependance est ignoree.

Modifier la configuration **avant** de charger les scripts. Le setup relit ce
fichier a chaque chargement. `install_missing_packages <- TRUE` y autorise
l'installation automatique; avec `FALSE`, le setup signale les packages
manquants sans les installer.

## Premier calcul

Preparer les dossiers et telecharger les fichiers FIGARO publics avant de
construire les comptes. Ce premier exemple utilise uniquement l'annee 2024 et
l'indicateur ECO, qui repose sur les agregats FIGARO:

```r
# Apres le setup, depuis la racine du projet.
for (path in c(download_dir, output_dir, figaro_data_dir, "data_temp")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
load_figaro_data_from_remote(years = 2024, verbose = TRUE)

accounts <- build_eco_obs_accounts(
  years = 2024,
  write_output = TRUE,
  verbose = TRUE
)
footprints <- build_footprints(serie_id = "eco_obs", verbose = TRUE)
```

Cet exemple ecrit `data_output/accounts_obs_eco.csv` pour 2024 et retourne les
empreintes en memoire. Utiliser le dossier FIGARO par defaut `data_figaro`:
plusieurs lecteurs ont encore ce chemin fixe.

## Production complete

Dans `config/config.R`, choisir les indicateurs et conserver
`write_output <- TRUE`: tendances, cibles et empreintes lisent les CSV produits
par les etapes precedentes. Preparer ensuite tous les fichiers necessaires:

```r
load_figaro_data_from_remote(years = 2010:2030, verbose = TRUE)
source("scripts/main.R", encoding = "UTF-8")
```

Les comptes sont enregistres dans `output_dir` (par defaut `data_output`) sous
`accounts_<obs|trd|tgt>_<indic>.csv`, les empreintes sous
`footprints_<obs|trd|tgt>_<indic>.csv`. Les autres sources sont chargees par les
builders et peuvent demander des telechargements supplementaires.

Les exports PostgreSQL sont des commandes `upload_*()` distinctes. Ils
**remplacent les series presentes dans les CSV** apres suppression de leurs
anciennes lignes en base. Aucun calcul ne declenche d'export implicite.

Les [parametres, workflows et exports](Documentation.md) sont decrits dans la
documentation detaillee, avec les conventions d'unites et la desagregation.

## Licence

Le code est distribue sous [EUPL-1.2](LICENSE), dont le
[texte officiel](https://interoperable-europe.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
est fourni dans le depot. Les sources de donnees et ressources tierces gardent
leurs conditions de reutilisation et leurs attributions propres.

## Credits / Sources

L'estimation des comptes GHG observes au-dela de la couverture d'Eurostat
(`obs_accounts/ghg/extended-eurostat-ghg-accounts/ghg_accounts_extensions.R`)
reprend la methodologie
developpee par Eurostat pour les empreintes GES FIGARO, telle que repliquee et
validee dans le projet [InseeFrLab/global-ghg-emissions](https://github.com/InseeFrLab/global-ghg-emissions)
(Sylvain Larrieu, Insee) :

- **Methodologie source** : Eurostat, *FIGARO – Greenhouse gas emission
  estimates, Methodological note*, 18 novembre 2024. Document inclus dans le
  depot InseeFrLab ci-dessus (`documentation/Methodological_Note_GHG_estimates_FIGARO.pdf`).
- **Replication et validation initiales** : Sylvain Larrieu (Insee), projet
  InseeFrLab/global-ghg-emissions.

Sources de donnees utilisees par
`extended-eurostat-ghg-accounts/ghg_accounts_extensions.R` : Eurostat (comptes
d'emissions atmospheriques `env_ac_ainah_r2`, comptes physiques de flux
d'energie `env_ac_pefasu`), [EDGAR](https://edgar.jrc.ec.europa.eu/) (JRC,
Commission europeenne), l'OCDE (emissions CO2 du transport aerien et maritime),
l'ONS (comptes d'emissions du Royaume-Uni) et l'OFS (comptes d'emissions de la
Suisse).

> **A noter** : pour les pays hors UE, EDGAR reprend des donnees IEA
> (International Energy Agency), modifiees par le JRC, sous licence
> **CC BY-NC-ND 4.0** (usage non commercial uniquement). Toute utilisation
> commerciale de ces donnees pour ces pays necessite un accord prealable de
> l'IEA (compliance@iea.org).

