# lsnmacrofootprint

`lsnmacrofootprint` est un bac a sable de scripts R pour construire des donnees
macro-sectorielles d'impacts directs, produire des trajectoires tendancielles et
cibles, puis calculer des empreintes macro-economiques a partir des tables
FIGARO.

Le depot n'est pas un package R installe via `library()`. L'usage recommande est
de travailler depuis la racine du projet et de charger les fonctions avec
`source("scripts/setup.R")`.

## Fonctionnalites

- Construction des comptes observes `*_obs` pour 12 indicateurs: `ART`, `ECO`,
  `GEQ`, `GHG`, `HAZ`, `IDR`, `KNW`, `MAT`, `NRG`, `SOC`, `WAS`, `WAT`.
- Projection des comptes tendanciels `*_trd` jusqu'a 2030.
- Construction de trajectoires cibles `*_tgt` pour les indicateurs disponibles.
- Calcul des footprints macro-economiques par pays, branche FIGARO et agregat.
- Cache local des donnees FIGARO au format Parquet.
- Cache local de l'inverse de Leontief dans `data_temp`.
- Upload optionnel des fichiers produits vers PostgreSQL.

## Structure

| Chemin | Role |
| --- | --- |
| `scripts/setup.R` | Charge les packages, la configuration et les fonctions du projet. |
| `scripts/main.R` | Lance le pipeline principal: accounts puis footprints. |
| `scripts/workflows.R` | Orchestre la generation des comptes observes, tendances et cibles. |
| `config/config.R` | Parametres locaux non sensibles: dossiers, indicateurs, options. |
| `obs_accounts/` | Builders des comptes observes par indicateur. |
| `trd_accounts/` | Projection des comptes tendanciels. |
| `tgt_accounts/` | Builders des trajectoires cibles. |
| `footprints/` | Calcul des empreintes macro-economiques. |
| `db/` | Connexion PostgreSQL et fonctions d'upload. |
| `utils/` | Fonctions communes: FIGARO, outliers, proxies, conversions. |
| `metadata/` | Nomenclatures et fichiers de correspondance. |
| `data_figaro/` | Donnees FIGARO locales au format Parquet. |
| `data_temp/` | Cache local, dont les inverses de Leontief. |
| `data_output/` | CSV produits par les builders. |

## Installation

Depuis R ou RStudio:

```r
setwd("path/to/lsnmacrofootprint")
source("scripts/install_dependencies.R")
source("scripts/setup.R")
```

Si un package manque, deux options:

```r
# Option 1: installer manuellement les packages signales
install.packages(c("arrow", "DBI", "RPostgres"))

# Option 2: autoriser l'installation automatique dans config/config.R
install_missing_packages <- TRUE
```

Les identifiants PostgreSQL sont lus depuis les variables d'environnement:

- `STATSDB_DATABASE`
- `STATSDB_HOST`
- `STATSDB_PORT`
- `STATSDB_USER`
- `STATSDB_PASSWORD`

## Configuration

Les principaux parametres sont dans `config/config.R`:

| Parametre | Role |
| --- | --- |
| `download_dir` | Dossier de telechargements/cache local. |
| `output_dir` | Dossier des CSV produits. |
| `figaro_data_dir` | Dossier des fichiers FIGARO Parquet. |
| `default_years` | Annees traitees par defaut. |
| `default_indics` | Indicateurs traites par defaut. |
| `default_tgt_indics` | Indicateurs avec trajectoires cibles. |
| `install_missing_packages` | Installe les packages R manquants si `TRUE`. |
| `do_update` | Active les ecritures en base dans les scripts d'orchestration. |
| `do_clean_outliers` | Active le nettoyage des valeurs atypiques. |
| `verbose` | Affiche les logs de progression. |

## Utilisation

Pipeline complet:

```r
source("scripts/main.R")
```

Execution plus controlee:

```r
source("scripts/setup.R")

update_obs_accounts(
  indics = default_indics,
  do_clean_outliers = do_clean_outliers,
  do_update = FALSE,
  verbose = TRUE
)

update_trd_accounts(
  indics = default_indics,
  do_update = FALSE,
  verbose = TRUE
)

update_tgt_accounts(
  indics = default_tgt_indics,
  do_update = FALSE,
  verbose = TRUE
)

build_footprints(
  series = c("ghg_obs", "ghg_trd", "ghg_tgt"),
  verbose = TRUE
)
```

Les builders ecrivent les fichiers dans `data_output`:

- `accounts_obs_<indic>.csv`
- `accounts_trd_<indic>.csv`
- `accounts_tgt_<indic>.csv`
- `footprints_<serie>_<indic>.csv`

## Upload en base

Les fonctions d'upload lisent les CSV deja produits dans `data_output` et les
inserent en base. Elles ajoutent les lignes sans supprimer l'existant.

```r
source("scripts/setup.R")

upload_accounts_data(verbose = TRUE)
upload_footprints_data(verbose = TRUE)
```

Tables ciblees:

| Fonction | Fichiers lus | Table cible |
| --- | --- | --- |
| `upload_accounts_data()` | `accounts_*.csv` | `impacts.directs_impacts` |
| `upload_footprints_data()` | `footprints_*.csv` | `macrodata.macro_fpt` |

## Notes techniques

- `build_footprints()` recharge les donnees FIGARO depuis `data_figaro`.
- L'inverse de Leontief est cachee dans
  `data_temp/figaro_inverse_leontief_<year>.parquet`.
- Si le fichier de cache existe, il est recharge au lieu de recalculer
  l'inverse.

## Licence

La licence est indiquee dans `DESCRIPTION`.
