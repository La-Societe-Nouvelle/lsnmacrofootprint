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
| `write_output` | Ecrit les CSV de resultats locaux (defaut: `TRUE`), sans export en base. |
| `do_clean_outliers` | Active le nettoyage des valeurs atypiques dans les tendances. |
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
  write_output = TRUE,
  verbose = TRUE
)

update_trd_accounts(
  indics = default_indics,
  do_clean_outliers = do_clean_outliers,
  write_output = TRUE,
  verbose = TRUE
)

update_tgt_accounts(
  indics = default_tgt_indics,
  write_output = TRUE,
  verbose = TRUE
)

update_footprints(
  indics = default_indics,
  write_output = TRUE,
  verbose = TRUE
)
```

Les builders de comptes et l'orchestrateur des footprints ecrivent les fichiers
dans `output_dir` (par defaut `data_output`):

- `accounts_obs_<indic>.csv`
- `accounts_trd_<indic>.csv`
- `accounts_tgt_<indic>.csv`
- `footprints_<serie>_<indic>.csv`

`write_output` remplace l'ancien argument `do_update`. Avec `FALSE`, les calculs
sont effectues sans ecrire ni remplacer les CSV de resultats. Les caches de
sources et de modeles peuvent toujours etre ecrits. Les etapes suivantes lisent
les CSV existants: conserver `TRUE` pour enchainer une production complete avec
les nouveaux comptes. Pour un calcul isole sans sauvegarde, les fichiers amont
doivent deja exister.

Le nettoyage est transmis a `update_trd_accounts()` et `build_trd_accounts()`;
avec `do_clean_outliers = FALSE`, `clean_outliers()` n'est pas appelee. Pour
charger le setup sans la dependance `fbi`, regler aussi cette option a `FALSE`
dans `config/config.R`.

Pour calculer une seule empreinte en memoire:

```r
footprints <- build_footprints(serie_id = "ghg_obs", verbose = TRUE)
```

`update_disaggregated_footprints(write_output = FALSE)` desactive la sauvegarde
du resultat final `nace_a732_fpt.csv`; les CSV intermediaires EEIO restent
necessaires a la compilation. Aucun de ces lancements n'exporte en PostgreSQL.

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

