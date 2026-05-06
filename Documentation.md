# Documentation

Ce document decrit l'organisation actuelle du bac a sable
`lsnmacrofootprint`. Le depot contient des scripts R modifiables librement par
des statisticiens; il n'est pas structure comme un package R formel.

## Demarrage

Depuis la racine du projet:

```r
source("scripts/setup.R")
```

`scripts/setup.R`:

1. lit `config/config.R`;
2. verifie les packages R necessaires;
3. installe les packages manquants si `install_missing_packages <- TRUE`;
4. charge les packages;
5. source les fonctions du projet.

Pour lancer le pipeline principal:

```r
source("scripts/main.R")
```

`scripts/main.R` construit les comptes directs, puis les footprints.

## Configuration

Les parametres modifiables sont centralises dans `config/config.R`.

| Parametre | Description |
| --- | --- |
| `download_dir` | Dossier de cache et telechargement local. |
| `output_dir` | Dossier des CSV produits par les builders. |
| `figaro_data_dir` | Dossier des fichiers FIGARO Parquet. |
| `default_years` | Annees traitees par defaut. |
| `default_indics` | Indicateurs traites par defaut. |
| `default_tgt_indics` | Indicateurs pour lesquels une cible est construite. |
| `install_missing_packages` | Si `TRUE`, installe les packages R manquants. |
| `do_update` | Active les ecritures en base dans les scripts d'orchestration. |
| `do_clean_outliers` | Active le nettoyage des outliers. |
| `verbose` | Active les logs de progression. |

Les secrets de connexion ne doivent pas etre mis dans `config/config.R`.
`db/stats_db.R` lit les variables d'environnement suivantes:

- `STATSDB_DATABASE`
- `STATSDB_HOST`
- `STATSDB_PORT`
- `STATSDB_USER`
- `STATSDB_PASSWORD`

## Arborescence

| Dossier | Contenu |
| --- | --- |
| `config/` | Configuration locale non sensible. |
| `scripts/` | Setup, main pipeline et orchestration. |
| `obs_accounts/` | Builders des comptes observes. |
| `trd_accounts/` | Construction des comptes tendanciels. |
| `tgt_accounts/` | Construction des trajectoires cibles. |
| `footprints/` | Calcul des empreintes macro-economiques. |
| `db/` | Connexion PostgreSQL et upload. |
| `utils/` | Fonctions communes. |
| `metadata/` | Nomenclatures et tables de correspondance. |
| `disaggregation/` | Travaux EEIO et desagregation. |
| `data_figaro/` | Donnees FIGARO locales au format Parquet. |
| `data_temp/` | Cache local, dont l'inverse de Leontief. |
| `data_output/` | Fichiers CSV produits. |

Tous les chemins sourcees dans les scripts sont en minuscules.

## Comptes observes

Les fonctions de comptes observes suivent le format:

```r
build_<indic>_obs_accounts()
```

Exemples:

```r
build_art_obs_accounts()
build_geq_obs_accounts()
build_ghg_obs_accounts()
```

Les 12 indicateurs observes sont:

```r
c("ART", "ECO", "GEQ", "GHG", "HAZ", "IDR",
  "KNW", "MAT", "NRG", "SOC", "WAS", "WAT")
```

Les sorties sont ecrites dans `data_output` sous la forme:

```text
accounts_obs_<indic>.csv
```

Colonnes attendues:

| Colonne | Description |
| --- | --- |
| `serie_id` | Identifiant de serie, par exemple `geq_obs`. |
| `country` | Code pays FIGARO. |
| `industry` | Branche FIGARO. |
| `year` | Annee. |
| `value` | Valeur de l'indicateur. |
| `flag` | Origine ou qualite de la valeur. |
| `lastupdate` | Date de production. |

Les builders disposent d'un argument `verbose` pour afficher les grandes etapes:
chargement des metadata, chargement FIGARO, construction des comptes et donnees
finales.

## Comptes tendanciels

La fonction principale est:

```r
build_trd_accounts(indic_i)
```

Elle lit les fichiers `accounts_obs_<indic>.csv`, projette les series jusqu'a
2030 et ecrit:

```text
accounts_trd_<indic>.csv
```

Les projections utilisent les helpers dans `trd_accounts/`, notamment les
simulations Monte Carlo, regressions et fallbacks de croissance.

## Comptes cibles

Les fonctions cibles suivent le format:

```r
build_target_<indic>()
```

Indicateurs cibles actuellement geres:

```r
c("GEQ", "GHG", "KNW", "MAT", "NRG", "SOC", "WAS", "WAT")
```

Les sorties sont ecrites dans:

```text
accounts_tgt_<indic>.csv
```

## Orchestration

Les fonctions d'orchestration sont dans `scripts/workflows.R`:

```r
update_obs_accounts()
update_trd_accounts()
update_tgt_accounts()
```

Exemple:

```r
source("scripts/setup.R")

update_obs_accounts(
  indics = default_indics,
  do_clean_outliers = TRUE,
  do_update = FALSE,
  verbose = TRUE
)
```

`do_update = FALSE` permet de produire les fichiers sans ecriture en base.

## Donnees FIGARO

Les donnees FIGARO locales sont chargees depuis `data_figaro`.

Fonctions principales:

| Fonction | Role |
| --- | --- |
| `load_local_figaro_main_aggregates(year_i)` | Charge les agregats principaux FIGARO. |
| `load_local_figaro_intermediate_inputs(year_i)` | Charge les consommations intermediaires FIGARO. |
| `get_figaro_main_aggregates(con, year_i, data_dir)` | Extrait et prepare les agregats depuis PostgreSQL. |
| `get_figaro_intermediate_inputs(con, year_i)` | Extrait les consommations intermediaires. |
| `get_figaro_capital_use(con, year_i)` | Extrait les consommations de capital fixe. |

Le script `utils/utils_figaro_data_files.R` sert a generer les fichiers Parquet
FIGARO locaux. Il n'est pas source automatiquement par `scripts/setup.R`, car il
peut lancer des traitements d'export.

## Footprints

La fonction principale est:

```r
build_footprints(series, verbose = FALSE)
```

Exemple:

```r
build_footprints(
  series = c("ghg_obs", "ghg_trd", "ghg_tgt"),
  verbose = TRUE
)
```

Pour chaque annee, `build_footprints()`:

1. charge les donnees FIGARO;
2. construit les matrices intermediaires;
3. calcule ou recharge l'inverse de Leontief;
4. applique les vecteurs d'impacts directs;
5. produit les footprints par pays, branche et agregat;
6. ecrit les CSV dans `data_output`.

L'inverse de Leontief est cachee dans:

```text
data_temp/figaro_inverse_leontief_<year>.parquet
```

Logique de cache:

1. si le fichier n'existe pas, la matrice est calculee puis enregistree;
2. dans tous les cas, la matrice est ensuite rechargee depuis le fichier.

Colonnes principales des footprints:

| Colonne | Description |
| --- | --- |
| `serie_id` | Serie source, par exemple `ghg_obs`. |
| `indic` | Indicateur. |
| `country` | Code pays FIGARO. |
| `industry` | Branche FIGARO ou `TOTAL`. |
| `year` | Annee. |
| `aggregate` | Agregat economique. |
| `value` | Valeur de footprint. |
| `flag` | Flag de qualite. |
| `lastupdate` | Date de production. |

## Upload PostgreSQL

La connexion est creee par:

```r
get_connection_db()
```

Les fonctions d'upload sont dans `db/upload.R` et sont chargees par
`scripts/setup.R`.

```r
upload_accounts_data(verbose = TRUE)
upload_footprints_data(verbose = TRUE)
```

| Fonction | Source | Table cible |
| --- | --- | --- |
| `upload_accounts_data()` | `data_output/accounts_*.csv` | `impacts.directs_impacts` |
| `upload_footprints_data()` | `data_output/footprints_*.csv` | `macrodata.macro_fpt` |

Les colonnes des fichiers sont supposees identiques aux colonnes des tables.
Les fonctions utilisent `DBI::dbWriteTable(..., append = TRUE)` et ne suppriment
pas les donnees existantes.

## Fonctions utiles

| Famille | Fonctions |
| --- | --- |
| Setup | `source("scripts/setup.R")` |
| Configuration | `config/config.R` |
| Observations | `build_<indic>_obs_accounts()` |
| Tendances | `build_trd_accounts()` |
| Cibles | `build_target_<indic>()` |
| Orchestration | `update_obs_accounts()`, `update_trd_accounts()`, `update_tgt_accounts()` |
| Footprints | `build_footprints()` |
| Base | `get_connection_db()`, `upload_accounts_data()`, `upload_footprints_data()` |
| FIGARO | `load_local_figaro_main_aggregates()`, `load_local_figaro_intermediate_inputs()` |
| Nettoyage | `clean_outliers()`, `proxy_missing_value_by_similarity()` |

## Points d'attention

- Les dossiers et chemins du projet sont en minuscules.
- Les scripts supposent un lancement depuis la racine du projet ou via
  `scripts/main.R`, qui repositionne le working directory.
- Les fonctions d'upload ajoutent les donnees; elles ne font pas de
  remplacement automatique.
- Les gros calculs de footprints dependent fortement du cache FIGARO et du
  cache de l'inverse de Leontief.
