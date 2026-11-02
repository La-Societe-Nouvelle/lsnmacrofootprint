# Documentation

Ce document decrit l'organisation actuelle du bac a sable
`lsnmacrofootprint`. Le depot contient des scripts R modifiables librement par
des statisticiens; il n'est pas structure comme un package R formel.

## Demarrage

Depuis la racine du projet:

Sur un nouveau poste, regler `install_missing_packages <- TRUE` dans
`config/config.R` avant le chargement pour installer les dependances manquantes.

```r
source("scripts/setup.R", encoding = "UTF-8")
```

`scripts/setup.R`:

1. lit `config/config.R`;
2. verifie les packages R necessaires;
3. installe les packages manquants si `install_missing_packages <- TRUE`;
4. charge les packages;
5. source les fonctions du projet.

`scripts/setup.R` regroupe le registre des dependances, leur verification et
les fonctions d'installation.
Les packages CRAN incluent `readsdmx`, `Rilostat` et `rvest`. Avec
`do_clean_outliers <- TRUE`, `fbi` est requis et installe depuis
[`cykbennie/fbi`](https://github.com/cykbennie/fbi) via `remotes`, ajoute si
necessaire. Avec `FALSE`, `fbi` n'est ni installe ni exige. Le setup attache les
packages CRAN; les appels a `fbi` utilisent explicitement son namespace.

Regler `config/config.R` avant de charger le setup, qui relit ce fichier.
Une affectation dans la console avant le setup ne remplace donc pas la
configuration. Le setup installe les dependances manquantes seulement si
`install_missing_packages <- TRUE`; avec `FALSE`, il les signale sans installation.
L'installation verifie ensuite que tous les packages selectionnes sont charges
par `requireNamespace()` et signale ceux qui restent manquants.

`DESCRIPTION` contient les metadonnees d'un projet de scripts (`Type: Project`).
Le depot ne se charge pas avec `library(lsnmacrofootprint)` et ne propose pas
de commande de construction ou d'installation de package.

Preparer les fichiers publics decrits dans [Donnees FIGARO](#donnees-figaro),
puis lancer le pipeline principal:

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
| `figaro_data_dir` | Dossier FIGARO cree par le lancement; plusieurs lecteurs utilisent encore le chemin fixe `data_figaro`. |
| `default_years` | Plage de reference; les builders conservent leurs propres annees par defaut. |
| `default_indics` | Indicateurs traites par defaut. |
| `default_tgt_indics` | Indicateurs pour lesquels une cible est construite. |
| `install_missing_packages` | Si `TRUE`, installe les packages R manquants. |
| `write_output` | Ecrit les CSV de resultats locaux (defaut: `TRUE`), sans export en base. |
| `do_clean_outliers` | Active le nettoyage des outliers dans les tendances. |
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

```text
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
| `unit` | Unite de la valeur. |
| `flag` | Origine ou qualite de la valeur. |
| `lastupdate` | Date de production. |

Les builders disposent d'un argument `verbose` pour afficher les grandes etapes:
chargement des metadata, chargement FIGARO, construction des comptes et donnees
finales.

### Sources et controles des douze indicateurs

Tous les producteurs observes et les controles `check_*_obs_accounts_output_data()` partagent
les chargeurs des fichiers `obs_accounts/<indic>/<indic>_accounts_sources.R`.
Ils appliquent les memes filtres, correspondances et conversions d'unites.
Les caches Eurostat ecrits par ces chargeurs utilisent `TIME_PERIOD` et
`OBS_VALUE`. Les anciens caches `time`/`values` sont convertis en memoire a la
lecture, sans etre reecrits. Les autres formats sont conserves selon la source:
Parquet, CSV large UNEP, CSV ILOSTAT, observations PRODCOM mixtes et fichiers BTS
delimites par des points-virgules.

| Indicateur | Sources partagees |
| --- | --- |
| ART | Stock SIRENE et historique des etablissements (Parquet). |
| ECO | Agregats annuels FIGARO (Parquet). |
| GEQ | Remunerations horaires ILOSTAT par sexe et secteur. |
| GHG | Eurostat, extension EDGAR/AEA et OCDE. |
| HAZ | Quantites et unites de production PRODCOM. |
| IDR | Fichiers BTS de l'Insee et parts de remuneration ILOSTAT par decile. |
| KNW | Depenses ANBERD, couts de formation Eurostat CVT et agregats OCDE STAN. |
| MAT | Extraction domestique UNEP, par categorie de matieres. |
| NRG | Comptes Eurostat et donnees JRC de reference. |
| SOC | Masses salariales URSSAF, dont le champ ESS. |
| WAS | Production de dechets Eurostat et OCDE. |
| WAT | Prelevements d'eau OCDE. |

Les controles lisent uniquement les caches locaux par defaut
(`use_temp_data = TRUE`, `refresh_sources = FALSE`). Un cache absent ou invalide
est signale dans `source_errors`; sa couverture vaut zero. Aucun telechargement
ni calcul d'extension EDGAR n'est lance dans ce mode.

```r
nrg_check <- check_nrg_obs_accounts_output_data(years = 2014:2023)
was_check <- check_was_obs_accounts_output_data(years = seq(2010, 2022, 2))
ghg_check <- check_ghg_obs_accounts_output_data(years = 2010:2024)
knw_check <- check_knw_obs_accounts_output_data(years = 2010:2024)
soc_check <- check_soc_obs_accounts_output_data(years = 2020:2023)

# Rafraichissement explicite des sources pour la plage demandee.
nrg_check <- check_nrg_obs_accounts_output_data(
  years = 2014:2023, refresh_sources = TRUE
)
```

L'ancien appel `use_temp_data = FALSE` demande egalement un rafraichissement.
Les nouveaux fichiers sont controles avant de remplacer le cache existant;
si le telechargement ou la validation echoue, l'erreur figure dans `source_errors`
et l'ancien cache reste disponible pour un prochain appel. Un rafraichissement peut remplacer
la plage d'annees presente dans le cache par celle demandee.

Ces rapports mesurent la disponibilite des donnees chargees avant imputation.
Les sources reparties par NVA sont controlees avant cette repartition: le rapport
ne valide pas ses poids. GEQ et les couts CVT de KNW reprennent la selection des
annees les plus proches appliquee par leurs builders. Le rapport peut donc
montrer une couverture pour une annee utilisant des donnees d'une autre annee.
Le chargeur ILOSTAT IDR conserve les libelles d'annees source de son calcul actuel.
Les conversions monetaires ANBERD sont appliquees apres chargement par le builder;
leur controle ne telecharge pas de taux de change.

`status = "OK"` signifie qu'au moins une unite source exploitable est presente;
les pourcentages donnent la couverture. IDR requiert les quatre deciles utilises
par le calcul; GEQ controle les trois valeurs de remuneration par secteur.
Pour KNW, la synthese exige ANBERD, CVT et les deux agregats STAN. Pour ART et SOC,
la couverture etrangere reste structurellement complete puisque leurs comptes
sont fixes a zero hors France. Le diagnostic JRC de NRG reste charge par son
builder mais son coefficient n'est pas applique au resultat.

Le controle IDR ne verifie pas encore la presence des quatre deciles francais
requis comme reference pour chaque annee : un statut `OK` peut donc preceder un
echec du builder. La synthese KNW verifie les sources separement et ne garantit
pas leur intersection sur les memes cles `(year, country, industry)`.

Les chargeurs et les controles sont charges par `scripts/setup.R` pour les douze
indicateurs. Les calculs de repartition, de trajectoire et d'imputation restent
dans les builders et leurs utilitaires.

Pour GHG, la synthese retient Eurostat, puis l'extension, puis l'OCDE pour chaque
cle `(year, country, industry)`. Le rapport inclut `estimated` pour la couverture
de l'extension et `selected_sources` pour les lignes effectivement retenues.
L'extension conserve son perimetre actuel, limite a 2024. Le chargeur OCDE
reutilise le cache de production `oecd_aea_<debut>_<fin>.csv`, y compris pour un
controle portant sur une partie de sa periode, ou l'ancien `DSD_AEA_GHG.csv`.

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

```text
build_<indic>_tgt_accounts()
```

Indicateurs cibles actuellement geres:

```r
c("GEQ", "GHG", "KNW", "MAT", "NRG", "SOC", "WAS", "WAT")
```

Les sorties sont ecrites dans:

```text
accounts_tgt_<indic>.csv
```

Les coefficients annuels des cibles MAT/WAS (2030) et GEQ/SOC (2050) utilisent
la duree entre la derniere annee observee et l'annee objectif. Par exemple, une
base 2024 et une cible 2030 representent six periodes annuelles, de 2025 a 2030.
La cible WAS vaut 95 % de l'intensite de 2010 (`value / NVA`), puis les intensites
projetees sont reconverties en volumes avec la NVA de chaque annee.

Apres reprise des tendances pour les pays etrangers, les contraintes de
monotonie (`cummin`/`cummax`) et les bornes par rapport a la base s'appliquent
encore a ces pays. Le resultat final peut donc differer de la tendance.

Pour les comptes SOC observes, une masse salariale totale nulle ou negative
donne une part ESS de zero.

Les comptes WAT observes sont exprimes en milliers de metres cubes (`THS_M3`).
`estimate_ghg_from_edgar()` retourne des milliers de tonnes equivalent CO2
(`THS_TCO2E`); le builder GHG multiplie ces valeurs par 1 000 pour produire les
comptes observes en tonnes equivalent CO2 (`TCO2E`).

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
  write_output = TRUE,
  verbose = TRUE
)
update_trd_accounts(
  indics = default_indics,
  do_clean_outliers = do_clean_outliers,
  write_output = TRUE,
  verbose = TRUE
)
```

`write_output` remplace `do_update` et controle uniquement les CSV de resultats.
Les builders de comptes enregistrent eux-memes leurs sorties une seule fois;
`update_footprints()` enregistre les empreintes. Avec `FALSE`, les calculs se
poursuivent sans sauvegarder les resultats. Les caches de sources et de modeles
restent independants de cette option.

Les tendances, cibles et footprints lisent les CSV amont dans `output_dir`:
conserver `write_output = TRUE` pour une production complete utilisant les
nouveaux comptes. Pour un calcul isole avec `FALSE`, ces entrees doivent deja
exister et correspondent aux dernieres sorties enregistrees.

`do_clean_outliers` est transmis uniquement aux tendances. Avec `FALSE`, le
nettoyage FBI est ignore. Regler aussi cette option dans `config/config.R`
avant le setup pour ne pas exiger la dependance `fbi`.

`update_disaggregated_footprints()` transmet `write_output` a la compilation
finale, qui retourne la table et n'ecrit `nace_a732_fpt.csv` que si l'option est
activee. Les CSV intermediaires EEIO restent necessaires a cette compilation.

Les exports PostgreSQL utilisent les commandes distinctes `upload_*()`;
aucun workflow de calcul ne les appelle implicitement.

## Donnees FIGARO

Les donnees FIGARO locales sont chargees depuis `data_figaro`.

Avant la premiere execution, creer les dossiers et utiliser le telechargement
public. Les annees ci-dessous couvrent les valeurs par defaut des builders:

```r
for (path in c(download_dir, output_dir, figaro_data_dir, "data_temp")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
load_figaro_data_from_remote(years = 2010:2030, verbose = TRUE)
```

`load_figaro_data_from_remote()` obtient les liens de telechargement depuis
`https://api.sinese.fr/v2/figarodata/`. Pour chaque annee, il telecharge les
agregats principaux, consommations intermediaires et consommations de capital,
puis le fichier commun `figaro_na_prices.parquet`. Il ecrit dans `data_figaro`
et remplace les fichiers demandes s'ils existent deja. Aucun acces PostgreSQL
n'est necessaire. Un sous-ensemble d'annees suffit pour un calcul isole, comme
l'exemple ECO 2024 du README; les tendances necessitent aussi les annees futures.

Fonctions principales:

| Fonction | Role |
| --- | --- |
| `load_figaro_data_from_remote(years, verbose)` | Telecharge les fichiers FIGARO publics. |
| `load_local_figaro_main_aggregates(year_i)` | Charge les agregats principaux FIGARO. |
| `load_local_figaro_intermediate_inputs(year_i)` | Charge les consommations intermediaires FIGARO. |
| `load_local_figaro_capital_use(year_i)` | Charge les consommations de capital fixe. |

Le telechargement public ci-dessus prepare les fichiers utilises par les
lecteurs et les builders du depot.

## Footprints

La fonction principale est:

```r
build_footprints(serie_id, verbose = FALSE)
```

Exemple:

```r
footprints <- build_footprints(
  serie_id = "ghg_obs",
  verbose = TRUE
)
```

Pour chaque annee, `build_footprints()`:

1. charge les donnees FIGARO;
2. construit les matrices intermediaires;
3. calcule ou recharge l'inverse de Leontief;
4. applique les vecteurs d'impacts directs;
5. produit les footprints par pays, branche et agregat;
6. retourne les footprints en memoire.

Pour calculer et enregistrer les series observees, tendancielles et cibles
disponibles d'un indicateur:

```r
update_footprints(indics = "GHG", write_output = TRUE, verbose = TRUE)
```

L'inverse de Leontief est cachee dans:

```text
data_temp/figaro_inverse_leontief_<year>.parquet
```

Logique de cache:

1. si le fichier n'existe pas, la matrice est calculee puis enregistree;
2. dans tous les cas, la matrice est ensuite rechargee depuis le fichier.

Si les fichiers FIGARO d'une annee ont ete remplaces, supprimer avant le calcul
des empreintes le cache `data_temp/figaro_inverse_leontief_<annee>.parquet`
correspondant pour qu'il soit recalcule. Le remplacement des fichiers FIGARO
n'invalide pas automatiquement ce cache.

Colonnes principales des footprints:

| Colonne | Description |
| --- | --- |
| `serie_id` | Serie source, par exemple `ghg_obs`. |
| `country` | Code pays FIGARO. |
| `industry` | Branche FIGARO ou `TOTAL`. |
| `year` | Annee. |
| `aggregate` | Agregat economique. |
| `value` | Valeur de footprint. |
| `unit` | Unite definie dans `metadata/metadata_indics.csv`. |
| `flag` | Flag de qualite. |
| `lastupdate` | Date de production. |
| `indic` | Indicateur conserve en fin de tableau par le builder, absent des CSV. |

## Upload PostgreSQL

Les identifiants proviennent des variables d'environnement `STATSDB_DATABASE`,
`STATSDB_HOST`, `STATSDB_PORT`, `STATSDB_USER` et `STATSDB_PASSWORD`.

La connexion est creee par:

```r
get_connection_db()
```

Les fonctions d'upload sont dans `db/upload.R` et sont chargees par
`scripts/setup.R`.

```r
upload_accounts_data(output_dir = output_dir, verbose = TRUE)
upload_footprints_data(output_dir = output_dir, verbose = TRUE)
```

| Fonction | Source | Table cible |
| --- | --- | --- |
| `upload_accounts_data()` | `data_output/accounts_*.csv` | `impacts.direct_impacts` |
| `upload_footprints_data()` | `data_output/footprints_*.csv` | `macrodata.macro_fpt` |

`upload_accounts_data()` retire `unit` avant l'insertion.
`upload_footprints_data()` retire `unit` et ajoute `currency = "CPEUR"`.
Chaque fonction selectionne ensuite les colonnes destinees a sa table cible.
Pour chaque `serie_id` present dans les donnees a exporter, les fonctions
suppriment toutes les lignes existantes de cette serie, puis inserent les lignes
des CSV avec `DBI::dbWriteTable(..., append = TRUE)`. Il faut donc fournir des
series completes: un extrait remplace lui aussi toute la serie correspondante.
Les series absentes des fichiers restent en base. La suppression et l'insertion
ne sont pas encore englobees dans une transaction commune (correction CT8).
`upload_footprints_data(series = ...)` permet de selectionner les series a
remplacer. Ces fonctions lisent `data_output` par defaut; passer `output_dir`
explicitement si la configuration utilise un autre dossier.

## Desagregation A*732

`update_disaggregated_footprints(year_i = 2022, write_output = TRUE)` prepare
les modeles EEIO, puis appelle `build_disaggregated_footprints()`. Le resultat
final est enregistre dans `disaggregation/data_temp/nace_a732_fpt.csv`.
Le dossier `disaggregation/data_temp` doit exister avant le lancement.

La compilation utilise les poids suivants selon `accuracy_mapping_a732`:

| Niveau | Poids |
| --- | --- |
| 1 | 8 |
| 2 | 5 |
| 3 | 2 |
| 4 (FIGARO) | 1 |

Les niveaux 5 et suivants sont exclus. L'empreinte agregee et la precision
rapportee utilisent ces memes poids. Les contraintes de millesime restent
propres aux modeles; le traitement conserve notamment une annee de prix 2022.

## Fonctions utiles

| Famille | Fonctions |
| --- | --- |
| Setup | `source("scripts/setup.R")` |
| Configuration | `config/config.R` |
| Observations | `build_<indic>_obs_accounts()` |
| Tendances | `build_trd_accounts()` |
| Cibles | `build_<indic>_tgt_accounts()` |
| Orchestration | `update_obs_accounts()`, `update_trd_accounts()`, `update_tgt_accounts()` |
| Footprints | `build_footprints()` |
| Base | `get_connection_db()`, `upload_accounts_data()`, `upload_footprints_data()` |
| FIGARO | `load_local_figaro_main_aggregates()`, `load_local_figaro_intermediate_inputs()` |
| Nettoyage | `clean_outliers()`, `proxy_missing_value_by_similarity()` |

## Points d'attention

- Les dossiers et chemins du projet sont en minuscules.
- Les scripts supposent un lancement depuis la racine du projet ou via
  `scripts/main.R`, qui repositionne le working directory.
- Les fonctions d'upload remplacent les series selectionnees apres suppression
  de leurs anciennes lignes; fournir des series completes.
- Les gros calculs de footprints dependent fortement du cache FIGARO et du
  cache de l'inverse de Leontief.
