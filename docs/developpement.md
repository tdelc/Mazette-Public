# Développer le dashboard

Pour qui modifie le code : comment l'application est construite, comment les
tables se fabriquent, et les règles qui évitent les pièges déjà rencontrés.
Le détail d'une fonction se lit dans son commentaire d'en-tête ; ce document
donne la carte.

- [Architecture](#architecture)
- [De la source au cache : comment les tables se font](#de-la-source-au-cache--comment-les-tables-se-font)
- [Les tables](#les-tables)
- [La comptabilité](#la-comptabilité)
- [Le coût du travail](#le-coût-du-travail)
- [Conventions](#conventions)
- [Pièges connus](#pièges-connus)
- [Tests](#tests)
- [Ajouter quelque chose](#ajouter-quelque-chose)

---

## Architecture

R / Shiny + bslib, sans build ni JavaScript compilé.

```
global.R   charge les paquets, source TOUT R/ (ordre alphabétique),
           puis définit les constantes partagées (couleurs, jours, mois…)
import.R   lit les sources et construit les tables DB_*
ui.R       la coquille, et une fonction ui_*() par onglet
server.R   charge le cache (une fois par processus), puis une section
           par onglet, dans l'ordre de la barre
R/         toute la logique : un fichier par volet, plus un socle commun
```

**Les données ne sont jamais calculées dans le serveur.** L'import produit les
tables `DB_*`, le serveur les lit et les met en forme. Un chiffre faux se
corrige dans `import.R` ou dans `R/`, jamais dans un `render*`.

### Le chargement

`connexion_ou_creation()` (`R/connect.R`) prend **le cache le plus récent**,
local (`outputs/R_new_env_<date>.RData`) ou sur Drive, quelle que soit sa
date ; il ne refait l'import que s'il n'existe aucun cache. La date du nom est
celle du dernier jour de ventes, affichée dans la barre (`R/fraicheur.R`).

Après chaque chargement, `finalise_donnees()` reconstruit `DB_TICKET` complet
(`hydrate_dans()`) et normalise les droits (`normalise_password()`).

Les données vivent dans l'environnement de `server.R`, hors de la fonction
serveur : elles sont partagées par toutes les sessions. Après une mise à
jour, `VERSION_DONNEES` (un `reactiveVal` partagé) avertit les écrans ouverts.

### Les accès, et les onglets insérés après connexion

`R/acces.R` déclare le catalogue `ONGLETS` (clé, titre, icône, nom du
constructeur `ui_*()`) et les `PROFILS`. Seul l'onglet de départ
(`ONGLET_DEPART`) part avec la page ; après la connexion, le serveur insère
les onglets autorisés (`nav_insert`). Un onglet interdit n'est donc jamais
envoyé au navigateur.

`ui.R` et `server.R` s'évaluent dans deux environnements frères : les
constructeurs `ui_*()` ne sont pas visibles du serveur. `ui.R` les publie avec
`enregistre_constructeurs_onglets()`. Tout ce qui sert à l'interface **et** au
serveur va dans `R/` ou `global.R`, jamais dans `ui.R`.

### Les fichiers de `R/`

| Fichier | Rôle |
|---|---|
| `acces.R` | onglets, profils, droits, constructeurs d'onglets |
| `connect.R` | quel cache charger, et `TABLES_DASHBOARD` / `TABLES_OPTIONNELLES` |
| `fraicheur.R`, `import_distant.R` | la date des données, et le déclenchement de l'import sur GitHub |
| `donnees.R` | stockage réduit des tickets, nomenclature et TVA des produits |
| `date.R` | calendrier de référence, `parse_date_souple()` |
| `helpers.R`, `theme.R`, `kpi.R`, `box.R`, `objectifs.R` | formats, tuiles, couleurs d'objectif, zones d'alerte, tri des tableaux |
| `maintenant.R`, `details.R`, `historique.R`, `annee.R`, `comparaison.R` | les volets de ventes |
| `boissons.R`, `brassins.R`, `consommation.R`, `produits.R`, `focaccia.R`, `pizzwanze.R` | boissons, fûts, nourriture |
| `simulation.R`, `reservations.R` | les volets du même nom |
| `travail.R`, `sources_travail.R`, `planning.R` | heures, coût du travail, planning |
| `plan_comptable.R`, `exploitation.R`, `analyse.R`, `ecritures.R`, `projection.R`, `validation.R`, `couts.R` | le volet Compta |
| `odoo.R`, `odoo_api.R` | des écritures Odoo à `DB_COMPTA` |

---

## De la source au cache : comment les tables se font

`import.R` s'exécute dans un environnement dédié (`refait_import()`, seul
endroit qui écrit un cache), dans cet ordre :

1. **Authentification Google** avec le compte de service (`LINK_JSON`), puis
   lecture du classeur d'index `PATHS` (`ID_DRIVE_PATHS`) : chaque source y
   est désignée par une clé (`get_path("ID_DRIVE_MAZETTE")`…).
2. **Lecture des sources** : le classeur principal (ventes, tickets, brassins,
   objectifs, mots de passe), les heures Horeko, l'ancienne comptabilité
   (`DB_COMPTA.rds`), puis Odoo (API ou export), les réservations (Google
   Sheet + calendriers iCal), le planning (iCal, archivé dans un Sheet), la
   paie, le journal des validations, et les archives de caisse 2023-2025.
3. **Premiers traitements** : chaque onglet brut devient une table typée
   (`DB_JOURS`, `DB_BRASSINS`, `DB_PRODUITS`, `DB_TICKET`, `DB_OBJECTIFS`,
   `DB_PASSWORD`, `DB_HEURES`, `DB_RESA`). Toute date passe par
   `parse_date_souple()`.
4. **Nomenclature des produits** : un libellé canonique par `ID_PRODUIT`, son
   taux de TVA et sa catégorie, les remises rattachées au produit remisé,
   puis le contrôle de ce qui reste sans TVA (`R/donnees.R`).
5. **Tables finales**, décrites ci-dessous.
6. **Sauvegarde** : `refait_import()` vérifie que toutes les tables de
   `TABLES_DASHBOARD` existent (sinon l'import échoue), enregistre celles-ci
   et les `TABLES_OPTIONNELLES` présentes dans un `.RData` compressé, en local
   et sur Drive (`PATH_DRIVE`).

Une source facultative qui manque ou qui échoue (Odoo, paie, planning,
validations) est signalée dans la trace et n'arrête pas l'import.

---

## Les tables

### Enregistrées à chaque fois (`TABLES_DASHBOARD`)

| Table | Contenu | Fabrication |
|---|---|---|
| `DB_JOURS` | CA HTVA / TVAC par jour | onglet `DB JOURS` + `IMPORT OLD DATA` + archives |
| `DB_KPI_SIMPLE` | CA par jour, réparti midi/soir, semaine/week-end, boisson/nourriture | répartitions tirées des tickets, recalées par règle de trois sur le CA de `DB_JOURS` (qui inclut les factures) |
| `DB_OBJECTIFS` | objectif par jour | objectifs mensuels (un onglet par année, `SHEETS_OBJECTIFS`) répartis selon le poids de chaque jour (`calculer_objectifs_journaliers()`) |
| `DB_TICKET` | chaque ligne de caisse, forme **réduite** | voir ci-dessous |
| `REF_PRODUITS` | le référentiel des lignes de caisse : produit, catégorie, TVA, volume | idem |
| `DB_PRODUITS` | ventes par jour × produit, avec le brassin rattaché | export Lightspeed, joint aux brassins actifs ce jour-là |
| `DB_BRASSINS` | référentiel des brassins | onglet `IMPORT BRASSINS` + correspondance bières |
| `DB_BIERES` | volumes vendus cumulés par brassin, pour le suivi des fûts | `DB_PRODUITS` (bières) × calendrier ; un fût sans vente depuis 10 jours est réputé fini |
| `DB_COMPTA` | comptabilité mensuelle par compte | voir [La comptabilité](#la-comptabilité) |
| `DB_COUTS_TRAVAIL` | heures et coût par jour × secteur × créneau | voir [Le coût du travail](#le-coût-du-travail) |
| `DB_RESA` | réservations : début, fin, couverts, lieu | historique + calendriers salle et terrasse |
| `DB_PASSWORD` | mots de passe, période de validité, nom, rôle, onglets | onglet `IMPORT PASS NEW` (`SHEET_PASS`) |

### Enregistrées si elles existent (`TABLES_OPTIONNELLES`)

Le sas d'une nouvelle source : le volet qui la lit sait se taire tant qu'elle
manque.

| Table | Contenu |
|---|---|
| `DB_ECRITURES` | le grand livre Odoo, une ligne par écriture |
| `DB_ONSS` | la paie : heures et coût par mois × secteur |
| `DB_HEURES_PLANNING` | heures planifiées par jour × service |
| `DB_VALIDATION_COMPTA` | les décisions prises sur les mois douteux |
| `CONTROLE_ODOO`, `CONTROLE_ONSS` | les contrôles de lecture, pour les afficher |

### `DB_TICKET` en deux morceaux

Le cache ne garde que `DATE, DATE_TS, HEURE, ID_TICKET, ID_REF, QUANTITE,
PRIX_TOTAL` ; les attributs du produit vivent une fois dans `REF_PRODUITS`.
`hydrate_donnees()` refait la jointure au chargement **et** à l'import (une
seule définition de la forme complète).

La clé est `ID_REF`, une ligne de référentiel par combinaison d'attributs
observée, et non `ID_PRODUIT` : la caisse laisse renommer une ligne, un même
produit porte plusieurs libellés, et une jointure sur `ID_PRODUIT` dupliquerait
les ventes. L'import s'arrête si l'hydratation ne rend pas exactement le
nombre de lignes reçu.

`DATE` est le jour de **service** (une vente après minuit appartient à la
veille) ; `DATE_TS` est le jour de l'horodatage.

### La TVA des produits

L'export produits est indexé par **libellé**. `tva_par_produit()` cherche donc
le taux sur tous les libellés d'un produit (le plus vendu tranche) ;
`tva_remises()` donne aux remises de caisse (« 20% sur produit : Espresso »,
lignes négatives) le taux du produit remisé, 21 % à défaut ;
`produits_sans_tva()` liste ce qui reste. Une ligne sans taux garde son TVAC
mais n'a pas de HTVA.

---

## La comptabilité

### D'où vient `DB_COMPTA`

- Jusqu'au 30/06/2026 : `DB_COMPTA.rds`, l'ancienne comptabilité consolidée.
- À partir du 01/07/2026 (`BASCULE_ODOO`) : les écritures Odoo, agrégées au
  mois par compte (`compta_mensuelle()`, classes 6 et 7 seulement).

`coud_compta()` partitionne par date : un mois vient de l'une ou de l'autre,
jamais des deux.

### Le contrat de `DB_COMPTA`

| Colonne | Sens |
|---|---|
| `ANNEE`, `MOIS` | la période ; le grain natif est le mois |
| `COMPTE` | numéro PCMN, en texte (`"604000"`) |
| `LIBELLE` | libellé du compte |
| `VALEUR` | **magnitude positive** : le signe vit dans le numéro de compte |
| `TYPE` | `"compte"`, ou `"solde"` (sous-total publié, sert au contrôle) |
| `AGREGE` | la ligne entre dans les totaux |
| `SECTEUR` | secteur des seuls comptes d'achat (60, 609) |
| `CATEGORIE`, `SECTION` | vocabulaire de l'ancien export, encore lu par deux consommateurs |

`VALEUR = SENS_G × (CREDIT − DEBIT)`, avec `SENS_G` = +1 pour un produit, −1
pour une charge : une charge comme un produit sortent positifs, une note de
crédit sort négative.

### Du compte au poste

Tout se classe sur le **numéro** de compte (`PLAN_PCMN`, `R/plan_comptable.R`) :
les libellés et les rubriques des exports changent d'un mois à l'autre, pas
la structure du PCMN. Un compte inconnu tombe dans « Non classé » et se voit
dans Compta → Contrôle.

`bloc_gestion()` regroupe ensuite les postes en blocs de gestion (CA, autres
produits, matières, rémunérations, frais généraux, amortissements,
financier), et `postes_exploitation()` en tire une ligne par mois.

### Les trois marges

| Colonne | Définition | Usage |
|---|---|---|
| `MARGE` | produits − charges d'exploitation, amortissements compris | confrontée au solde publié (`controle_exploitation()`) |
| `MARGE_AA` | CA + autres produits − matières − rémunérations − frais généraux + financier | la marge affichée partout |
| `MARGE_PA` | `MARGE_AA − AMORTISSEMENT` | l'option « après amortissements » |

Identités vérifiées par les tests : `MARGE_AA = MARGE + AMORTISSEMENT +
FINANCIER`, `MARGE_PA = MARGE + FINANCIER`. Les pourcentages sont rapportés
au CA seul. Les trois marges sont recalculées au même endroit dans
`recalcule_gestion()` (`R/projection.R`), qu'un test garde identique.

### Les règles qui traversent le volet

- **Les mois à venir** (loyers, amortissements programmés dans Odoo) sont
  retirés par `compta_a_date()`, partout où `DB_COMPTA` entre dans le serveur ;
  un test vérifie qu'il n'y a pas d'autre chemin. Le grand livre les garde.
- **Compta brute / compta retenue** : la brute (sans les mois à venir) sert au
  tableau des mois et aux diagnostics de comptes ; la retenue (brute moins les
  mois écartés) sert à tout le reste.
- **Vraisemblance** (`periodes_improbables()`) : trois motifs — des charges
  sans vente, un poste sous la moitié de sa médiane des 12 mois précédents, le
  mois en cours. Un mois invraisemblable de moins de 4 mois est « à risque »,
  plus vieux « ancien, à vérifier ».
- **Validation** (`R/validation.R`) : une décision humaine recouvre l'état
  calculé ; elle enregistre le CA et le nombre de comptes vus, et cesse de
  s'appliquer s'ils changent.
- **Le pont de marge** (`pont_marge()`) sépare l'effet volume (`ΔCA × taux de
  marge de référence`) de la dérive de chaque poste (`réel − poids de
  référence × CA de la période`) ; la somme retombe à l'euro sur l'écart.
- **Le rabotage** ramène une période partielle et sa référence aux mêmes
  mois (`serie_rabotee()`), pour le pont comme pour le forage par compte.
- **La projection** (`R/projection.R`) ne produit qu'un affichage : rien de ce
  qu'elle calcule ne sort de son onglet.

---

## Le coût du travail

Trois sources, aucune complète :

| Source | Grain | Heures | Coût |
|---|---|---|---|
| Paie (`DB_ONSS`) | mois × secteur | réellement travaillées | coût employeur réel |
| Horeko (`DB_COUTS_TRAVAIL`) | jour × secteur × créneau | pointées | estimé : heures × taux du contrat |
| Comptabilité (comptes 62) | mois | aucune | réel, sans ventilation |

`DB_COUTS_TRAVAIL` part d'Horeko, puis `recale_couts_travail()` y substitue la
paie dès qu'elle couvre le mois : la paie fait autorité sur le total du mois
et du secteur, Horeko ne sert qu'à le répartir sur les jours et les créneaux.
`HEURES` / `COUT_TRAVAIL` sont les valeurs retenues, `HEURES_HOREKO` /
`COUT_HOREKO_LIGNE` les valeurs d'origine, `SOURCE_HEURES` dit laquelle a
servi. La comptabilité est un point de contrôle, jamais un correcteur.

Les libellés de secteur de toutes les sources passent par
`normalise_secteur()` (`R/planning.R`). Une **heure variable** est une heure
de service, un jour d'ouverture (`JOURS_FERMES`), sur un créneau de service ;
Travail et Planning utilisent la même définition.

---

## Conventions

- **Les dates d'un classeur** passent toujours par `parse_date_souple()`.
- **Pas de `%b`, `%B`, `%a`, `wday(label = TRUE)`** : ils suivent la locale du
  serveur. Utiliser `vecteur_mois_court` et `vecteur_jours` (`global.R`).
- **Pas de constante de premier niveau dans `R/` qui lise une `COUL_*`** :
  `global.R` définit les couleurs après avoir sourcé `R/`. Une table de
  couleurs se construit dans une fonction.
- **Les montants** : `format_CA(x, -1)` (à la dizaine d'euros) pour les
  ordres de grandeur, `euro_compta()` (au centime) pour la comptabilité.
- **`[[ ]]` et non `$`** pour lire une liste par son nom (`$` apparie par
  préfixe, en silence).
- **Un tableau triable** joint à chaque colonne formatée sa jumelle de tri
  `.tri:<colonne>` (`defs_tri()`, `R/theme.R`).
- **Les registres plutôt que les `if`** : les indicateurs (`INDICATEURS_ANALYSE`),
  les marges (`MARGES_GESTION`), les postes projetables (`PROJ_POSTES`), les
  sources (`SOURCES_TRAVAIL`) sont des tables ; ajouter un cas, c'est ajouter
  une ligne.

---

## Pièges connus

Tous échouent en silence ou loin de leur cause.

**Les inputs d'un onglet n'existent qu'après son insertion.** Avant la
connexion, ils valent `NULL`, alors que les observateurs tournent déjà.
- *Lire* un input d'onglet dans un `observe()` : `req()` ou une valeur de
  repli (`1 + NULL/100` vaut `numeric(0)`).
- *Garnir* un input d'onglet (`update*Input`) : attendre `ONGLETS_PRETS()`,
  qui passe à `TRUE` au flush suivant l'insertion. Sinon le message est perdu
  et le sélecteur reste vide. Dans un `observeEvent`, le `req(ONGLETS_PRETS())`
  va dans l'expression déclenchante, pas dans le corps.

**Un select vide renvoie `""`, pas `NULL`.** `as.integer("")` donne `NA`, et
l'erreur sort plus loin. Tester la valeur convertie.

**Une zone d'alerte vide ne s'affiche jamais.** Un `uiOutput` vide est masqué
en CSS (`.zone-alerte:empty`), et Shiny suspend les sorties masquées : elle ne
se calculerait jamais. Toute zone `class = "zone-alerte"` doit figurer dans
`ZONES_ALERTE` (`R/helpers.R`) ; `reveille_zones_alerte(output)` est appelée
en dernier dans `server.R`. Un test le vérifie. `testServer()` ne voit pas ce
défaut : seul un navigateur le montre.

**DT trie sur ce qu'il affiche.** Sans jumelle de tri, « 90€ » passe devant
« 1.234€ ». Et une jumelle ne doit jamais être `orderable = FALSE`, sinon
plus rien ne se trie.

**`layout_columns()` garde la cellule d'un enfant masqué.** Construire
seulement ce qu'on affiche.

**Un `tryCatch()` dans `import.R`** évalue son bloc dans l'environnement de
l'import : y assigner avec `<-`, jamais `<<-`.

**Les hauteurs de carte et de DataTable** peuvent se contredire ; la règle
`#ecr_table` de `www/style.css` en est l'exemple.

---

## Tests

```sh
Rscript tests/lancer.R            # toute la suite (une vingtaine de secondes)
Rscript tests/lancer.R analyse    # seulement les fichiers dont le nom contient « analyse »
```

Sans framework : chaque fichier imprime une ligne par assertion, le lanceur
compte et sort en erreur s'il reste un `FAIL`. Shiny, plotly et DT y sont
remplacés par des doublures inertes : on éprouve du calcul, pas du rendu.
Chaque fichier source le code avec `source(..., local = TRUE)`.

Plusieurs tests rejouent un **bloc réel** de `server.R` ou d'`import.R`, repéré
par ses titres de section (`#### Volet "Compta / Gestion" — socle commun ####`,
`#### Compta Odoo ####`…) : ne renommez pas un titre de section sans mettre à
jour le test qui le cherche.

La suite tourne sur GitHub Actions à chaque poussée
(`.github/workflows/tests.yml`). Sur une machine en locale C, lancer avec
`LC_ALL=C.UTF-8`.

---

## Ajouter quelque chose

| Pour… | Faire |
|---|---|
| un onglet | une ligne dans `ONGLETS` (`R/acces.R`), une fonction `ui_*()` dans `ui.R`, une section dans `server.R` ; l'ajouter aux `PROFILS` concernés |
| une source facultative | la lire dans `import.R` dans un `tryCatch`, ajouter sa table à `TABLES_OPTIONNELLES`, faire taire proprement le volet tant qu'elle manque |
| une année d'objectifs | un onglet `IMPORT OBJECTIFS <année>` dans le classeur, et son nom dans `SHEETS_OBJECTIFS` (`import.R`) |
| un indicateur à Écarts / Évolution | une ligne dans `INDICATEURS_ANALYSE` (`R/analyse.R`) |
| une zone d'alerte | `class = "zone-alerte"` dans `ui.R` et son identifiant dans `ZONES_ALERTE` |
| un compte nouveau | rien si son préfixe est dans `PLAN_PCMN` ; sinon une ligne dans `PLAN_PCMN` |
