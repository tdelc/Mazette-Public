# Brancher Odoo sur Mazette

> État : document de travail, écrit avant toute implémentation.
> Rien dans `R/` ne dépend encore de ce qui suit.

## Le problème, en une phrase

`DB_COMPTA` arrive aujourd'hui dans le dashboard sous forme d'un `.rds` déjà
consolidé, téléchargé du Drive (`ID_DB_COMPTA`, cf. `import.R:91`). Personne ne
peut dire, en lisant le dépôt, d'où sortent ses chiffres ni comment les refaire.
C'est la dernière boîte noire du projet, et c'est elle qu'Odoo doit remplacer.

L'objectif n'est donc pas « importer Odoo ». Il est : **rendre la comptabilité
du dashboard reproductible à partir d'une source brute**, comme le sont déjà les
tickets, les heures et la paie.

---

## 1. Le contrat : ce que le dashboard attend

Avant de parler d'Odoo, il faut écrire ce qui est consommé en aval, parce que
c'est ça qui contraint tout le reste. Aucun de ces points n'est documenté
aujourd'hui ailleurs que dans le code qui les lit.

### Colonnes de `DB_COMPTA`

| Colonne     | Type    | Rôle                                                              |
|-------------|---------|-------------------------------------------------------------------|
| `ANNEE`     | int     | Année de la période.                                               |
| `MOIS`      | int     | Mois (1–12). La granularité native est **le mois**, jamais plus fin.|
| `COMPTE`    | chr     | Numéro de compte PCMN, **en texte** (`"604000"`).                   |
| `LIBELLE`   | chr     | Libellé du compte.                                                 |
| `VALEUR`    | num     | **Magnitude positive** — voir ci-dessous.                          |
| `TYPE`      | chr     | `"compte"` pour une ligne de compte, `"solde"` pour un sous-total.  |
| `AGREGE`    | lgl     | `TRUE` si la ligne entre dans les totaux (écarte les reprises « dont »). |
| `SECTEUR`   | chr     | Vocabulaire de `normalise_secteur()` (`R/planning.R:50`). **Ne sert qu'aux comptes d'achat** — voir §2.5. |
| `CATEGORIE` | chr     | Étiquette héritée de l'export du comptable : `ACHATS`, `VARIATION_STOCK`, `REMUNERATION`… |
| `SECTION`   | chr     | Idem, littéral : `"Coût des ventes et prestations"`.                |

### La convention de signe

**`VALEUR` est une magnitude positive. Le signe vit dans le numéro de compte**,
via `SENS_G` (+1 produit, −1 charge) que `classe_comptes()` déduit du PCMN
(`R/plan_comptable.R:28`). Une charge de 1 200 € s'écrit `VALEUR = 1200`, jamais
`-1200`. C'est écrit noir sur blanc dans `R/exploitation.R:79` et tout le volet
Compta en dépend.

D'où la seule formule de conversion qui tienne, depuis un débit et un crédit :

```r
VALEUR = SENS_G * (CREDIT - DEBIT)
```

Une charge (`SENS_G = -1`) a un solde débiteur : la formule rend `DEBIT - CREDIT`,
positif. Un produit (`SENS_G = +1`) a un solde créditeur : elle rend
`CREDIT - DEBIT`, positif aussi. Une note de crédit sort négative, et c'est
correct — elle diminue la charge. Se tromper de sens ici inverse tout le volet
Compta sans lever la moindre erreur.

### Les quatre consommateurs

| Où                                            | Ce qu'il filtre                                                                                   |
|-----------------------------------------------|---------------------------------------------------------------------------------------------------|
| `classe_comptes()` — tout le volet Compta      | `TYPE == "compte"`, `AGREGE`, puis classe sur `COMPTE`                                             |
| `DB_COUTS_MATIERE` — `server.R:47`             | `CATEGORIE ∈ {ACHATS, VARIATION_STOCK}` **et** `SECTION == "Coût des ventes et prestations"`, par `SECTEUR` |
| `DB_HEURES_COMPTA` — `import.R:742`            | `CATEGORIE == "REMUNERATION"`                                                                      |
| `controle_exploitation()` — `R/exploitation.R:132` | `TYPE == "solde"` et `LIBELLE` ≈ « Bénéfice … d'exploitation »                                 |

Deux remarques qui pèsent sur la suite :

1. **`CATEGORIE` et `SECTION` sont des reliquats** de la mise en page du
   comptable. Le reste du dashboard les a déjà abandonnés au profit du numéro de
   compte, précisément parce qu'ils dérivaient d'un mois à l'autre
   (`R/plan_comptable.R:11`). Les deux consommateurs qui s'en servent encore
   devront migrer vers `classe_comptes()` — sinon la reconstruction Odoo devra
   fabriquer de fausses chaînes `"Coût des ventes et prestations"` pour satisfaire
   un `filter()`, ce qui est exactement le genre de chose qui pourrit.

2. **Les lignes `TYPE == "solde"` ne sont pas des données, c'est un contrôle.**
   Elles portent les sous-totaux *publiés par la comptabilité*, que
   `controle_exploitation()` confronte à la marge *recalculée* par le dashboard.
   Si on reconstruit les soldes nous-mêmes à partir des écritures, le contrôle se
   compare à lui-même et ne vaut plus rien. Il faut donc une **seconde source,
   indépendante** — voir §2.5.

---

## 2. Phase 1 — sans l'API

### 2.1 Quoi télécharger : les écritures, pas les rapports

**Télécharge `Écritures comptables` (le modèle `account.move.line`), pas le
compte de résultat.** C'est contre-intuitif — le rapport ressemble davantage à ce
qu'on veut afficher — mais :

- **Le grain fin est stable, la mise en page ne l'est pas.** Le dépôt en porte
  déjà la trace : 31 nombres de lignes différents sur 52 mois d'exports du
  comptable, et des comptes qui changent de rubrique d'un exercice à l'autre
  (`R/plan_comptable.R:11-21`). Une ligne d'écriture, elle, a les mêmes dix
  colonnes depuis toujours.
- **Tout le reste s'en déduit** : compte de résultat, balance, grand livre,
  coûts par secteur. L'inverse est faux.
- **Le dashboard sait déjà s'en servir.** `classe_comptes()` n'a besoin que de
  `COMPTE`, `VALEUR`, `ANNEE`, `MOIS`. Rien à réécrire.
- **Le bilan vient gratuitement.** Prends toutes les classes, pas seulement 6 et
  7 : trésorerie, dettes, stocks et immobilisations seront là le jour où tu en
  voudras, sans nouvel export.

### 2.2 L'export, pas à pas

1. **Comptabilité → Écritures comptables** (le menu de ta capture).
2. **Filtre** : `Comptabilisé` (`parent_state = posted`). Les brouillons ne sont
   pas de la comptabilité.
3. **Retire le filtre de période par défaut** s'il y en a un, et prends tout
   l'historique disponible.
4. Coche la case d'en-tête, puis **« Sélectionner les N enregistrements »** — sans
   ça tu n'exportes que la page affichée.
5. **Icône de téléchargement** (en haut à gauche de ta capture) → Exporter.
6. **Format XLSX**, pas CSV. En XLSX, `readxl` reçoit des dates et des nombres
   déjà typés ; en CSV tu hérites du format belge (`1.234,56`, `30/06/2026`) et
   il faut le défaire à la main — on a bien `nombre_europeen()`
   (`R/sources_travail.R:129`), mais autant ne pas en avoir besoin.
7. **Enregistre le modèle d'export** (« Enregistrer le format d'exportation »).
   C'est ce qui garantit que l'export du mois prochain aura exactement les mêmes
   colonnes dans le même ordre — donc qu'aucun code R ne cassera en silence.

### 2.3 Les champs à cocher

| Libellé Odoo            | Champ technique       | Pourquoi                                             |
|-------------------------|-----------------------|------------------------------------------------------|
| Date                    | `date`                | Détermine `ANNEE` / `MOIS`.                           |
| Pièce comptable         | `move_id` / `name`    | Traçabilité (« OD/2026/06/0001 »).                   |
| Journal                 | `journal_id`          | Repérer les OD de clôture, isoler les ventes.         |
| Compte → Code           | `account_id.code`     | **`COMPTE`.** Si le dialogue ne propose pas le code séparément, prends `Compte` et sépare en R. |
| Compte → Nom            | `account_id.name`     | `LIBELLE`.                                            |
| Libellé                 | `name`                | Le texte de la ligne, utile au débogage.              |
| Partenaire              | `partner_id`          | Pas utilisé tout de suite ; gratuit.                  |
| Débit                   | `debit`               | → `VALEUR`.                                           |
| Crédit                  | `credit`              | → `VALEUR`.                                           |
| Solde                   | `balance`             | Contrôle : `balance = debit − credit`.                |

Dans ta capture, la colonne « Compte » affiche `111900 Autres…` : c'est le
*display name*, code et libellé concaténés. Si tu dois passer par lui :

```r
COMPTE  <- stringr::str_extract(x, "^[0-9]+")
LIBELLE <- stringr::str_trim(stringr::str_remove(x, "^[0-9]+"))
```

### 2.4 Ce qu'un extrait réel confirme — et le secteur

Mesuré sur un extrait de 51 lignes de septembre 2026 (journaux `MZACH`, `MZVEN`,
`TRI1`), en appliquant la règle de préfixe de `PLAN_PCMN` :

| Constat | Chiffre | Conséquence |
|---|---|---|
| Lignes de bilan (classes 1–5) | **47 sur 51** | voir ci-dessous |
| Lignes de résultat (classes 6–7) | **4 sur 51** | c'est tout ce qui alimente `DB_COMPTA` |
| Comptes non classés par `PLAN_PCMN` | 8, **tous de classe 1–5** | le plan couvre bien tout le résultat |
| Pièces déséquilibrées | 2 sur 24 | artefact de l'extrait tronqué — le contrôle fonctionne |

**Il faut donc filtrer sur les classes 6 et 7 avant de produire `DB_COMPTA`.**
Sans ce filtre, `comptes_non_classes()` — le contrôle qui doit signaler un compte
de résultat oublié — se retrouverait noyé sous 92 % de comptes de bilan et ne
dirait plus rien. L'extrait complet, lui, se garde de côté : c'est le bilan de
demain, et il ne coûte rien à conserver.

Le classement, lui, tombe juste du premier coup :

```
623700  Titres-repas                      -> Rémunérations        sens -1   820,56
700200  Ventes 12% (nourriture sur place) -> Chiffre d'affaires   sens +1  1785,50
700300  Ventes 21% (boisson sur place)    -> Chiffre d'affaires   sens +1   620,00
```

Au passage : le journal se lit dans le préfixe du numéro de pièce
(`str_extract(NUMERO, "^[^/]+")`), et le code de compte dans le préfixe du
libellé (`str_extract(COMPTE, "^[0-9]+")`), `"623700 Titres-repas"` donnant
`623700` et `Titres-repas`.

#### Le secteur : un besoin beaucoup plus étroit qu'il n'y paraît

Première chose à lever, parce que la question se pose naturellement :
`normalise_secteur()` **ne peut pas servir ici**. Elle normalise des *libellés de
secteur* déjà présents (« Transfo alimentaire » → « Transformation
alimentaire ») ; elle ne peut pas déduire un secteur d'un numéro de compte. Il
faut une table de correspondance, qui n'existe pas encore.

Mais son périmètre est minuscule. `DB_COMPTA$SECTEUR` n'a **qu'un seul
consommateur** : `DB_COUTS_MATIERE` (`server.R:47`), qui filtre
`CATEGORIE ∈ {ACHATS, VARIATION_STOCK}` — c'est-à-dire, dans le vocabulaire du
plan, les préfixes `60` et `609`. Tous les autres `SECTEUR` du dashboard viennent
de `DB_COUTS_TRAVAIL` (Horeko et paie), pas de la comptabilité.

**Seuls les comptes de la classe 60 ont besoin d'un secteur.** Ni les ventes, ni
les rémunérations, ni les frais généraux, ni le bilan.

La table vit dans une feuille Google, comme les autres paramètres — le plan
comptable grandira, et une correspondance figée dans le code vieillirait mal :

| `COMPTE` | `SECTEUR` |
|---|---|
| `604100` | Transformation alimentaire |
| `604200` | Service |
| … | … |

avec trois règles : le préfixe le plus long l'emporte (comme dans `PLAN_PCMN`),
un compte absent tombe dans **« Non ventilé »** plutôt que dans un secteur par
défaut, et un contrôle affiche ce que « Non ventilé » pèse chaque mois. C'est la
règle déjà appliquée à la paie (`SECTEUR_NON_VENTILE`, `R/sources_travail.R:76`)
et aux produits sans taux de TVA.

**Ce qu'il faut pour l'écrire** : le plan comptable des classes 6 et 7, code et
libellé (Comptabilité → Configuration → Plan comptable, filtre sur le code,
export). Un fichier de quelques dizaines de lignes.

La question à laquelle ce fichier répondra — et qui décide de la suite : **les
comptes d'achat distinguent-ils déjà la nourriture de la boisson ?** Les comptes
de vente le font (`700200` nourriture, `700300` boisson) ; si les achats suivent
la même logique, la correspondance s'écrit en dix minutes. Sinon — un seul
`604000 Achats de marchandises` pour tout — aucune table ne pourra ventiler quoi
que ce soit, et il faudra créer des sous-comptes dans Odoo. C'est le seul endroit
où le travail retombe côté comptabilité plutôt que côté code.

Bonus à garder en tête : ce découpage des ventes par taux de TVA donne, le jour
où on le voudra, un **CA nourriture / boisson issu de la comptabilité** — donc
un contrôle indépendant du point de vente, sur la grandeur la plus regardée du
dashboard.

### 2.5 Le contrôle indépendant

Les lignes `TYPE == "solde"` disparaissent avec l'ancien export. Pour que
`controle_exploitation()` garde un sens, télécharge **en plus, une fois par
mois**, un rapport agrégé — `Balance générale` ou le MIS Report du compte de
résultat — et range-le dans une petite table à part, par exemple
`CONTROLE_COMPTA(ANNEE, MOIS, LIBELLE, VALEUR)`.

Ce fichier n'alimente **aucun chiffre affiché**. Il sert uniquement à dire :
« le résultat d'exploitation que je recalcule à partir des écritures vaut-il
celui qu'Odoo publie ? ». C'est le même principe que `controle_onss()` pour la
paie, et c'est ce qui permet de faire confiance au reste.

### 2.6 Où déposer les fichiers

Sur le Drive, un dossier `compta/odoo/`, **un fichier par export, daté, jamais
écrasé** : `ecritures_2026-10-10.xlsx`. Une anomalie repérée en mars doit pouvoir
être retracée jusqu'au fichier qui l'a introduite.

Deux nouvelles clés dans la feuille des chemins, à côté de `ID_DB_COMPTA` :

| Clé                   | Contenu                                          |
|-----------------------|--------------------------------------------------|
| `ID_ODOO_ECRITURES`   | Le dernier export d'écritures.                   |
| `ID_ODOO_ANALYTIQUE`  | Le dernier export de lignes analytiques.          |
| `ID_ODOO_CONTROLE`    | Le rapport agrégé de contrôle.                   |

### 2.7 Le code : un fichier, quatre fonctions

Un nouveau `R/odoo.R`, sur le modèle de `R/sources_travail.R` : des fonctions
**pures**, testables sans réseau ni fichier.

```r
# Lecture brute. C'est la SEULE fonction que l'API remplacera (cf. §3).
lit_ecritures_fichier <- function(chemin)        # -> tibble brut

# Mise en forme : types, code de compte, débit/crédit -> magnitude + sens.
normalise_ecritures   <- function(brut)          # -> une ligne par écriture

# Agrégation mensuelle : c'est elle qui produit DB_COMPTA.
compta_mensuelle      <- function(ecritures, secteurs = NULL)

# Contrôles. Renvoie une table, jamais un stop().
controle_ecritures    <- function(ecritures, compta, controle = NULL)
```

Puis, dans `import.R`, un bloc qui enchaîne les quatre et **remplace** le
`readRDS("DB_COMPTA.rds")` de la ligne 91.

### 2.8 Les contrôles à écrire, et pourquoi chacun

| Contrôle                                              | Ce qu'il attrape                                                    |
|-------------------------------------------------------|----------------------------------------------------------------------|
| `sum(debit) == sum(credit)` par pièce et au total     | Un export tronqué (sélection partielle, limite de lignes).           |
| Résultat recalculé vs rapport de contrôle (§2.4)      | Un compte mal classé, une écriture oubliée.                          |
| `comptes_non_classes()` — existe déjà                 | Un compte hors PCMN qui n'entre dans aucun total.                    |
| Montant par secteur « Non ventilé »                   | Une analytique qui se dégrade sans prévenir.                         |
| Un mois déjà clôturé qui change entre deux imports    | Une écriture passée rétroactivement.                                 |
| Journaux et dates des écritures de clôture            | Voir ci-dessous.                                                     |

**Les OD de clôture.** Réponse obtenue : l'exercice fiscal est l'**année
civile**, et la pièce `OD/2026/06/0001` du 30/06/2026 est une **reprise de
balance** — le solde de l'ancien comptable, repris à l'ouverture d'Odoo. Elle ne
touche que des comptes de bilan (11, 14, 15, 17), donc elle n'entre pas dans le
résultat. Le contrôle reste à écrire, parce que ce qui est vrai de cette
pièce-là ne l'est pas forcément de la suivante : **aucune ligne de classe 6 ou 7
ne doit porter la date de reprise.** Si l'une s'y glisse, elle ferait doublon
avec l'ancienne source.

### 2.9 La jonction avec l'ancienne comptabilité

C'est la conséquence structurelle de la reprise, et elle ne se contourne pas :
**Odoo ne contient rien avant le 30/06/2026.** L'historique 2022 → juin 2026 ne
vit que dans l'ancien `DB_COMPTA.rds`. La table finale est donc une couture :

```
DB_COMPTA  =  ancien .rds   jusqu'au 2026-06 inclus
            + Odoo          à partir du 2026-07
```

La date de couture est une **constante nommée**, pas un nombre répété dans trois
`filter()`. La coupure tombe heureusement sur une frontière de mois, ce qui évite
le pire.

Trois contrôles, tous nécessaires, parce que chacun attrape une faute que les
deux autres laissent passer :

1. **Aucun mois présent des deux côtés.** Un recouvrement double un mois entier.
2. **Aucun mois manquant entre les deux.** Un trou se lit comme une chute
   d'activité.
3. **Aucune ligne de classe 6 ou 7 datée du 30/06/2026** dans Odoo (cf.
   ci-dessus).

Et une conséquence à accepter : **2026 est une année à deux sources**, moitié
ancienne, moitié Odoo. Les comparaisons d'année à année du volet Analyse
l'enjamberont. Ce n'est pas un bug, mais ça mérite d'être dit à l'écran le jour
où on les regardera — comme on marque déjà les périodes à cheval dans Détails.

## 3. Phase 2 — avec l'API

### Ce qui change : une fonction. Vraiment.

C'est l'argument central de tout ce document : si la phase 1 est découpée comme
au §2.7, **seule `lit_ecritures_fichier()` est remplacée** par
`lit_ecritures_odoo()`. La normalisation, l'agrégation, les contrôles, les tests
et `DB_COMPTA` ne bougent pas. Le travail de la phase 1 n'est pas du jetable :
c'est 90 % de la phase 2, écrit et éprouvé d'avance.

### Comment appeler Odoo depuis R

Pas de client Odoo en R, mais il n'en faut pas : **JSON-RPC avec `httr` et
`jsonlite`**, tous deux déjà dans `manifest.json` (1.4.7 et 2.0.0). Aucune
dépendance nouvelle, donc aucun risque sur le déploiement.

```r
# Esquisse — non testée contre ton instance.
odoo_appel <- function(url, base, uid, cle, modele, methode,
                       args = list(), kwargs = list()) {
  corps <- list(
    jsonrpc = "2.0", method = "call",
    params = list(service = "object", method = "execute_kw",
                  args = c(list(base, uid, cle, modele, methode),
                           list(args), list(kwargs))))
  rep <- httr::POST(paste0(url, "/jsonrpc"),
                    body = jsonlite::toJSON(corps, auto_unbox = TRUE),
                    httr::content_type_json())
  res <- jsonlite::fromJSON(httr::content(rep, "text", encoding = "UTF-8"))
  if (!is.null(res$error)) stop("Odoo : ", res$error$data$message)
  res$result
}

# L'authentification passe par le service "common" / "login" et rend un uid.
```

**Authentification : une clé API, jamais le mot de passe.** Dans Odoo :
Préférences → Sécurité du compte → Clés API. Elle se range comme les autres
secrets (variable d'environnement, à côté de `LINK_JSON` et `PATH_DRIVE`), pas
dans le dépôt.

L'appel utile :

```
modele  : "account.move.line"
methode : "search_read"
domaine : [["parent_state","=","posted"], ["date",">=","2022-01-01"]]
champs  : date, move_name, journal_id, account_id, name, debit, credit, balance
pagination : limit = 5000, offset croissant jusqu'à retour vide
```

### Trois décisions à prendre maintenant, pas plus tard

1. **Tout recharger à chaque import, pas d'incrémental.** Un filtre sur
   `write_date` rate les écritures passées rétroactivement sur un mois ancien —
   et en comptabilité, ça arrive. Le volume est de l'ordre de quelques dizaines
   de milliers de lignes : c'est quelques secondes. Même raisonnement que pour
   l'historique du planning.
2. **Agréger en R, pas avec `read_group`.** Odoo sait agréger côté serveur, mais
   ce serait un second chemin de calcul à côté de celui de la phase 1 : deux
   chemins divergent toujours. On récupère les lignes et on passe par
   `compta_mensuelle()`, la même pour les deux phases.
3. **Garder le contrôle indépendant.** Avec l'API, la tentation est de tout
   dériver des écritures. Le rapport agrégé d'Odoo reste la seule chose qui
   puisse dire que notre recalcul est juste — et il s'appelle par l'API lui aussi
   (`account.report` ou le MIS Builder que tu as déjà installé).

---

## 4. Dans quel ordre

| # | Étape                                                                              | Bloque la suite ? |
|---|------------------------------------------------------------------------------------|-------------------|
| 1 | Un export XLSX d'écritures, tout l'historique, déposé sur le Drive                   | oui               |
| 2 | Vérifier sur ce fichier : date de reprise, aucune classe 6/7 à cette date, comptes hors PCMN | oui        |
| 3 | `R/odoo.R` + ses tests, sur le fichier réel                                          | oui               |
| 4 | Coudre Odoo à l'ancien `.rds` au 30/06/2026, et **comparer poste à poste sur un mois commun reconstruit** | oui |
| 5 | Migrer `DB_COUTS_MATIERE` et `DB_HEURES_COMPTA` du couple `CATEGORIE`/`SECTION` vers `classe_comptes()` | non |
| 6 | Table de correspondance compte 60 → secteur, d'après le plan comptable               | non               |
| 7 | Remplacer la lecture fichier par l'API                                               | non               |

L'étape 4 est la seule qui ne se saute pas : **tant que les deux sources ne
donnent pas le même compte de résultat au centime près, on garde l'ancien.** Un
écart, même petit, est une question à résoudre, pas un arrondi à accepter.
