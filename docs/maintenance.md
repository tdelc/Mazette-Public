# Maintenir le dashboard

Pour qui fait tourner le dashboard : installation, sources Google, import
automatique, Odoo, hébergement, accès, et que faire quand ça casse.

- [Vue d'ensemble](#vue-densemble)
- [Les variables d'environnement](#les-variables-denvironnement)
- [Google Drive et Google Sheets](#google-drive-et-google-sheets)
- [Odoo](#odoo)
- [L'import quotidien](#limport-quotidien)
- [Héberger sur Posit Connect](#héberger-sur-posit-connect)
- [Mots de passe et profils](#mots-de-passe-et-profils)
- [Trancher un mois douteux](#trancher-un-mois-douteux)
- [Gestes courants](#gestes-courants)
- [Quand ça casse](#quand-ça-casse)
- [Installer de zéro](#installer-de-zéro)

---

## Vue d'ensemble

```
Google Drive / Sheets ─┐
Odoo (API) ────────────┼─► import.R ─► R_new_env_<date>.RData ─► dashboard
calendriers iCal ──────┘   (GitHub Actions,     (dossier Drive)    (Posit Connect)
                            chaque matin)
```

- L'**import** lit toutes les sources et dépose un fichier de données (le
  « cache ») sur Drive. Il tourne chaque matin sur GitHub Actions, ou à la
  demande depuis le dashboard.
- Le **dashboard** ouvre toujours le cache le plus récent et affiche sa date.
  Il ne refait jamais l'import de lui-même (sauf s'il n'existe aucun cache).
- Les **identifiants** ne sont jamais dans le dépôt, qui est public : ils
  vivent dans les variables d'environnement de chaque machine.

---

## Les variables d'environnement

À déclarer **sur chaque machine qui lance l'import** : le PC de développement
(fichier `.Renviron` à la racine, ignoré par git), l'hébergeur du dashboard
(Posit Connect → *Vars*), et GitHub (environnement `SECRET`).

| Variable | Rôle | Où |
|---|---|---|
| `LINK_JSON` | URL privée de la clé JSON du compte de service Google | partout |
| `PATH_DRIVE` | identifiant du dossier Drive où déposer les caches | partout |
| `ID_DRIVE_PATHS` | identifiant du classeur d'index `PATHS` | partout |
| `PATH_VALIDATION_COMPTA` | identifiant du classeur des décisions sur les mois | PC, hébergeur |
| `ODOO_URL`, `ODOO_DB`, `ODOO_LOGIN`, `ODOO_CLE_API` | l'accès à Odoo (cf. [Odoo](#odoo)) | partout |
| `GH_TOKEN_IMPORT` | jeton GitHub pour lancer l'import depuis le dashboard | hébergeur |
| `GH_DEPOT`, `GH_REF` | facultatifs : `tdelc/Mazette-Public` et `main` par défaut | hébergeur |

Ce sont des **secrets** : jamais en clair dans un fichier du dépôt, jamais
comme « variable » GitHub (les journaux d'Actions sont publics). Les espaces,
retours à la ligne et guillemets collés autour d'une valeur sont retirés à la
lecture pour les variables Odoo, mais tapez-les proprement partout.

---

## Google Drive et Google Sheets

### Le compte de service

L'application lit Drive avec un compte de service Google. Sa clé JSON est
téléchargée depuis `LINK_JSON` au démarrage (puis écrite dans `connect.json`,
ignoré par git). **Toute personne qui a cette URL a accès aux fichiers** :
gardez-la privée.

Chaque fichier lu doit être **partagé avec l'adresse du compte de service**,
comme avec un collègue : en lecture, sauf les deux classeurs qu'il écrit.

### Le classeur d'index `PATHS`

Aucun identifiant n'est écrit dans le code : un classeur (`ID_DRIVE_PATHS`)
porte un onglet `PATHS` à deux colonnes, `Path` et `Link`. Déplacer une
source, c'est changer une ligne.

| `Path` | La source | Partage |
|---|---|---|
| `ID_DRIVE_MAZETTE` | le classeur principal (ventes, tickets, brassins, objectifs, mots de passe) | lecture |
| `ID_DRIVE_HEURES` | le classeur Horeko (onglets `Rapport` et `Cout`) | lecture |
| `ID_DB_COMPTA` | `DB_COMPTA.rds`, l'ancienne comptabilité (jusqu'au 30/06/2026) | lecture |
| `ID_DRIVE_RESA` | l'historique des réservations | lecture |
| `ICS_SALLE`, `ICS_TERRASSE` | les calendriers iCal des réservations | — |
| `ICS_HEURES` | le calendrier iCal des shifts planifiés | — |
| `PATH_HEURES_PLANNING` | le Sheet où l'import archive le planning (onglet `HEURES PLANNING`, créé au premier passage) | **éditeur** |
| `PATH_HEURES_ONSS` | le Sheet de paie (onglet `PAIE`, à défaut le premier) | lecture |
| `ID_ODOO_ECRITURES` | facultatif : un export Odoo, si l'API n'est pas configurée | lecture |
| `ID_MAZETTE_2023`, `ID_MAZETTE_2025` | les archives de caisse 2023-2025 | lecture |

Le classeur des décisions (`PATH_VALIDATION_COMPTA`) est lui aussi partagé en
**éditeur**.

### Le classeur principal

| Onglet | Contenu |
|---|---|
| `DB JOURS` | CA par jour |
| `IMPORT TICKET`, `IMPORT CAISSE` | les lignes de caisse |
| `IMPORT LIGHTSPEED` | ventes par jour et par produit, avec le taux de TVA |
| `IMPORT OLD DATA` | l'historique d'avant la caisse actuelle |
| `IMPORT BRASSINS`, `CORRESPONDANCE BIERES` | les brassins, et nom commercial → boisson → logo |
| `IMPORT OBJECTIFS` (2024), `IMPORT OBJECTIFS 2022`, `… 2023`, `… 2025`, `… 2026` | les objectifs mensuels, un onglet par année |
| `IMPORT PASS NEW` | les mots de passe (cf. plus bas) |

**Renommer un onglet** casse l'import : son nom est écrit dans `import.R`.

---

## Odoo

Depuis le 01/07/2026, la comptabilité vient d'Odoo ; l'historique d'avant
reste dans `DB_COMPTA.rds`. L'import lit les écritures **comptabilisées**
(les brouillons ne sont pas lus), les agrège au mois, et garde aussi le grand
livre ligne par ligne.

### Par l'API (le chemin normal)

Les quatre variables `ODOO_*` :

| Variable | Valeur |
|---|---|
| `ODOO_URL` | l'adresse du serveur, sans barre finale : `https://…` |
| `ODOO_DB` | le **nom de la base**, pas le sous-domaine : visible sur `<url>/web/database/selector` ou en bas de l'écran de connexion |
| `ODOO_LOGIN` | l'adresse e-mail du compte |
| `ODOO_CLE_API` | une **clé API**, pas le mot de passe : Odoo → *Préférences → Sécurité du compte → Clés API → Nouvelle clé* (elle se révoque sans changer le compte) |

Les quatre ou rien : avec trois sur quatre, l'import se rabat sur l'export
fichier (et le dit dans sa trace).

Pour vérifier la connexion, sans rien écrire :

```sh
Rscript essai_odoo_api.R
```

Il teste, dans l'ordre, les variables, le serveur (sans authentification),
l'authentification, les champs, le nombre de lignes, puis la lecture complète,
et **s'arrête au premier point qui échoue en le nommant**. Options :
`--depuis=2026-07-01`, `--max=500` (essai rapide), `--csv=sortie.csv`,
`--fichier=export.xlsx` (compare avec un export manuel).

### Par un export (en secours)

Odoo → *Comptabilité → Écritures comptables*, filtre **Comptabilisé**, sans
filtre de période, sélectionner **tous** les enregistrements, *Exporter* au
format **XLSX** avec les colonnes Date, Numéro, Compte, Partenaire, Libellé,
Débit, Crédit (enregistrez le modèle d'export pour garder les mêmes colonnes).
Déposez le fichier sur Drive et mettez son identifiant dans `PATHS` sous
`ID_ODOO_ECRITURES`. La comptabilité ne sera alors à jour que du dernier
export.

### Les contrôles de lecture

Visibles dans *Compta → Contrôle → Lecture des écritures*, et (sujet seul) dans
le résumé de l'import GitHub :

| Contrôle | Il signale |
|---|---|
| Lignes lues | des lignes écartées faute de date ou de compte (un format de date qui change) |
| Période couverte | la date de la dernière écriture ; ambre au-delà de 14 jours sans écriture |
| Écritures à venir | les écritures datées de mois futurs (normales, hors calculs) |
| Débit = crédit, Pièces équilibrées | un export tronqué |
| Comptes 6 et 7 classés | un compte de résultat que le plan ne connaît pas |
| Matières non ventilées | le poids des achats sans secteur (604000) |
| Pièce de reprise | des comptes de résultat dans la pièce de reprise du 30/06/2026 |
| Mois d'Odoo écartés | des mois d'Odoo antérieurs à la bascule (ambre, normal pour la seule reprise) |

---

## L'import quotidien

### Sur GitHub Actions

`.github/workflows/import.yml` lance `import_quotidien.R`, qui refait l'import
et dépose le cache sur Drive. Les secrets sont rangés dans un
**environnement GitHub nommé `SECRET`** (*Settings → Environments*), en
*secrets* : `LINK_JSON`, `PATH_DRIVE`, `ID_DRIVE_PATHS` et les quatre
`ODOO_*`. Restreindre cet environnement à la branche `main` (*Deployment
branches*) empêche toute autre branche de les lire.

Le journal d'Actions est **public** : `import_quotidien.R` n'y publie qu'un
résumé — les tables et leur nombre de lignes, les dates couvertes, la source
des écritures et le sujet des contrôles, jamais un montant.

Lancer un import à la main : onglet *Actions → Import quotidien → Run
workflow*.

### À heure fixe : Google Apps Script

La planification de GitHub (`schedule`) ne tient pas l'heure : des retards de
huit à dix heures ont été constatés. L'heure est donc donnée par **Apps
Script**, qui appelle le workflow à la demande (`workflow_dispatch`, exécuté
en quelques secondes). Le `schedule` reste en filet.

Dans [script.google.com](https://script.google.com), nouveau projet :

1. *Paramètres du projet* → fuseau horaire **Bruxelles** (l'heure d'été est
   gérée seule).
2. *Propriétés du script* → `GH_TOKEN` : un jeton GitHub *fine-grained*,
   portée **Actions: write** sur le seul dépôt (cf. plus bas).
3. Coller le code, puis exécuter **une fois** `installerDeclencheur`.

```javascript
const DEPOT  = 'tdelc/Mazette-Public';
const HEURE  = 7;    // heure de Bruxelles à laquelle lancer l'import
const MINUTE = 30;   // 7h30 : données prêtes vers 7h40

// Appelée toutes les cinq minutes : ne fait rien avant l'heure, ni deux fois
// le même jour. Un échec n'est pas noté : l'essai suivant recommence.
function verifierImport() {
  const tz   = Session.getScriptTimeZone();
  const now  = new Date();
  const jour = Utilities.formatDate(now, tz, 'yyyy-MM-dd');
  const min  = Number(Utilities.formatDate(now, tz, 'H')) * 60 +
               Number(Utilities.formatDate(now, tz, 'm'));
  const props = PropertiesService.getScriptProperties();
  if (min < HEURE * 60 + MINUTE) return;
  if (props.getProperty('DERNIER_IMPORT') === jour) return;
  lancerImportMazette();
  props.setProperty('DERNIER_IMPORT', jour);
}

function lancerImportMazette() {
  const r = UrlFetchApp.fetch(
    'https://api.github.com/repos/' + DEPOT +
      '/actions/workflows/import.yml/dispatches',
    {
      method: 'post',
      contentType: 'application/json',
      headers: {
        Authorization: 'Bearer ' +
          PropertiesService.getScriptProperties().getProperty('GH_TOKEN'),
        Accept: 'application/vnd.github+json'
      },
      payload: JSON.stringify({ ref: 'main' }),
      muteHttpExceptions: true
    });
  // 204 = accepté ; sinon une erreur, et Apps Script envoie un courriel.
  if (r.getResponseCode() !== 204)
    throw new Error('GitHub a répondu ' + r.getResponseCode() + ' : ' +
                    r.getContentText());
}

// À exécuter une fois ; la relancer remplace le déclencheur.
function installerDeclencheur() {
  ScriptApp.getProjectTriggers()
    .filter(t => t.getHandlerFunction() === 'verifierImport')
    .forEach(t => ScriptApp.deleteTrigger(t));
  ScriptApp.newTrigger('verifierImport').timeBased().everyMinutes(5).create();
}
```

Pour tester tout de suite : exécuter `lancerImportMazette` ; une exécution
« workflow_dispatch » apparaît dans l'onglet *Actions*.

### Les jetons GitHub

Deux jetons *fine-grained* (*GitHub → Settings → Developer settings →
Personal access tokens*), chacun limité au dépôt et à la portée **Actions:
write** : un pour Apps Script (`GH_TOKEN`), un pour le dashboard
(`GH_TOKEN_IMPORT`). Ils permettent de lancer ou d'annuler des exécutions,
rien d'autre : ni lire les secrets, ni modifier le code. Ils expirent : à
l'échéance, Apps Script envoie un courriel d'erreur et le bouton « Refaire
l'import » échoue ; il suffit de les remplacer.

### Deux limites à connaître

- Sur un dépôt public, GitHub **désactive les workflows planifiés après 60
  jours sans commit**. Le déclenchement par Apps Script n'est pas concerné.
- **Une seule machine devrait produire les caches.** Un import lancé ailleurs
  (un PC, l'hébergeur sans `GH_TOKEN_IMPORT`) dépose lui aussi un cache sur
  Drive ; s'il n'a pas les variables Odoo, ses écritures dateront du dernier
  export fichier.

### Ce qui dira que ça a cassé

1. **La pastille de date** du dashboard : ambre le lendemain, rouge au bout de
   quatre jours.
2. **Le courriel de GitHub** quand une exécution échoue (et celui d'Apps
   Script quand l'appel est refusé).
3. **Le résumé de l'exécution** : il distingue l'import qui plante de l'import
   qui réussit sur une source qui n'a pas bougé (« le dernier jour connu a N
   jours de retard »).

---

## Héberger sur Posit Connect

Le dashboard est publié sur Posit Connect. Les variables d'environnement se
déclarent dans les réglages du contenu (*Vars*) : elles ne sont jamais dans le
dépôt.

`manifest.json` décrit les fichiers et les paquets à installer. **Celui du
dépôt est obsolète** (il cite des fichiers supprimés depuis longtemps) : le
régénérer avant de publier.

```r
install.packages("rsconnect")
rsconnect::writeManifest()   # régénère manifest.json
rsconnect::deployApp()       # ou publier depuis RStudio
```

Le bouton « Refaire l'import » lance le workflow GitHub si `GH_TOKEN_IMPORT`
est déclaré ; sans lui, il refait l'import **dans le processus Shiny**, qui
reste bloqué plusieurs minutes pour tout le monde. En production, déclarez-le.

---

## Mots de passe et profils

Un onglet du classeur principal, `IMPORT PASS NEW` :

| Colonne | Rôle | Exemple |
|---|---|---|
| `Date_debut`, `Date_fin` | période de validité | `01/09/2026` → `31/12/2026` |
| `pass` | le mot de passe (sensible à la casse) | `mazette2026` |
| `nom` | affiché une fois connecté | `Boris` |
| `role` | le profil | `service` |
| `onglets` | onglets **en plus** du profil, ou `*` pour tout ; vide en général | `compta, travail` |
| `actif` | `non` coupe l'accès sans effacer la ligne | `oui` |

Les profils (`admin`, `support`, `service`, `transfo`, `public`, `invite`)
sont définis dans `PROFILS`, `R/acces.R` : leur contenu est décrit dans la
vignette [utilisation](utilisation.md#se-connecter-et-ce-quon-voit). Un rôle
vide ou mal orthographié n'ouvre que l'onglet *Maintenant*.

Un changement dans le classeur prend effet **au prochain import**. Un
changement de profil (`PROFILS`) demande de republier l'application.

C'est un contrôle d'accès interne, pas un coffre-fort : les mots de passe sont
en clair dans le classeur.

---

## Trancher un mois douteux

Le bouton *Trancher un mois* (Compta → Contrôle) écrit une ligne par décision
dans le classeur `PATH_VALIDATION_COMPTA`, partagé en **éditeur** avec le
compte de service. Rien n'y est jamais modifié : la dernière ligne d'un mois
fait foi.

| Colonne | Contenu |
|---|---|
| `MOIS` | le premier du mois, `AAAA-MM-JJ` |
| `DECISION` | `valide`, `rejete` ou `revoque` |
| `COMMENTAIRE` | ce qui a été vérifié |
| `QUI`, `QUAND` | la personne connectée ; `AAAA-MM-JJ HH:MM:SS` |
| `CA`, `N_COMPTES` | les chiffres du mois au moment de la décision |

Le classeur peut partir vide (la première décision pose l'en-tête) et se
corriger à la main. Une décision cesse de s'appliquer quand le CA ou le nombre
de comptes du mois change ; si ces deux colonnes sont vides, elle s'applique
toujours. Si l'écriture échoue (classeur non partagé en éditeur), rien ne
change à l'écran et un message le dit.

---

## Gestes courants

| Pour… | Faire |
|---|---|
| ajouter ou retirer un accès | une ligne dans `IMPORT PASS NEW` ; effet au prochain import |
| rafraîchir les données tout de suite | clic sur la date → *Refaire l'import* (associés), ou *Actions → Run workflow* |
| ajouter une année d'objectifs | un onglet `IMPORT OBJECTIFS <année>` dans le classeur, et son nom dans `SHEETS_OBJECTIFS` (`import.R`) |
| changer ce que voit un profil | `PROFILS` dans `R/acces.R`, puis republier |
| changer l'heure de l'import | `HEURE` et `MINUTE` dans le script Apps Script |
| déplacer une source Drive | sa ligne dans le classeur `PATHS` |
| renouveler une clé Odoo | nouvelle clé dans Odoo, puis `ODOO_CLE_API` partout |

---

## Quand ça casse

| Symptôme | Cause probable |
|---|---|
| Aucun mot de passe n'est accepté | l'onglet `IMPORT PASS NEW` a été renommé, ou `Date_fin` est dépassée |
| La date est ambre ou rouge | l'import du matin n'a pas tourné : voir l'onglet *Actions*, le script Apps Script, ou l'expiration d'un jeton |
| L'import échoue sur une table manquante | un onglet du classeur a été renommé ou vidé ; le message nomme la table |
| Erreur d'authentification Drive | `LINK_JSON` injoignable, clé révoquée, ou fichier plus partagé avec le compte de service |
| `Odoo a refusé les identifiants` | mauvaise clé API ou mauvais login |
| `Le serveur Odoo ne trouve pas la base « … »` (ou une erreur `psycopg2` / `PGSQL`) | `ODOO_DB` n'est pas exactement le nom de la base sur cette machine ; comparer avec celle où l'import fonctionne |
| `Odoo a répondu HTTP 404` | `ODOO_URL` fausse |
| Les écritures récentes manquent | elles sont en **brouillon** dans Odoo ; ou l'import a tourné sans les variables Odoo (la trace dit « lecture par fichier ») ; ou le dashboard n'a pas repris le dernier import (clic sur la date → *Reprendre le dernier import*) |
| Un mois de Compta paraît effondré | export incomplet : Compta → Contrôle, puis écarter ou trancher le mois |
| Des produits « sans taux de TVA » dans la trace | leur libellé n'existe pas dans l'export Lightspeed ; ils comptent en TVAC, pas en HTVA |
| Un sélecteur reste vide | défaut de code : voir [développement](developpement.md#pièges-connus) |
| La publication installe de mauvais paquets | `manifest.json` obsolète : `rsconnect::writeManifest()` |

---

## Installer de zéro

1. **Le code**

   ```sh
   git clone https://github.com/tdelc/Mazette-Public.git
   cd Mazette-Public
   mkdir -p outputs
   ```

2. **Les paquets R** (R 4.5 ou proche)

   ```r
   install.packages(c(
     "shiny", "bslib", "shinyjs", "shinyWidgets", "shinybusy", "dplyr",
     "ggplot2", "tidyr", "purrr", "stringr", "tibble", "lubridate",
     "googledrive", "googlesheets4", "readxl", "scales", "plotly", "forecast",
     "DT", "zoo", "patchwork", "cli", "httr", "jsonlite", "calendar",
     "janitor", "htmlwidgets"))
   ```

3. **Le compte de service Google** : dans la console Google Cloud, créer un
   compte de service, activer les API Drive et Sheets, générer une clé JSON,
   la déposer à une URL privée (`LINK_JSON`), et partager chaque fichier avec
   l'adresse du compte.
4. **Le classeur `PATHS`**, puis les **variables d'environnement** dans un
   `.Renviron` à la racine (cf. plus haut).
5. **Premier lancement** :

   ```sh
   Rscript -e 'shiny::runApp(launch.browser = TRUE)'
   ```

   Sans aucun cache, l'import complet démarre (quelques minutes) ; un
   `.RData` apparaît ensuite dans `outputs/` et sur Drive.
6. **Publier** sur Posit Connect, déclarer les variables, puis mettre en
   place [l'import quotidien](#limport-quotidien).
