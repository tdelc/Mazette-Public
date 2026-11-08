# L'import de la nuit, et la fraîcheur affichée

## Le problème qu'on résout

Jusqu'ici, le dashboard réclamait au démarrage le cache de la **veille**,
nommément : `R_new_env_<hier>.RData`. S'il ne le trouvait ni en local ni sur
Drive, il refaisait tout l'import — lecture d'une dizaine de classeurs Drive,
interrogation d'Odoo, reconstruction d'une quinzaine de tables. Quelques
minutes.

Ce coût tombait toujours sur la même personne : **la première connectée du
matin**. Les suivantes trouvaient le cache tout chaud.

Et il tombait au pire moment : l'import s'exécutait *dans* le processus Shiny,
qui n'a qu'un fil d'exécution. Pendant ces minutes-là, le dashboard ne
répondait à personne.

## Ce qui a changé

Trois choses, qui ne valent qu'ensemble.

### 1. Le dashboard ouvre sur le dernier cache, quelle que soit sa date

`connexion_ou_creation()` (cf. `R/connect.R`) ne réclame plus un nom de
fichier : il prend le plus récent de ceux qui existent.

```
cache local le plus frais ─┐
                           ├─ le plus récent gagne ─→ chargement
cache Drive le plus frais ─┘
                               (aucun des deux ? alors, et alors
                                seulement, l'import complet)
```

À égalité de date, le local gagne : même contenu, autant s'épargner le
téléchargement. Drive injoignable : le local sert, même vieux — un dashboard
qui montre les chiffres d'avant-hier **en le disant** vaut mieux qu'un
dashboard qui ne s'ouvre pas.

L'import complet ne tourne donc plus jamais au démarrage, sauf au tout premier
lancement d'une installation neuve.

### 2. La date des données est affichée en permanence

C'est la contrepartie, et elle n'est pas négociable. « Le dashboard est
ouvert » ne vaut plus « les données sont d'hier », et aucun graphique ne
trahit son âge : un chiffre d'affaires de mardi ressemble exactement à un
chiffre d'affaires de mardi, qu'il ait été lu cette nuit ou il y a cinq jours.

La pastille de la barre de navigation (`R/fraicheur.R`) porte la date du
dernier jour connu, et sa couleur dit le retard :

| Retard | Couleur | Ce que ça veut dire |
|---|---|---|
| 0 jour | beige | le cache est d'hier, tout va bien |
| 1 à 3 jours | ambre | l'import de cette nuit n'a pas tourné |
| 4 jours et plus | rouge saumon | ce n'est plus un accident, allez voir |

Le 21 octobre a montré le prix du silence : l'import échouait chaque matin,
le dashboard affichait sans broncher des chiffres qui ne bougeaient plus, et
personne ne l'a vu pendant plusieurs jours. Un volet entier sert à vérifier la
comptabilité ; il n'y avait rien pour vérifier la fraîcheur de l'ensemble.

### 3. Deux boutons, qui ne coûtent pas la même chose

Un clic sur la pastille ouvre le dialogue :

- **Reprendre le dernier import** — quelques secondes. Télécharge le fichier
  déposé sur Drive par l'import de la nuit. C'est le geste courant : le
  travail a déjà été fait, le dashboard ne le savait pas.
- **Refaire l'import** — quelques minutes, et réservé aux associés. Relit
  toutes les sources. C'est le geste à faire après avoir corrigé un classeur.

Dans les deux cas, l'écran se recharge à la fin (et le mot de passe est
redemandé). C'est volontaire : les tables `DB_*` ne sont pas des réactifs, et
les remplacer sous un écran ouvert laisserait des chiffres calculés sur les
anciennes. Les autres écrans ouverts voient leur pastille passer à
« nouvelles données » et peuvent recharger quand ils veulent.

## Où faire tourner l'import ?

Trois pistes ont été pesées.

| | Un PC allumé en permanence | Google Apps Script seul | GitHub Actions |
|---|---|---|---|
| Exécute du R | oui | **non** | oui |
| À entretenir | le PC, l'OS, R, les paquets, l'électricité | rien | rien |
| Sait qu'il a échoué | il faut y penser | — | courriel automatique |
| Déjà en place | non | oui | oui (la suite de tests y tourne) |
| Coût | un PC | nul | nul (dépôt public) |

Le blocage était « Apps Script ne lance pas un `.R` ». C'est vrai, et ce n'est
pas gênant : Apps Script n'a pas besoin d'exécuter le R, il lui suffit de
**demander** à quelqu'un d'autre de l'exécuter. Et ce quelqu'un existe déjà —
l'exécutant GitHub Actions qui fait tourner la suite de tests à chaque
poussée. Il a R, il a le dépôt, il lit les secrets, il tourne un quart d'heure
puis disparaît, et il écrit à l'adresse que le dashboard interroge déjà : le
dossier Drive.

D'où `.github/workflows/import.yml`, qui tourne chaque matin à 6h43 UTC
(7h43 à Bruxelles en hiver) et appelle `import_quotidien.R`.

Le PC allumé en permanence reste une option valable, mais il n'achète rien de
plus ici — et il ajoute une machine à surveiller.

## Ce qu'il faut configurer

### Les secrets du dépôt

Les mêmes valeurs que les variables d'environnement de l'hébergeur Shiny :

```
LINK_JSON       ID_DRIVE_PATHS    ODOO_DB       ODOO_CLE_API
PATH_DRIVE      ODOO_URL          ODOO_LOGIN
```

Elles sont rangées dans un **environnement** GitHub nommé `SECRET`
(*Settings → Environments*), en tant que *secrets* et jamais en tant que
*variables* : une variable est stockée en clair et s'imprime telle quelle
dans les journaux, qui sont publics ici.

Un secret d'environnement n'est lisible que par un job qui **déclare** cet
environnement — d'où la ligne `environment: SECRET` dans le workflow. Sans
elle, `secrets.LINK_JSON` vaut la chaîne vide et l'import échoue sur
« cannot open URL '' » : le secret n'est pas introuvable, il est hors de
portée, et rien ne le dit.

C'est la portée la plus étroite des deux (l'autre étant les *repository
secrets*, lisibles par tous les workflows du dépôt), donc la bonne ici. Elle
se resserre encore en limitant l'environnement à la seule branche `main` :
*Settings → Environments → SECRET → Deployment branches*. Aucun workflow
poussé sur une autre branche ne peut alors lire ces valeurs.

### Les variables de l'hébergeur Shiny

Une seule est nouvelle, et seulement pour le bouton « refaire l'import » :

| Variable | Rôle |
|---|---|
| `GH_TOKEN_IMPORT` | un *fine-grained personal access token*, portée **Actions: write**, sur le seul dépôt `tdelc/Mazette-Public`. Sans lui, le bouton n'est pas proposé en production et l'import tourne sur place en développement. |
| `GH_DEPOT` | facultatif, par défaut `tdelc/Mazette-Public` |
| `GH_REF` | facultatif, par défaut `main` — la branche où le workflow doit être allé |

Le jeton ne doit **jamais** entrer dans le dépôt : il est public.

### Le premier essai

Le workflow doit être sur la branche par défaut pour que la planification le
voie. Une fois poussé : *Actions → Import quotidien → Run workflow*. Le résumé
de l'exécution donne la liste des tables, leur nombre de lignes, la période
couverte et le verdict de chaque contrôle d'import.

## L'horloge d'Apps Script, si la planification de GitHub ne suffit pas

Deux défauts connus de `schedule` :

1. GitHub décale les exécutions planifiées de dix à quarante minutes aux
   heures chargées, et peut en sauter une.
2. Sur un dépôt **public**, GitHub désactive les workflows planifiés après
   60 jours sans commit (il prévient par courriel).

Les deux disparaissent si c'est Apps Script qui donne l'heure :
`workflow_dispatch` est ponctuel et ne s'endort jamais. Un déclencheur
temporel quotidien sur cette fonction suffit :

```javascript
function lancerImportMazette() {
  const depot = 'tdelc/Mazette-Public';
  UrlFetchApp.fetch(
    'https://api.github.com/repos/' + depot + '/actions/workflows/import.yml/dispatches',
    {
      method: 'post',
      contentType: 'application/json',
      headers: {
        Authorization: 'Bearer ' +
          PropertiesService.getScriptProperties().getProperty('GH_TOKEN'),
        Accept: 'application/vnd.github+json'
      },
      payload: JSON.stringify({ ref: 'main' })
    });
}
```

Le jeton va dans les *propriétés du script*, jamais dans le code. Si cette
voie est retenue, on peut retirer le bloc `schedule:` du workflow et ne
garder que `workflow_dispatch:`.

## Ce qui dira que ça a cassé

Trois filets, dans cet ordre :

1. **La pastille du dashboard.** C'est le seul qui parle à quelqu'un qui
   regarde des chiffres. Un import qui ne tourne plus se voit en ambre le
   lendemain, en rouge au bout de quatre jours.
2. **Le courriel de GitHub.** Une exécution planifiée qui échoue est signalée
   au propriétaire du dépôt.
3. **Le résumé de l'exécution.** Il distingue deux échecs que rien ne
   séparait avant : l'import qui plante, et l'import qui réussit sur une
   source qui n'a pas bougé (« le dernier jour connu a N jours de retard sur
   hier »). Le second était exactement le cas du 21 octobre.

## Les journaux d'Actions de ce dépôt sont publics

C'est pour ça que `import_quotidien.R` détourne la trace de l'import vers un
fichier et ne publie qu'un résumé choisi : des noms de tables, des nombres de
lignes, des dates, et le **sujet** des contrôles sans leur détail — un détail
de contrôle comptable porte des montants.

Et c'est pour ça que le workflow ne contient **aucun** `upload-artifact` : le
`.RData` contient toute la comptabilité de la brasserie.

## Ce qui n'a pas pu être vérifié d'ici

Le workflow n'a jamais tourné : il demande les secrets, et je n'y ai pas
accès. Ce qui est éprouvé par la suite de tests, c'est le choix du cache
(`tests/test_connect.R`), l'affichage de la fraîcheur
(`tests/test_fraicheur.R`) et le dialogue avec l'API de GitHub
(`tests/test_import_distant.R`, appel HTTP doublé). Ce qui reste à éprouver
en vrai, c'est la première exécution — d'où le `workflow_dispatch`, qui la
met à un clic.
