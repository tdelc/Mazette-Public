# Mazette

Le tableau de bord de la brasserie Mazette, en R / Shiny. Il répond à quatre
questions : **qu'est-ce qui se passe maintenant**, **qu'est-ce qui s'est
passé**, **combien ça a rapporté**, et **qu'est-ce qu'on prévoit**.

## La documentation

| Vignette | Pour qui |
|---|---|
| [Utilisation](docs/utilisation.md) | qui consulte le dashboard : chaque onglet, ce qu'il montre, les astuces |
| [Développement](docs/developpement.md) | qui modifie le code : architecture, fabrication des tables, conventions, pièges, tests |
| [Maintenance](docs/maintenance.md) | qui le fait tourner : Google Drive et Sheets, Odoo, import quotidien, Posit Connect, accès, dépannage |

## En bref

```
global.R   paquets, puis tout R/, puis les constantes partagées
import.R   lit les sources (Google Drive, Odoo, calendriers) et construit les DB_*
ui.R       une fonction ui_*() par onglet
server.R   charge le dernier cache, puis une section par onglet
R/         toute la logique, un fichier par volet ou par sujet
```

L'import tourne chaque matin sur GitHub Actions et dépose un fichier de
données sur Drive ; le dashboard ouvre le plus récent et affiche sa date dans
la barre de navigation.

```sh
Rscript -e 'shiny::runApp()'      # lancer l'application (variables dans .Renviron)
Rscript tests/lancer.R            # la suite de tests
Rscript essai_odoo_api.R          # vérifier la connexion Odoo, sans rien écrire
```

| Dossier | Contenu |
|---|---|
| [`R/`](R/README.md) | la logique |
| [`tests/`](tests/README.md) | la suite de tests, sans framework |
| [`docs/`](docs/README.md) | les trois vignettes |
| [`www/`](www/README.md) | feuille de style, icônes, manifeste d'application |
| [`outputs/`](outputs/README.md) | le cache local des données, jamais versionné |
| `logos/` | les logos des brassins |

## Intégration continue

- `.github/workflows/tests.yml` lance la suite à chaque poussée.
- `.github/workflows/import.yml` refait l'import, déclenché chaque matin par
  Apps Script (cf. [maintenance](docs/maintenance.md#limport-quotidien)).

## Les branches

`main` est la branche publiée, `dev` porte le développement en cours ; une
fonctionnalité vit sur sa propre branche, rejoint `dev`, puis `main`.
