# Mazette

Tableau de bord de la brasserie Mazette, en R / Shiny. Il répond à quatre
questions, dans cet ordre : **qu'est-ce qui se passe maintenant**, **qu'est-ce
qui s'est passé**, **combien ça a rapporté**, et **qu'est-ce qu'on prévoit**.

## Comment ça tourne

```
global.R   charge les paquets, les couleurs, puis TOUT le contenu de R/
             ↓
import.R   lit les sources (Google Sheets, Drive, Odoo) et construit les DB_*
             ↓
ui.R       une fonction ui_*() par onglet ; R/acces.R décide lesquels insérer
server.R   une section par onglet, dans le même ordre que la barre
```

Les données ne sont **jamais** calculées dans le serveur : l'import produit des
tables `DB_*`, le serveur ne fait que les lire et les mettre en forme. Un
chiffre qui change doit donc se corriger dans `import.R` ou dans `R/`, jamais
dans un `render*`.

`R/connect.R` décide de ce qui survit entre deux sessions : `TABLES_DASHBOARD`
est le contrat minimum, `TABLES_OPTIONNELLES` le sas par lequel une nouvelle
source entre sans bloquer l'import de tout le reste.

## Les onglets

| Onglet | Ce qu'on y cherche |
|---|---|
| **Maintenant** | la veille, la semaine en cours, l'avancement du mois |
| **Ventes** | la loupe sur une période : tickets, produits, paniers, heures |
| **Historique** / **Année** | la série longue, et l'année face à son objectif |
| **Boissons** / **Nourriture** | la consommation par catégorie, les fûts, les focaccias |
| **Simulation** | l'effet d'une variation de prix |
| **Compta** | six sous-onglets, dans l'ordre des questions — voir ci-dessous |
| **Travail** | heures posées, productivité, et les trois sources de coût |
| **Planning** | les heures à venir, face aux heures habituelles et à l'objectif |
| **Réservations** / **Comparaison** | salle et terrasse ; comparaison libre de périodes |

Le volet **Compta** suit son propre fil : *Contrôle* (puis-je me fier à ces
chiffres), *Exploitation* (combien), *Écarts* (pourquoi ça a bougé),
*Évolution* (où ça va), *Comptabilité générale* (le compte de résultat),
*Écritures* (d'où ça sort). Une alerte du premier mène d'un clic au dernier.

## Lancer

```sh
Rscript -e 'shiny::runApp()'      # l'application
Rscript tests/lancer.R            # toute la suite de tests
Rscript tests/lancer.R analyse    # seulement les fichiers « analyse »
Rscript essai_odoo_api.R          # vérifier la connexion Odoo, sans rien écrire
```

## Les répertoires

| Dossier | Contenu | Détail |
|---|---|---|
| [`R/`](R/) | toute la logique, un fichier par volet ou par sujet | [`R/README.md`](R/README.md) |
| [`tests/`](tests/) | la suite de tests, sans framework | [`tests/README.md`](tests/README.md) |
| [`docs/`](docs/) | la documentation longue | [`docs/README.md`](docs/README.md) |
| [`www/`](www/) | la feuille de style servie au navigateur | [`www/README.md`](www/README.md) |
| [`outputs/`](outputs/) | le cache local des données — jamais versionné | [`outputs/README.md`](outputs/README.md) |
| `logos/` | les images de l'interface |

## Pour aller plus loin

- **[`docs/index.html`](docs/index.html)** — la documentation complète : ce que
  fait l'application, d'où viennent ses données, et un volet *Administration*
  qui explique comment la remonter de zéro. Elle s'ouvre par double-clic, sans
  serveur ni build, et propose deux niveaux de lecture.
- **[`docs/acces.md`](docs/acces.md)** — les mots de passe, les profils, et ce
  qui se passe côté serveur quand un onglet est interdit.
- **[`docs/odoo.md`](docs/odoo.md)** — brancher la comptabilité Odoo : ce qu'il
  faut exporter, le contrat que `DB_COMPTA` doit respecter, et l'API.

## Les branches

`sql` est la branche par défaut sur GitHub, `claude/compta-armature` porte le
développement en cours. Une fonctionnalité vit sur sa propre branche le temps
d'être éprouvée, puis rejoint la branche de développement, puis la branche
principale.
