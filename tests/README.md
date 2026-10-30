# `tests/` — la suite

```sh
Rscript tests/lancer.R            # tout
Rscript tests/lancer.R odoo       # seulement les fichiers « odoo »
```

Le lanceur sort en erreur s'il reste un `FAIL`, et n'imprime le détail d'un
fichier que lorsqu'il y a quelque chose à lire : une suite verte de six cents
lignes, personne ne la relit, et elle cacherait les trois qui comptent le jour
où elles apparaissent.

Elle tourne aussi toute seule, à chaque poussée
(`.github/workflows/tests.yml`).

## Pas de framework, et pourquoi

Chaque fichier imprime une ligne par assertion, le lanceur compte. Les paquets
lourds du dashboard (shiny, plotly, DT) sont remplacés par des **doublures
inertes** : on y éprouve du *calcul*, pas du rendu. Le test tourne donc en une
seconde, partout, sans installer la moitié du CRAN.

## Deux règles qui ont coûté cher

1. **Chaque fichier tourne dans son propre environnement.** Une doublure
   définie dans l'un ne doit pas contaminer le suivant. Conséquence : un test
   doit sourcer le dashboard avec `source(..., local = TRUE)`, sinon son
   `source()` retombe dans l'environnement global et les fonctions chargées n'y
   voient plus les constantes définies juste au-dessus d'elles.

2. **Un test serveur ne prouve pas qu'une sortie s'affiche.** Lire un output
   dans `testServer()` le force à se calculer ; dans un navigateur, une sortie
   masquée est *suspendue* et ne se calcule jamais. Trois défauts de cette
   famille ont traversé la suite sans qu'une assertion bronche — d'où
   `test_zones_alerte.R`, et d'où les vérifications au navigateur qui
   accompagnent chaque changement d'interface.

## Ce que couvre la suite

| Fichier | Ce qu'il éprouve |
|---|---|
| `test_donnees.R` | la normalisation des tickets, la jointure au référentiel |
| `test_theme.R` | les formats d'affichage, et le tri des tableaux |
| `test_plan_comptable.R` | le classement des comptes par leur numéro |
| `test_exploitation.R` | du CA à la marge, la vraisemblance, le topo des mois |
| `test_exploitation_srv.R` | la chaîne réactive, sur le **vrai** bloc de `server.R` |
| `test_analyse.R` | le pont de marge, le rabotage, les alertes d'intégrité |
| `test_ecritures.R` | le grand livre, ses filtres, le forage |
| `test_odoo.R` / `test_odoo_api.R` | la lecture des écritures, par fichier et par API |
| `test_import_odoo.R` | le branchement dans `import.R`, sur le vrai bloc |
| `test_details.R`, `test_planning.R`, `test_sources_travail.R` | les volets |
| `test_zones_alerte.R` | que toute zone d'alerte est bien réveillée |
| `test_ui_onglets.R` | que chaque onglet se construit encore |

Les assertions portent sur ce qui a **déjà été faux au moins une fois** : la
duplication des lignes de caisse, le 6 % compté en boisson, `format_CA` qui
rendait « NA€ », le tri lexicographique, l'ordre des arguments de
`summarise()`, le prorata du coût de paie, le bouclage du pont. Chaque
commentaire dit la raison d'être du test, jamais sa paraphrase.

`fixtures/` ne contient que des données **synthétiques** : le dépôt est public.
