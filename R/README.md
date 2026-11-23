# `R/` — toute la logique

`global.R` source **tout** ce répertoire, par ordre alphabétique, puis définit
ses propres constantes (couleurs, jours, mois). Deux conséquences :

- une fonction peut en appeler une autre définie plus loin ;
- une **constante** de premier niveau est évaluée tout de suite : elle ne peut
  donc pas utiliser les couleurs `COUL_*` de `global.R`. Une table de couleurs
  se construit dans une fonction (`etats_topo()`).

Chaque fichier s'ouvre sur un commentaire qui dit son rôle.

| Fichiers | Rôle |
|---|---|
| `acces.R` | onglets (`ONGLETS`), profils (`PROFILS`), droits. Les fonctions `ui_*()` y sont désignées par leur **nom**, en chaîne |
| `connect.R` | quel cache charger, et les tables enregistrées (`TABLES_DASHBOARD`, `TABLES_OPTIONNELLES`) |
| `fraicheur.R`, `import_distant.R` | la date des données ; le déclenchement de l'import sur GitHub |
| `donnees.R` | stockage réduit des tickets, nomenclature et TVA des produits |
| `date.R` | calendrier de référence et `parse_date_souple()`, LE parseur de dates |
| `helpers.R`, `theme.R`, `kpi.R`, `box.R`, `objectifs.R` | formats, tri des tableaux, tuiles, couleurs d'objectif, zones d'alerte |
| `maintenant.R`, `details.R`, `historique.R`, `annee.R`, `comparaison.R`, `simulation.R`, `reservations.R` | les volets du même nom (`details.R` = Ventes) |
| `boissons.R`, `brassins.R`, `consommation.R`, `produits.R`, `focaccia.R`, `pizzwanze.R` | boissons, fûts, nourriture |
| `travail.R`, `sources_travail.R`, `planning.R` | heures, coût du travail et ses trois sources, planning |
| `plan_comptable.R`, `exploitation.R`, `analyse.R`, `ecritures.R`, `projection.R`, `validation.R`, `couts.R` | le volet Compta |
| `odoo.R`, `odoo_api.R` | des écritures Odoo à `DB_COMPTA` |

Ce qui sert à deux volets vit dans le socle ; ce qui ne sert plus s'en va.
Voir [`docs/developpement.md`](../docs/developpement.md).
