# `R/` — toute la logique

`global.R` source **tout** ce répertoire, par ordre alphabétique. Deux
conséquences à connaître :

- une fonction peut en appeler une autre définie plus loin — l'ordre ne compte
  que pour le code exécuté **au chargement** ;
- mais une **constante** de premier niveau, elle, est évaluée tout de suite.
  C'est pourquoi aucune table ici ne peut référencer les couleurs `COUL_*` :
  elles sont définies dans `global.R`, *après* ce répertoire. Une table de
  couleurs se construit donc dans une fonction (`etats_topo()`), jamais au
  chargement.

Chaque fichier s'ouvre sur un commentaire qui dit son rôle et, souvent, le
piège qu'il a coûté. C'est là qu'il faut lire avant de modifier.

## Le socle

| Fichier | Rôle |
|---|---|
| `acces.R` | le catalogue des onglets (`ONGLETS`), les profils, les droits. **Les fonctions `ui_*()` y sont désignées par leur NOM, sous forme de chaîne** : aucune analyse statique ne voit qu'elles servent |
| `connect.R` | ce qui survit entre deux sessions : `TABLES_DASHBOARD` et `TABLES_OPTIONNELLES` |
| `donnees.R` | stockage normalisé des tickets et reconstruction au chargement |
| `helpers.R` | périodes, ratios, bandeaux d'alerte — et `ZONES_ALERTE`, qui évite un piège Shiny coûteux |
| `theme.R` | formats d'affichage (`format_CA`, `euro_compta`), pastilles, et les jumelles de tri des tableaux |
| `kpi.R` | les tuiles de synthèse |
| `date.R`, `box.R`, `produits.R` | utilitaires transverses |

## La comptabilité

| Fichier | Rôle |
|---|---|
| `plan_comptable.R` | classement des comptes par leur **numéro** (`PLAN_PCMN`), compte de résultat |
| `exploitation.R` | du CA à la marge ; le contrôle de vraisemblance et le topo des mois |
| `analyse.R` | le pont de marge, les contributions par compte, les alertes d'intégrité |
| `ecritures.R` | le grand livre, ses filtres, et le forage depuis une alerte |
| `odoo.R` | des écritures Odoo brutes à `DB_COMPTA`, et la couture avec l'ancienne source |
| `odoo_api.R` | la même lecture, par l'API plutôt que par un fichier |
| `couts.R` | coûts matière : table canonique et normalisation des sources |
| `sources_travail.R` | les trois sources d'heures et de coût du travail, et leur réconciliation |

## Les volets

`accueil.R`, `maintenant.R`, `details.R`, `historique.R`, `année.R`,
`boissons.R`, `consommation.R`, `focaccia.R`, `pizzwanze.R`, `brassins.R`,
`simulation.R`, `travail.R`, `planning.R`, `reservations.R`, `comparaison.R`,
`objectifs.R`.

Un fichier par volet, nommé comme son onglet. Il contient les calculs et les
graphiques de ce volet, et rien d'autre : ce qui sert à deux volets remonte
dans le socle.

## `sql.R`

La couche Cloudflare D1 — écrite, testée nulle part, appelée nulle part pour
l'instant. Elle attend son usage.
