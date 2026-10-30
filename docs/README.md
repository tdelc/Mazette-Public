# `docs/`

| Fichier | Ce qu'on y trouve |
|---|---|
| `index.html` | la documentation complète, autonome : elle s'ouvre par double-clic, sans serveur ni build. Deux niveaux de lecture — « j'ai cinq minutes » et « je veux tout savoir » — et un volet *Administration* qui explique comment remonter le dashboard de zéro |
| `acces.md` | les mots de passe, les profils, et ce qui se passe côté serveur quand un onglet est interdit |
| `odoo.md` | brancher la comptabilité Odoo : quoi exporter, le contrat que `DB_COMPTA` doit respecter, et l'API |

`index.html` est aussi la mémoire des **pièges** : les défauts qui n'ont été
trouvés qu'au navigateur, ceux qu'aucun test ne pouvait voir, et la raison de
chaque choix qui a l'air arbitraire. C'est là qu'ils sont racontés — pas dans
l'interface, où ils alourdiraient chaque écran.

Pour publier en ligne : *Settings → Pages*, source *Deploy from a branch*,
dossier `/docs`.
