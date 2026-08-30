# Mazette

Tableau de bord de la brasserie Mazette (R / Shiny).

## Documentation

**[`docs/index.html`](docs/index.html)** — la documentation complète : ce que fait
l'application, d'où viennent ses données, la gestion des accès, et un volet
**Administration** qui explique comment remonter le dashboard de zéro.

La page est autonome : elle s'ouvre par double-clic, sans serveur ni build. Elle
propose deux niveaux de lecture, « j'ai cinq minutes » et « je veux tout savoir ».
Pour la publier en ligne : *Settings → Pages*, source *Deploy from a branch*,
dossier `/docs`.

Ce README ne garde que le résumé de la gestion des accès, qui change souvent.

## Accès et mots de passe

Les mots de passe viennent d'un onglet du Google Sheet, comme avant —
désormais **`IMPORT PASS NEW`**, refait à part pour ne pas casser l'ancien (le
nom est défini une fois, dans `SHEET_PASS` en tête d'`import.R`). Il porte
quatre colonnes de plus, qui décident **des onglets visibles** — l'accès se
joue à l'onglet entier, jamais à l'intérieur d'un onglet.

| Colonne | Rôle | Exemple |
|---|---|---|
| `Date_debut`, `Date_fin` | période de validité (inchangé) | `01/09/2026` → `31/12/2026` |
| `pass` | le mot de passe (inchangé) | `mazette2026` |
| `nom` | à qui il appartient — affiché en haut à droite une fois connecté | `Boris` |
| `role` | le profil, qui donne une liste d'onglets | `gestion` |
| `onglets` | onglets **en plus** du profil, ou `*` pour tout. Vide dans le cas général | `compta, travail` |
| `actif` | `non` coupe l'accès sans effacer la ligne | `oui` |

### Profils

Définis dans `R/acces.R` (constante `PROFILS`) — c'est là qu'on ajuste la
politique, sans toucher au tableur.

| Profil | Onglets |
|---|---|
| `admin` | tout, compta et coût du personnel compris |
| `gestion` | tout l'opérationnel, **sans** la compta générale |
| `equipe` | l'activité, sans les coûts (ni compta, ni masse salariale) |
| `salle` | Maintenant, Fûts, Boissons, Réservations |
| `brasserie` | Maintenant, Fûts, Boissons |
| `public` | Maintenant, Historique, Année — pour présenter à l'extérieur |
| `invite` | l'accueil et rien d'autre |

Tout le monde a l'**Accueil** : c'est la page d'atterrissage. Sa grille ne
contient que les cartes des onglets autorisés — construites, pas masquées,
sinon chaque carte retirée laisserait une cellule vide dans la grille.

Un `role` vide, inconnu ou mal orthographié ne donne que l'accueil : une faute
de frappe ferme la porte, elle ne l'ouvre pas.

### Ce qui se passe côté serveur

Un onglet interdit n'est pas masqué dans le navigateur : il n'est **jamais
inséré**. Seul l'accueil part avec la page ; après la connexion, le serveur
ajoute un à un les onglets autorisés (`nav_insert`). Le HTML des autres ne
quitte pas le serveur et leurs calculs ne sont jamais déclenchés.

Conséquence à connaître en développant : **les inputs d'un onglet n'existent
qu'après son insertion**, donc après la connexion. Les sorties (`render*`) ne
s'en aperçoivent pas — Shiny ne les calcule que lorsqu'elles sont visibles ;
mais un `observe()` tourne dès le premier flush, avant la connexion. D'où deux
règles, selon le sens :

- **Lire** un input d'onglet → le protéger par `req()`, ou prévoir un repli
  explicite comme le fait `sim_periode_val()`. Sinon le calcul part sur `NULL`
  — et `1 + NULL/100` vaut `numeric(0)`, pas `NA`, ce qui produit des erreurs
  de longueur loin de leur cause.
- **Garnir** un input d'onglet (`update*Input`) → attendre `ONGLETS_PRETS()`.
  Sinon le message vise un champ inexistant : il est perdu **sans erreur**, et
  l'observateur ne rejoue jamais. Le sélecteur reste vide pour de bon.

Dans un `observeEvent`, ce garde-fou va dans l'expression déclenchante, pas
dans le corps : le corps est isolé, une dépendance posée là ne rejouerait
jamais l'observateur.

### Se déconnecter

Un bouton à droite de la barre, à côté du nom. Il recharge la page plutôt que
de défaire l'insertion des onglets : c'est la seule façon de garantir qu'il ne
reste rien de la session précédente — ni onglet, ni période saisie, ni prix
simulé. Les données étant chargées une fois par processus R (en tête de
`server.R`, hors de la fonction serveur), le rechargement ne retouche pas au
`.RData`.

### Ajouter un onglet

Une seule ligne, dans la table `ONGLETS` de `R/acces.R` : clé, titre, icône et
nom du constructeur `ui_*()`. La barre de navigation, les droits et (le cas
échéant) la carte d'accueil s'y réfèrent tous.

### Compatibilité

Les quatre nouvelles colonnes sont facultatives. Tant qu'elles n'existent pas
dans le Sheet — ou tant que le `.RData` en cache date d'avant — chaque mot de
passe conserve tous les droits, c'est-à-dire le comportement d'avant.
