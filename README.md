# Mazette

Tableau de bord de la brasserie Mazette (R / Shiny).

## Accès et mots de passe

Les mots de passe viennent de l'onglet **`IMPORT PASS`** du Google Sheet, comme
avant. Il porte maintenant quatre colonnes de plus, qui décident **des onglets
visibles** — l'accès se joue à l'onglet entier, jamais à l'intérieur d'un onglet.

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
| `invite` | l'accueil et rien d'autre |

Tout le monde a l'**Accueil** : c'est la page d'atterrissage. Ses cartes sont
filtrées, une carte ne s'affiche que si son onglet est autorisé.

Un `role` vide, inconnu ou mal orthographié ne donne que l'accueil : une faute
de frappe ferme la porte, elle ne l'ouvre pas.

### Ce qui se passe côté serveur

Un onglet interdit n'est pas masqué dans le navigateur : il n'est **jamais
inséré**. Seul l'accueil part avec la page ; après la connexion, le serveur
ajoute un à un les onglets autorisés (`nav_insert`). Le HTML des autres ne
quitte pas le serveur et leurs calculs ne sont jamais déclenchés.

### Ajouter un onglet

Une seule ligne, dans la table `ONGLETS` de `R/acces.R` : clé, titre, icône et
nom du constructeur `ui_*()`. La barre de navigation, les droits et (le cas
échéant) la carte d'accueil s'y réfèrent tous.

### Compatibilité

Les quatre nouvelles colonnes sont facultatives. Tant qu'elles n'existent pas
dans le Sheet — ou tant que le `.RData` en cache date d'avant — chaque mot de
passe conserve tous les droits, c'est-à-dire le comportement d'avant.
