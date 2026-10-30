# `outputs/`

Le cache local des données. **Rien ici n'est versionné** (cf. `.gitignore`) :
ce sont les chiffres réels de la brasserie, et le dépôt est public.

`R/connect.R` y dépose un `.RData` horodaté à chaque import réussi, et le
relit au démarrage suivant plutôt que de tout retélécharger. Pour forcer un
import complet : supprimer le fichier, ou appeler `connexion_ou_creation()`
avec `force_dl = TRUE`.

Le dossier doit exister — d'où ce fichier, qui est aussi ce qui le fait
voyager avec le dépôt.
