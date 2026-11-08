# Trancher un mois douteux

## Le problème

Le topo des mois (onglet *Compta → Contrôle*) juge les chiffres, et il ne sait
juger qu'eux. Un mois dont la rémunération a chuté de 60 % est « à risque », et
il le restera à chaque ouverture du dashboard — y compris après qu'on a ouvert
Odoo, regardé, compris que c'était un décalage de paie, et conclu que les
chiffres sont bons.

Le verdict ne retenait rien du travail de vérification. L'alerte restait rouge
pour toujours, et une alerte qui ne s'éteint jamais finit par ne plus rien
vouloir dire.

## Ce qui a été ajouté

Un bouton **Trancher un mois**, à côté de *Écarter les mois à risque*. Il ouvre
un dialogue : on choisit le mois, on dit ce qu'on a trouvé, on commente.

Trois décisions possibles :

| Décision | Pastille | Effet |
|---|---|---|
| Les chiffres sont bons | vert, « Validé à la main » | le mois sort des alertes d'intégrité et n'est plus proposé à l'écartement |
| Les chiffres sont faux | rouge, « Écarté à la main » | le mois reste proposé à l'écartement, **même devenu vieux** |
| Annuler la décision | — | le mois retrouve son état calculé |

La colonne *Pourquoi* du tableau porte alors qui a tranché, quand, et le
commentaire. Le résumé en haut d'écran compte les mois tranchés à la main.

### Écarter et trancher ne sont pas la même chose

C'est la distinction qui porte tout le mécanisme.

- **Écarter** est une décision de *lecture* : elle retire un mois des autres
  volets, elle est locale à la session, et elle est perdue à la fermeture.
- **Trancher** est un *constat de vérification* : quelqu'un est allé voir. Ça
  survit à la session, au processus, et à l'import.

## Le classeur

Les décisions vont dans un Google Sheet, désigné par la variable
d'environnement `PATH_VALIDATION_COMPTA` (son identifiant ou son URL).

C'est un **journal**, pas un état : une ligne par décision, jamais de
modification ni de suppression. Changer d'avis ajoute une ligne ; la décision
qui compte pour un mois est simplement la dernière le concernant. On garde
ainsi l'historique des vérifications, qui est l'essentiel de ce qu'on cherchait
à ne pas perdre.

| Colonne | Contenu |
|---|---|
| `MOIS` | le premier du mois, `AAAA-MM-JJ` |
| `DECISION` | `valide`, `rejete` ou `revoque` |
| `COMMENTAIRE` | ce qui a été vérifié |
| `QUI` | le nom de la personne connectée |
| `QUAND` | `AAAA-MM-JJ HH:MM:SS` — en ISO pour que l'ordre du texte soit l'ordre du temps |
| `CA` | le chiffre d'affaires du mois **au moment de la décision** |
| `N_COMPTES` | le nombre de comptes mouvementés, au même moment |

Le classeur peut partir vide : la première décision y pose l'en-tête. Il reste
modifiable à la main — une ligne sans mois lisible ou sans décision connue est
ignorée, et l'import dit combien.

### Le compte de service doit pouvoir ÉCRIRE

C'est la seule chose à ne pas oublier. Les autres classeurs sont partagés en
*lecture* avec le compte de service ; celui-ci doit l'être en **Éditeur**, sans
quoi l'enregistrement échoue — avec un message à l'écran, et sans que la
pastille verdisse (voir plus bas pourquoi).

## Une décision porte sur des chiffres, pas sur un nom de mois

« Octobre est bon » ne veut rien dire en soi : ce qui a été vérifié, c'est un CA
et un nombre de comptes, à une date. Si l'export Odoo du lendemain apporte
quinze écritures de plus, le mois validé n'est plus celui qu'on a regardé.

La décision enregistre donc les deux chiffres qu'elle a vus, et **cesse de
s'appliquer** s'ils ont bougé de plus d'un euro (ou si le nombre de comptes a
changé). Le mois retrouve alors son état calculé, et la colonne *Pourquoi* le
dit : « la décision du 8 nov. 2026 ne porte plus : les chiffres ont changé
depuis ».

Sans ce garde-fou, une validation deviendrait avec le temps un tapis sous lequel
balayer. Avec lui, elle reste ce qu'elle est : un constat daté.

Un cas est traité à l'envers, à dessein : si les colonnes `CA` ou `N_COMPTES`
sont vides (classeur rempli à la main), on ne peut rien comparer, donc on ne
révoque rien. Révoquer en silence des décisions valables serait pire.

## Deux choix qui peuvent surprendre

**Le mois en cours ne se tranche pas.** Il est partiel par construction : le
valider n'aurait aucun sens aujourd'hui, et la décision tomberait demain de
toute façon puisque ses chiffres auront bougé.

**Un mois rejeté reste dans les alertes d'intégrité.** Ses chiffres sont bien
improbables — c'est confirmé, pas résolu. Le masquer rendrait l'alerte muette
sur un vrai problème de source. Seuls les mois *validés* en sortent : c'est
précisément l'objet de la validation.

## Si l'écriture échoue

Rien ne change à l'écran, et un message le dit. C'est volontaire : une pastille
verte sur une décision qui n'est pas dans le classeur disparaîtrait au prochain
import, et personne ne saurait pourquoi. Mieux vaut un refus visible qu'un
succès qui s'évapore.

## Où c'est écrit

| Fichier | Rôle |
|---|---|
| `R/validation.R` | le journal, les décisions, le dialogue |
| `R/exploitation.R` | les deux nouveaux états du topo (`valide`, `rejete`) |
| `import.R` | relit le classeur à chaque import (`DB_VALIDATION_COMPTA`) |
| `server.R` | le bouton, l'enregistrement, et le partage entre sessions |
| `tests/test_validation.R` | 71 assertions, dont la révocation automatique |

Une décision prise dans une session verdit la pastille dans **toutes** les
sessions ouvertes, sans attendre l'import du lendemain : le journal est un état
partagé par le processus, comme la fraîcheur des données.
