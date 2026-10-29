# format_CA et le tri des tableaux : deux endroits ou l'erreur est SILENCIEUSE.
suppressMessages({library(stringr); library(tibble)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/theme.R", encoding = "UTF-8", local = TRUE)

# ---------- format_CA ----------
ok("un montant ordinaire", format_CA(1234.4) == "1.234€")
ok("les decimales quand on les demande", format_CA(12.345, 2) == "12,35€")
# format() bascule en scientifique des qu'elle est plus COURTE : « 1e+05€ ».
# Le piege ne frappe QUE les nombres ronds, et seulement en appel scalaire.
ok("un nombre rond ne part pas en notation scientifique",
   format_CA(100000, -1) == "100.000€")
ok("ni un million", format_CA(1000000, -1) == "1.000.000€")
ok("zero ne s'affiche pas : c'est la convention des tableaux", format_CA(0) == "")
# « NA€ » se lit comme un montant. Le detail d'un ticket en etait plein.
ok("une valeur inconnue donne un tiret, pas NA€", format_CA(NA_real_) == "—")
ok("et dans un vecteur aussi", identical(format_CA(c(10, NA, 0)), c("10€", "—", "")))
ok("un montant negatif garde son signe", format_CA(-1234) == "-1.234€")

# ---------- les colonnes de tri cachees ----------
# L'arithmetique d'indices est silencieuse quand elle se trompe : une cible
# decalee trie la mauvaise colonne, sans erreur.
tbl <- tibble(Jour = "01/10", Ticket = 431L, `CA HTVA` = "90€",
              `.tri:Jour` = 20000, `.tri:CA HTVA` = 90)
d <- defs_tri(tbl)
ok("trois instructions : une de masquage, deux d'ordre", length(d) == 3)
ok("les jumelles sont cachees",
   identical(d[[1]]$targets, list(3L, 4L)) && isFALSE(d[[1]]$visible))
ok("les jumelles ne sont pas cherchables", isFALSE(d[[1]]$searchable))
# LA regression a ne pas refaire. Interdire le tri sur une jumelle semblait
# gratuit — une colonne cachee n'a pas d'en-tete a cliquer. Mais quand
# DataTables aplatit une consigne de tri (_fnSortFlatten), il ECARTE les
# colonnes source non triables : la consigne entiere tombe, et le tableau ne
# se trie plus du tout. L'en-tete affiche sa fleche, l'ordre est enregistre,
# et rien ne bouge. Verifie au navigateur ; aucun test serveur ne pouvait le
# voir, puisque le defaut est dans DataTables, pas dans ce qu'on lui envoie.
ok("mais elles restent TRIABLES, sinon le tri qu'elles pilotent ne part pas",
   is.null(d[[1]]$orderable))
ok("Jour (colonne 0) s'ordonne sur sa jumelle (colonne 3)",
   d[[2]]$targets == 0 && d[[2]]$orderData == 3)
ok("CA (colonne 2) s'ordonne sur sa jumelle (colonne 4)",
   d[[3]]$targets == 2 && d[[3]]$orderData == 4)
ok("les indices sont en base 0, comme DataTables les attend",
   all(unlist(lapply(d, function(x) unlist(x$targets))) < ncol(tbl)))
ok("une table sans jumelle n'a aucune instruction", is.null(defs_tri(tbl[, 1:3])))
# Une jumelle orpheline doit etre cachee sans viser un indice absent.
o <- defs_tri(tibble(Jour = "01/10", `.tri:Disparue` = 1))
ok("une jumelle orpheline est cachee sans instruction d'ordre",
   length(o) == 1 && identical(o[[1]]$targets, list(1L)))
ok("le nom de jumelle se fabrique par col_tri", col_tri("CA HTVA") == ".tri:CA HTVA")
# Le cas qui a trompe la lecture : « le plus gros ticket fait 90 € ».
ok("le tri texte et le tri numerique ne donnent pas le meme maximum",
   { t <- c("90€", "1.234€"); v <- c(90, 1234)
     order(t, decreasing = TRUE)[1] == 1 && order(v, decreasing = TRUE)[1] == 2 })
