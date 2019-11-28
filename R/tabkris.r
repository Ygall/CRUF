#' TABKRIS
#'
#' @param baz nom de table R
#' @param vect.var liste des noms de variables entre "" s?par?es par des virgules
#' @param vect.quali liste des types de variables s?par?es par des virgules
#'                     type=1 si     variable qualitative
#'                     type=0 sinon  (variable quantitative)
#' @param varint Variables d'intérêt
#' @param valvarint Liste des valeurs de la variable d'intérêt que l'on souhaite afficher dans le tableau
#' @param nomvarint Nom des valeurs de la variable d'intérêt
#' @param test Vecteurs des tests à passer dans la fonction TABKRIS
#' @param vecnoms Vecteur des noms des variables
#' @param valeurs Listes des valeurs que l'on souhaite afficher
#' @param vecrefs Modalité de référence pour les variables qualitatives
#' @param varassoc Variable imbriqué
#' @param codassoc Code de la variable imbriquée
#' @param pres   Nombre de chiffres significatifs
#' @param langue Exit language, "en", "fr"
#' @param digits Nombre de décimales dans le tableau
#'
#' @return Rend une data frame contenant les informations de description
#' @export
#'
#'
TABKRIS <-
  function(baz,
           vect.var,
           vect.quali,
           varint = NULL,
           valvarint = NULL,
           nomvarint = NULL,
           test = NULL,
           vecnoms = NULL,
           valeurs = NULL,
           vecrefs = NULL,
           varassoc = NULL,
           codassoc = NULL,
           pres = NULL,
           langue = "fr",
           digits = 2)
##
## descriptif variables qualitatives+quantitatives
## baz   :
## vect.var   : liste des noms de variables entre "" s?par?es par des virgules
## vect.quali   : liste des types de variables s?par?es par des virgules
##                      type=1 si     variable qualitative
##                      type=0 sinon  (variable quantitative)
## varint : nom de la variable de stratification entre ""
## valvarint : liste des valeurs que l'on souhaite afficher dans le tableau concernant la variable d'int?r?t
## nomvarint : nom de la variable d'int?r?t que l'on souhaite utiliser pour l'affichage
##              (bras de randomisation, sachant qu'on utilise ensuite valvarint pour nommer les groupes de randomisation)
## test : liste des tests que l'on souhaite effectuer entre ""  s?par?s par des virgules
##                       variables quali :  "fisher", "chisq" ou "mcnemar"
##                       variables quanti : "t", "wilcox","aov","kruskal"
## !!! on peut d?cider d'effectuer uniquement certains tests il faut alors songer ? mettre "" pour les tests que l'on ne souhaite pas r?aliser
## vecnoms : liste des noms que l'on souhaite utiliser pour la sortie tableau, entre "" s?par?s par des virgules
## valeurs (utiles pour les quali et les quanti) :
## liste des valeurs que l'on souhaite utiliser pour la sortie tableau, entre "" s?par?es par des virgules
##                      exemple : si la variable est cod?e 1/2 on veut afficher Masculin/F?minin
##                      permet aussi d'afficher les unit?s pour les avriables quantitatives
## vecrefs (utiles pour les quali) : liste des r?f?rences que l'on souhaite utiliser pour la sortie tableau
##                      exemple "Masculin" pour le sexe
## !!!! de bien d?finir une r?f?rence qui existe dans le vecteur des valeurs de la variable
## !!!! il faut songer ? d?finir une r?f?rence vide pour les variables quantitatives
## varassoc : liste des variables "m?res" qui impliquent des r?ponses imbriqu?es
##             exemple : TABAC (oui/non) est une variable de ce type pour la variable type de tabagisme (sevr?/actif)
##              mettre "" si pas de variable "m?re" pour la variable de vect.var
## codassoc : code qui devait conduire ? la r?ponse d'une variable dite imbriqu?e
##              exemple : codassoc pour TABAC == "Oui"
## pres : liste des pr?cisions que l'on souhaite utiliser pour les variables quantitatives
##                     "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean (sd)), "mean_mm" (mean(min;max))
## !!!! le vecteur entr? ici doit avoir la taille du vecteur de variables vect.var
## !!!! il faut donc penser ? mettre "" pour les variables qualitatives
## langue : permet de choisir la langue de sortie du tableau
##                    "fr" = FRANCAIS; "an" ou "en" = ANGLAIS
##
## utilise les fonctions qualitbis et quantitbis
##
####NB : on peut entrer une variable d'int?r?t qui contiendrait des NA
#### seulement dans le tableau de sortie on ne retrouve pas les infos de cette colonne ...
  {
    if (!is.null(varassoc))
    {
      print(
        "Vous avez annonc? des variables dites -m?res-, pensez ? v?rifier au pr?alable la coh?rence de vos imbrications, si des ?l?ments sont discordants cela risque de faire planter la fonction TABKRIS"
      )
      print(
        "par exemple, si vous avez variable-m?re- = non Remplie ou Non mais variable-imbriqu?e- remplie ..."
      )
    }

    if (!is.null(test) & is.null(varint))
    {
      print("Vous tentez d'effectuer un test sur une population Single Arm.")
      return()
    }
    if (langue %in% c("fr", "FR", "Fr", "fR"))
    {
      langue = "fr"
    }
    if (langue %in% c("en", "EN", "an", "AN", "An", "En", "eN", "aN"))
    {
      langue = "en"
    }
    if (!(langue %in% c("fr", "an", "en")))
    {
      cat(
        "Vous avez choisi une langue que je ne connais pas !! \n Le fran?ais sera pris par d?faut \n"
      )
      langue = "fr"
    }

    noms <- names(baz)
    if (is.null(pres))
    {
      pres <- rep("med", length(vect.var))
    }
    if (length(pres) == 1)
    {
      pres2 <- rep(pres, length(vect.var))
      pres <- pres2
    }
    if (!is.null(vecrefs)) {
      vecrefs[vecrefs == ""] <- NA
    }
    if (!is.null(test)) {
      test[test == ""] <- NA
    }
    if (!is.null(pres)) {
      pres[pres == ""] <- NA
    }
    if (!is.null(valeurs)) {
      valeurs[valeurs == ""] <- NA
    }
    if (!is.null(varassoc)) {
      varassoc[varassoc == ""] <- NA
    }
    if (!is.null(codassoc)) {
      codassoc[codassoc == ""] <- NA
    }

    if (is.null(varassoc)) {
      varassoc <- rep(NA, length(vect.var))
    }

    vecnoms[vecnoms == ""] <- vect.var[vecnoms == ""]

    lgtest <- ifelse(is.null(test), 0, 1)

    if (is.null(varint))
    {
      effect <- dim(baz)[1]
      i <- 1
      if (is.na(varassoc[i]))
      {
        if (vect.quali[i] == 1)
        {
          #digits <- 1
          levvar <- levels(as.factor(baz[, vect.var[i]]))
          if (!is.null(vecrefs))
          {
            if (is.null(valeurs))
            {
              if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
              {
                cat(
                  "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                  vecrefs[i],
                  " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    baz = baz,
                    ref = ref,
                    nom = vecnoms[i],
                    digits = digits
                  )
              }
            } else {
              if (!is.element(vecrefs[i], valeurs[i:length(levvar)]) &
                  !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    baz = baz,
                    ref = ref,
                    nom = vecnoms[i],
                    valeurs = valeurs[i:length(levvar)],
                    digits = digits
                  )
              }
            }
          } else {
            descript <-
              qualitbis(
                x = vect.var[i],
                baz = baz,
                nom = vecnoms[i],
                valeurs = valeurs[i:length(levvar)],
                digits = digits
              )
          }
        } else {
          if (is.na(pres[i]))
          {
            presi = "med"
          } else {
            presi = pres[i]
          }
          #digits=max(3, getOption("digits")-3)
          descript <-
            quantitbis(
              x = vect.var[i],
              baz = baz,
              nom = vecnoms[i],
              pres = presi,
              val = valeurs[i]
            )
          #descript<-matrix(descript,ncol=4,byrow=F)
        }
      } else {
        indV <- match(noms, varassoc[i])
        Vassoc <- baz[, varassoc[i]]
        Cassoc <- codassoc[i]
        soustable <-
          baz[Vassoc == as.character(Cassoc) &  is.na(Vassoc) == F, ]
        if (vect.quali[i] == 1)
        {
          levvar <- levels(as.factor(soustable[, vect.var[i]]))
          if (!is.null(vecrefs))
          {
            if (is.null(valeurs))
            {
              if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
              {
                cat(
                  "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                  vecrefs[i],
                  " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    baz = soustable,
                    ref = ref,
                    nom = vecnoms[i]
                  )
              }
            } else {
              if (!is.element(vecrefs[i], valeurs[i:length(levvar)]) &
                  !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    baz = soustable,
                    ref = ref,
                    nom = vecnoms[i],
                    valeurs = valeurs[i:length(levvar)]
                  )
              }
            }
          } else {
            descript <-
              qualitbis(
                x = vect.var[i],
                digits = digits,
                baz = soustable,
                nom = vecnoms[i],
                valeurs = valeurs[i:length(levvar)]
              )
          }
        } else {
          if (is.na(pres[i]))
          {
            presi = "med"
          } else {
            presi = pres[i]
          }
          descript <-
            quantitbis(
              x = vect.var[i],
              baz = soustable,
              nom = vecnoms[i],
              pres = presi,
              val = valeurs[i]
            )
        }
      }
      #descript<-matrix(descript,ncol=4,byrow=F)
      if (length(vect.var) >= 2)
      {
        for (i in 2:length(vect.var))
        {
          deb = 0
          for (j in 1:(i - 1))
          {
            if (vect.quali[j] == 1)
            {
              deb = deb + length(levels(as.factor(baz[, vect.var[j]])))
            } else {
              deb = deb + 1
            }
          }
          deb = deb + 1
          if (is.na(varassoc[i]))
          {
            if (vect.quali[i] == 1)
            {
              levvar <- levels(as.factor(baz[, vect.var[i]]))

              #digits=1
              if (!is.null(vecrefs))
              {
                if (is.null(valeurs))
                {
                  if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i])) {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    descript <-
                      rbind(
                        descript,
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          baz = baz,
                          ref = ref,
                          nom = vecnoms[i]
                        )
                      )
                  }
                } else {
                  if (!is.element(vecrefs[i], valeurs[deb:(deb + length(levvar) - 1)]) &
                      !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i])) {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    descript <-
                      rbind(
                        descript,
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          baz = baz,
                          ref = ref,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                      )
                  }
                }
              } else {
                descript <-
                  rbind(
                    descript,
                    qualitbis(
                      x = vect.var[i],
                      digits = digits,
                      baz = baz,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                    )
                  )
              }
            } else {
              if (is.na(pres[i]))
              {
                presi = "med"
              } else {
                presi = pres[i]
              }
              #digits=max(3, getOption("digits")-3)
              descript <-
                rbind(
                  descript,
                  quantitbis(
                    x = vect.var[i],
                    baz = baz,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
                )
            }

          } else {
            indV <- match(noms, varassoc[i])
            Vassoc <- baz[, varassoc[i]]
            Cassoc <- codassoc[i]
            soustable <-
              baz[Vassoc == as.character(Cassoc) &  is.na(Vassoc) == F, ]
            if (vect.quali[i] == 1)
            {
              levvar <- levels(as.factor(soustable[, vect.var[i]]))

              if (!is.null(vecrefs))
              {
                if (is.null(valeurs))
                {
                  if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i])) {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    descript <-
                      rbind(
                        descript,
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          baz = soustable,
                          ref = ref,
                          nom = vecnoms[i]
                        )
                      )
                  }
                } else {
                  if (!is.element(vecrefs[i], valeurs[deb:(deb + length(levvar) - 1)]) &
                      !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i])) {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    descript <-
                      rbind(
                        descript,
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          baz = soustable,
                          ref = ref,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                      )
                  }
                }
              } else {
                descript <-
                  rbind(
                    descript,
                    qualitbis(
                      x = vect.var[i],
                      digits = digits,
                      baz = soustable,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                    )
                  )
              }
            } else {
              if (is.na(pres[i]))
              {
                presi = "med"
              } else {
                presi = pres[i]
              }
              #digits=max(3, getOption("digits")-3)
              descript <-
                rbind(
                  descript,
                  quantitbis(
                    x = vect.var[i],
                    baz = soustable,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
                )
            }
          }
        } #fin du for
        #descript<-matrix(descript,ncol=4,byrow=F)
        descript <- rbind(c("", "", effect, ""), descript)
        descript <-
          suppressWarnings(data.frame(descript, stringsAsFactors = FALSE))
        if (langue == "fr")
        {
          names(descript) <- c("Parametres", "Valeurs", c("N", "Statistiques*"))
          #names(descript)<-descript[1,]
          #descript[1,] <- c("Parametres","Valeurs",c("N","Statistiques*"))
        }
        if (langue == "en")
        {
          names(descript) <- c("Parameters", "Values", c("N", "Statistics*"))
          #names(descript)<-descript[1,]
          #descript[1,] <- c("Parameters","Values",c("N","Statistics*"))
        }

      } else {
        descript <- rbind(c("", "", effect, ""), descript)
        descript <-
          suppressWarnings(data.frame(descript, stringsAsFactors = FALSE))
        if (langue == "fr")
        {
          names(descript) <- c("Parametres", "Valeurs", c("N", "Statistiques*"))
          #names(descript)<-descript[1,]
          #descript[1,] <- c("Parametres","Valeurs",c("N","Statistiques*"))
        }
        if (langue == "en")
        {
          names(descript) <- c("Parameters", "Values", c("N", "Statistics*"))
          #names(descript)<-descript[1,]
          #descript[1,] <- c("Parameters","Values",c("N","Statistics*"))
        }
      }

      return(descript)
    }

    ### avec variable d'int?r?t
    if (!is.null(varint))
    {
      varintind <- match(varint, noms)
      nlev <- length(levels(as.factor(baz[, varintind])))
      lev <- levels(as.factor(baz[, varintind]))
      lev <- lev[lev != ""]
      effect <- table(baz[, varintind])
      i = 1
      if (lgtest == 1)
      {
        if (is.na(test[i]))
        {
          testi <- NULL
        } else {
          testi <- test[i]
        }
      } else{
        testi <- NULL
      }
      if (is.na(varassoc[i]))
      {
        if (vect.quali[i] == 1)
        {
          #digits=1
          levvar <- levels(as.factor(baz[, vect.var[i]]))
          if (!is.null(vecrefs))
          {
            if (is.null(valeurs))
            {
              if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    y = varint,
                    baz = baz,
                    ref = ref,
                    test = testi,
                    nom = vecnoms[i]
                  )
                if (lgtest == 1 & is.null(testi))
                {
                  if (class(descript) == "matrix")
                  {
                    descript <- cbind(descript, rep("", dim(descript)[1]))
                  } else {
                    descript <- cbind(descript, rep("", length(descript)))
                  }
                }
              }
            } else {
              if (!is.element(vecrefs[i], valeurs[i:length(levvar)]) &
                  !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    y = varint,
                    baz = baz,
                    ref = ref,
                    test = testi,
                    nom = vecnoms[i],
                    valeurs = valeurs[i:length(levvar)]
                  )
                if (lgtest == 1 & is.null(testi))
                {
                  if (class(descript) == "matrix")
                  {
                    descript <- cbind(descript, rep("", dim(descript)[1]))
                  } else {
                    descript <- cbind(descript, rep("", length(descript)))
                  }
                }
              }
            }
          } else {
            descript <-
              qualitbis(
                x = vect.var[i],
                digits = digits,
                y = varint,
                baz = baz,
                test = testi,
                nom = vecnoms[i],
                valeurs = valeurs[i:length(levvar)]
              )
            if (lgtest == 1 & is.null(testi))
            {
              if (class(descript) == "matrix")
              {
                descript <- cbind(descript, rep("", dim(descript)[1]))
              } else {
                descript <- cbind(descript, rep("", length(descript)))
              }
            }
          }
        } else {
          if (is.na(pres[i]))
          {
            presi = "med"
          } else {
            presi = pres[i]
          }
          #digits=max(3, getOption("digits")-3)
          descript <-
            quantitbis(
              x = vect.var[i],
              y = varint,
              baz = baz,
              test = testi,
              nom = vecnoms[i],
              pres = presi,
              val = valeurs[i]
            )
          #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
          if (lgtest == 1 & is.null(testi))
          {
            descript <- c(descript, "")
          }
        }
      } else {
        indV <- match(noms, varassoc[i])
        Vassoc <- baz[, varassoc[i]]
        Cassoc <- codassoc[i]
        soustable <-
          baz[Vassoc == as.character(Cassoc) &  is.na(Vassoc) == F, ]
        if (vect.quali[i] == 1)
        {
          #digits=1
          levvar <- levels(as.factor(soustable[, vect.var[i]]))
          if (!is.null(vecrefs))
          {
            if (is.null(valeurs))
            {
              if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    y = varint,
                    baz = soustable,
                    ref = ref,
                    test = testi,
                    nom = vecnoms[i]
                  )
                if (lgtest == 1 & is.null(testi))
                {
                  if (class(descript) == "matrix")
                  {
                    descript <- cbind(descript, rep("", dim(descript)[1]))
                  } else {
                    descript <- cbind(descript, rep("", length(descript)))
                  }
                }
              }
            } else {
              if (!is.element(vecrefs[i], valeurs[i:length(levvar)]) &
                  !is.na(vecrefs[i]))
              {
                print(
                  paste(
                    "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                    vecrefs[i],
                    " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                  )
                )
                print(vect.var[i])
                return()
              } else {
                if (is.na(vecrefs[i]))
                {
                  ref <- NULL
                } else {
                  ref <- vecrefs[i]
                }
                descript <-
                  qualitbis(
                    x = vect.var[i],
                    digits = digits,
                    y = varint,
                    baz = soustable,
                    ref = ref,
                    test = testi,
                    nom = vecnoms[i],
                    valeurs = valeurs[i:length(levvar)]
                  )
                if (lgtest == 1 & is.null(testi))
                {
                  if (class(descript) == "matrix")
                  {
                    descript <- cbind(descript, rep("", dim(descript)[1]))
                  } else {
                    descript <- cbind(descript, rep("", length(descript)))
                  }
                }
              }
            }
          } else {
            descript <-
              qualitbis(
                x = vect.var[i],
                digits = digits,
                y = varint,
                baz = soustable,
                test = testi,
                nom = vecnoms[i],
                valeurs = valeurs[i:length(levvar)]
              )
            if (lgtest == 1 & is.null(testi))
            {
              if (class(descript) == "matrix")
              {
                descript <- cbind(descript, rep("", dim(descript)[1]))
              } else {
                descript <- cbind(descript, rep("", length(descript)))
              }
            }
          }
        } else {
          if (is.na(pres[i]))
          {
            presi = "med"
          } else {
            presi = pres[i]
          }
          #digits=max(3, getOption("digits")-3)
          descript <-
            quantitbis(
              x = vect.var[i],
              y = varint,
              baz = soustable,
              test = testi,
              nom = vecnoms[i],
              pres = presi,
              val = valeurs[i]
            )
          #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
          if (lgtest == 1 & is.null(testi))
          {
            descript <- c(descript, "")
          }
        }
      }

      #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
      if (length(vect.var) >= 2)
      {
        for (i in 2:length(vect.var))
        {
          #print(vect.var[i])
          if (lgtest == 1)
          {
            if (is.na(test[i]))
            {
              testi <- NULL
            } else {
              testi <- test[i]
            }
          }
          deb = 0
          for (j in 1:(i - 1))
          {
            if (vect.quali[j] == 1)
            {
              deb = deb + length(levels(as.factor(baz[, vect.var[j]])))
            } else {
              deb = deb + 1
            }
          }
          deb = deb + 1
          if (is.na(varassoc[i]))
          {
            if (vect.quali[i] == 1)
            {
              levvar <- levels(as.factor(baz[, vect.var[i]]))
              #digits=1
              if (!is.null(vecrefs))
              {
                if (is.null(valeurs))
                {
                  if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i]))
                    {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    if (lgtest == 1 & is.null(testi))
                    {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = baz,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i]
                        )
                      if (class(tmp) == "matrix")
                      {
                        tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                      } else {
                        tmp <- c(tmp, "")
                      }
                    } else {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = baz,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i]
                        )
                    }
                    descript <- rbind(descript, tmp)
                  }
                } else {
                  if (!is.element(vecrefs[i], valeurs[deb:(deb + length(levvar) - 1)]) &
                      !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i]))
                    {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    if (lgtest == 1 & is.null(testi))
                    {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = baz,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                      if (class(tmp) == "matrix")
                      {
                        tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                      } else {
                        tmp <- cbind(tmp, rep("", length(tmp)))
                      }
                    } else {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = baz,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                    }
                    descript <- rbind(descript, tmp)
                  }
                }
              } else {
                if (lgtest == 1 & is.null(testi))
                {
                  tmp <-
                    qualitbis(
                      x = vect.var[i],
                      digits = digits,
                      y = varint,
                      baz = baz,
                      test = testi,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                    )
                  if (class(tmp) == "matrix")
                  {
                    tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                  } else {
                    tmp <- cbind(tmp, rep("", length(tmp)))
                  }
                } else {
                  tmp <-
                    qualitbis(
                      x = vect.var[i],
                      digits = digits,
                      y = varint,
                      baz = baz,
                      test = testi,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                    )
                }
                descript <- rbind(descript, tmp)
              }
            } else {
              #digits=max(3, getOption("digits")-3)
              if (is.na(pres[i]))
              {
                presi = "med"
              } else {
                presi = pres[i]
              }

              if (lgtest == 1 & is.null(testi))
              {
                tmp <-
                  quantitbis(
                    x = vect.var[i],
                    y = varint,
                    baz = baz,
                    test = testi,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
                tmp <- c(tmp, "")

              } else {
                tmp <-
                  quantitbis(
                    x = vect.var[i],
                    y = varint,
                    baz = baz,
                    test = testi,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
              }

              descript <- rbind(descript, tmp)
            }
          } else {
            indV <- match(noms, varassoc[i])
            Vassoc <- baz[, varassoc[i]]
            Cassoc <- codassoc[i]
            soustable <-
              baz[Vassoc == as.character(Cassoc) &  is.na(Vassoc) == F, ]
            if (vect.quali[i] == 1)
            {
              levvar <- levels(as.factor(soustable[, vect.var[i]]))
              #digits=1
              if (!is.null(vecrefs))
              {
                if (is.null(valeurs))
                {
                  if (!is.element(vecrefs[i], levvar) & !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i]))
                    {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    if (lgtest == 1 & is.null(testi))
                    {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = soustable,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i]
                        )
                      if (class(tmp) == "matrix")
                      {
                        tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                      } else {
                        tmp <- c(tmp, "")
                      }
                    } else {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = soustable,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i]
                        )
                    }
                    descript <- rbind(descript, tmp)
                  }
                } else {
                  if (!is.element(vecrefs[i], valeurs[deb:(deb + length(levvar) - 1)]) &
                      !is.na(vecrefs[i]))
                  {
                    print(
                      paste(
                        "Vous essayez de r?aliser un tableau, avec une r?f?rence * ",
                        vecrefs[i],
                        " * que vous ne d?finissez pas dans le vecteur de valeurs possibles pour la variable"
                      )
                    )
                    print(vect.var[i])
                    return()
                  } else {
                    if (is.na(vecrefs[i]))
                    {
                      ref <- NULL
                    } else {
                      ref <- vecrefs[i]
                    }
                    if (lgtest == 1 & is.null(testi))
                    {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          digits = digits,
                          y = varint,
                          baz = soustable,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                      if (class(tmp) == "matrix")
                      {
                        tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                      } else {
                        tmp <- cbind(tmp, rep("", length(tmp)))
                      }
                    } else {
                      tmp <-
                        qualitbis(
                          x = vect.var[i],
                          y = varint,
                          digits = digits,
                          baz = soustable,
                          ref = ref,
                          test = testi,
                          nom = vecnoms[i],
                          valeurs = valeurs[deb:(deb + length(levvar) - 1)]
                        )
                    }
                    descript <- rbind(descript, tmp)
                  }
                }
              } else {
                if (lgtest == 1 & is.null(testi))
                {
                  tmp <-
                    qualitbis(
                      x = vect.var[i],
                      y = varint,
                      baz = soustable,
                      test = testi,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)],
                      digits = digits
                    )
                  if (class(tmp) == "matrix")
                  {
                    tmp <- cbind(tmp, rep("", dim(tmp)[1]))
                  } else {
                    tmp <- cbind(tmp, rep("", length(tmp)))
                  }
                } else {
                  tmp <-
                    qualitbis(
                      x = vect.var[i],
                      y = varint,
                      baz = soustable,
                      test = testi,
                      nom = vecnoms[i],
                      valeurs = valeurs[deb:(deb + length(levvar) - 1)],
                      digits = digits
                    )
                }
                descript <- rbind(descript, tmp)
              }
            } else {
              #digits=max(3, getOption("digits")-3)
              if (is.na(pres[i]))
              {
                presi = "med"
              } else {
                presi = pres[i]
              }

              if (lgtest == 1 & is.null(testi))
              {
                tmp <-
                  quantitbis(
                    x = vect.var[i],
                    y = varint,
                    baz = soustable,
                    test = testi,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
                tmp <- c(tmp, "")

              } else {
                tmp <-
                  quantitbis(
                    x = vect.var[i],
                    y = varint,
                    baz = soustable,
                    test = testi,
                    nom = vecnoms[i],
                    pres = presi,
                    val = valeurs[deb]
                  )
              }
              descript <- rbind(descript, tmp)
            }
          }
        }
      } # fin du for
      templev <- NULL
      for (i in 1:nlev)
      {
        if (!is.null(valvarint))
        {
          templev <- c(templev, paste(effect[i]), paste(valvarint[i]))
        } else {
          templev <- c(templev, paste(effect[i]), paste(lev[i]))
        }
      }

      #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
      descript <- rbind(c("", "", templev, rep("", lgtest)), descript)
      descript <-
        suppressWarnings(data.frame(descript, stringsAsFactors = FALSE))
      if (langue == "fr")
      {
        if (is.null(test))
        {
          if (is.null(nomvarint))
          {
            nomstable <-
              c("Parametres", "Valeurs", rep(c("N", "Statistiques*"), nlev))
            names(descript) <- nomstable
          } else {
            ligne1 <-
              c("Parametres",
                "Valeurs",
                rep("", nlev),
                nomvarint,
                rep("", nlev * 2 - (nlev + 1)))
            ligne2 <- c("", "", rep(c("N", "Statistiques*"), nlev))
            descript <- rbind(ligne2, descript)
            names(descript) <- ligne1
          }
        } else {
          if (is.null(nomvarint))
          {
            nomstable <-
              c("Parametres", "Valeurs", rep(c("N", "Statistiques*"), nlev), "p-value")
            names(descript) <- nomstable
          } else {
            ligne1 <-
              c("Parametres",
                "Valeurs",
                rep("", nlev),
                nomvarint,
                rep("", nlev * 2 - nlev))
            ligne2 <-
              c("", "", rep(c("N", "Statistiques*"), nlev), "p-value")
            descript <- rbind(ligne2, descript)
            names(descript) <- ligne1
          }
        }
      }
      if (langue == "en")
      {
        if (is.null(test))
        {
          if (is.null(nomvarint))
          {
            nomstable <- c("Parameters", "Values", rep(c("N", "Statistics*"), nlev))
            names(descript) <- nomstable
          } else {
            ligne1 <-
              c("Parameters",
                "Values",
                rep("", nlev),
                nomvarint,
                rep("", nlev * 2 - (nlev + 1)))
            ligne2 <- c("", "", rep(c("N", "Statistics*"), nlev))
            descript <- rbind(ligne2, descript)
            names(descript) <- ligne1
          }
        } else {
          if (is.null(nomvarint))
          {
            nomstable <-
              c("Parameters", "Values", rep(c("N", "Statistics*"), nlev), "p-value")
            names(descript) <- nomstable
          } else {
            ligne1 <-
              c("Parameters",
                "Values",
                rep("", nlev),
                nomvarint,
                rep("", nlev * 2 - (nlev + 1)))
            ligne2 <-
              c("", "", rep(c("N", "Statistics*"), nlev), "p-value")
            descript <- rbind(ligne2, descript)
            names(descript) <- ligne1
          }
        }
      }
      return(descript)
    } #fin avec variable d'int?r?t
  }
