#' @importFrom stats aov qnorm sd

or.calc <- function(obj, alpha = 0.05)
  ##
  ## calculer un OR ? partir d'un objet (r?sultat d'un certain mod?le)
  ##
{
  se <- sqrt(diag(obj$var))
  res <-
    data.frame(
      OR = exp(obj$coef),
      lower = exp(obj$coef - qnorm(1 - alpha / 2) * se),
      upper = exp(obj$coef + qnorm(1 - alpha / 2) * se),
      p.value = (1 - pchisq((obj$coef / se) ^ 2, 1))
    )
  return(res)
}

result.cox <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[, 1], 2)
  res$IC <-
    paste("[" ,
          round(modele.detail[, 3], 2) ,
          " - " ,
          round(modele.detail[, 4], 2) ,
          "]",
          sep = "")
  for (i in seq_along(res$IC))
  {
    res$pval[i] <- as.character(format_pv(summary(modele)$coef[i, 5]))
  }
  return(res)
}

qualitbis <-
  function(x,
           y = NULL,
           test = NULL,
           ref = NULL,
           digits = 2,
           baz = NULL,
           nom = NULL,
           valeurs = NULL)
    ## utilis?e dans completFRbis
    ##
    ## descriptif variables qualitatives
    ## x : variable d'int?r?t ou "nom" de la variable
    ## y : groupe ? comparer ou "nom" de la variable
    ## test : "fisher", "chisq" ou "mcnemar"
    ## ref : valeur de r?f?rence de la variable x
    ## digits : pr?cision du pourcentage de sortie
    ## baz : data
    ## nom : nom devant apparaitre dans le tableau si diff?rent de x
    ## valeurs : valeurs ? faire appara?tre dans le tableau
  {
    if (!is.null(baz))
    {
      if (is.null(nom))
      {
        nom <- x
      }
      x <- baz[, x]
    }
    if (is.null(baz) & is.null(nom))
    {
      nom <- "x"
    }
    x <- as.factor(x)
    if (!is.null(valeurs))
    {
      levels(x) <- valeurs
    }
    if (is.null(y))
    {
      nb.na <- sum(is.na(x))
      if (nb.na == dim(baz)[1])
      {
        tmp <- cbind(nom, "NA", nb.na, "")
        return(tmp)
      }
      if (is.null(ref))
      {
        tab <- table(x)
        pourc <- round(tab / sum(tab) * 100, digits = digits)
        if (length(tab) >= 2)
        {
          tmp <-
            cbind(c(nom, rep("", length(tab) - 1)), names(tab), tab, paste(pourc, "%"))
        } else {
          tmp <- cbind(nom, names(tab), tab, paste(pourc, "%"))
        }
        if (nb.na != 0) {
          tmp <- rbind(tmp, c("", "NA", nb.na, ""))
        }
        #tab<-table(x,exclude=NULL)
        #pourc<-round(prop.table(tab)*100,digits=digits)
        #tmp<-cbind(c(nom,rep("",(length(tab)-1))),names(tab),paste(tab),paste(pourc,"%"))
        #if ( nb.na==0) {tmp<-tmp[-dim(tmp)[1],]}
        return(tmp)
      }
      if (!is.null(ref))
      {
        tab <- table(x == ref)
        pourc <- round(tab / sum(tab) * 100, digits = digits)
        tmp <-
          cbind(c(nom, rep("", length(ref) - 1)), ref, tab, paste(pourc, "%"))
        if (dim(tmp)[1] == 2)
        {
          tmp <- tmp[2, ]
        }
        if (nb.na != 0)
        {
          tmpna <- c("", "NA", nb.na, "")
          tmp <- rbind(tmp, paste(tmpna))
          #tab<-table(x==ref,exclude=NULL)
          #pourc<-round(prop.table(tab)*100,digits=digits)
          #if (nb.na != 0) {nbna <- c(2:3);ref <- c(ref,NA)} else {nbna <- 2}
          #tmp<-cbind(c(nom,rep("",length(ref)-1)),ref,tab[nbna],paste(pourc[nbna],"%"))
        }
        return(tmp)
      }
    }

    if (!is.null(y))
    {
      if (!is.null(baz))
      {
        y <- baz[, y]
      }
      y <- as.factor(y)
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      tab <- table(x, y)
      if (sum(nb.na) == dim(baz)[1])
      {
        tmpna <- NULL
        for (i in seq_along(levels(y)))
        {
          tmpna <- c(tmpna, nb.na[i], "")
        }
        tmpna <- c(nom, "NA", tmpna)
        return(tmpna)
      }

      if (!is.null(test))
      {
        if (test %in% c("wilcox", "t", "kruskal", "aov"))
        {
          restest <-
            suppressWarnings(eval(call(
              paste(test, ".test", sep = ""), as.numeric(x) ~ y
            )))
        }
        else {
          restest <-
            suppressWarnings(eval(call(paste(
              test, ".test", sep = ""
            ), tab)))
        }
        p.value <- format_pv(restest$p.value)
        #p.value <- ifelse (p.value>0.055,signif(p.value,1),signif(p.value,2))
        #p.value <- ifelse (p.value<0.0001,"<0.0001",p.value)
        #p.value <- ifelse(nchar(as.character(p.value))==3,paste(p.value,"0",sep=""),p.value)
      }
      if (is.null(ref))
      {
        som <-
          matrix(
            colSums(tab),
            nrow = dim(tab)[1],
            ncol = dim(tab)[2],
            byrow = TRUE
          )
        #effect <- rbind((som[1,]+nb.na),(som[1,]+nb.na))
        #pourc<-round(tab/effect*100,digits=digits)
        pourc <- round(tab / som * 100, digits = digits)
        tmp <- NULL
        for (i in 1:dim(tab)[2])
        {
          tmp <- cbind(tmp, paste(tab[, i]), paste(pourc[, i], "%"))
        }
        tmp[tmp == "NaN %"] <- ""
        tmp <- cbind(paste(nom), paste(rownames(tab)), tmp)
        if (dim(tab)[1] >= 2)
        {
          tmp[2:dim(tab)[1], 1] <- ""
        }
        if (sum(nb.na) != 0)
        {
          tmpna <- NULL
          for (i in 1:dim(tab)[2])
          {
            #tmpna <- c(tmpna,nb.na[i],paste(round(nb.na[i]/(som[i]+nb.na[i])*100,digits=digits),"%"))
            tmpna <- c(tmpna, nb.na[i], "")
          }
          tmpna <- c("", "NA", tmpna)
          tmp <- rbind(tmp, paste(tmpna))
        }
        if (!is.null(test))
        {
          if (sum(nb.na) == 0)
          {
            tmp <- cbind(tmp, c(p.value, rep("", (
              dim(tab)[1] - 1
            ))))
          } else {
            tmp <- cbind(tmp, c(p.value, rep("", (
              dim(tab)[1] - 1
            )), ""))
          }
        }
        return(tmp)
      }
      if (!is.null(ref))
      {
        nbligne = 1
        tab <- table(x == ref, y)
        som <-
          matrix(
            colSums(tab),
            nrow = dim(tab)[1],
            ncol = dim(tab)[2],
            byrow = TRUE
          )
        pourc <- round(tab / som * 100, digits = digits)
        tmp <- NULL
        for (i in 1:dim(tab)[2])
        {
          tmp <- cbind(tmp, paste(tab[, i]), paste(pourc[, i], "%"))
        }
        tmp[tmp == "NaN %"] <- ""
        tmp <- cbind(paste(nom), paste(rownames(tab)), tmp)
        if (dim(tab)[1] >= 2)
        {
          tmp[2, 2] <- ref
          tmp <- tmp[2, ]
        } else {
          tmp[, 2] <- ref
        }
        if (sum(nb.na) != 0)
        {
          tmpna <- NULL
          for (i in 1:dim(tab)[2])
          {
            #tmpna <- c(tmpna,nb.na[i],paste(round(nb.na[i]/(som[i]+nb.na[i])*100,digits=digits),"%"))
            tmpna <- c(tmpna, nb.na[i], "")
          }
          tmpna <- c("", "NA", tmpna)
          tmp <- rbind(paste(tmp), paste(tmpna))
          nbligne = nbligne + 1
        }
        #tmp<-matrix(tmp,nrow=nbligne,ncol=2+2*nlev,byrow=F)
        if (!is.null(test))
        {
          if (sum(nb.na) == 0)
          {
            tmp <- c(tmp, p.value)
          } else {
            tmp <- cbind(tmp, c(p.value, ""))
          }
        }
        return(tmp)
      }
    }
  }

quantitbis <-
  function(x,
           y = NULL,
           test = NULL,
           digits = max(3, getOption("digits") - 3),
           baz = NULL,
           nom = NULL,
           pres = "med",
           val = NULL)
    ## utilis?e dans completFRbis
    ##
    ## descriptif variables qualitatives
    ## x : variable d'int?r?t ou "nom" de la variable
    ## y : groupe ? comparer ou "nom" de la variable
    ## test : "t", "wilcox","aov","kruskal"
    ## digits : pr?cision du pourcentage de sortie
    ## baz : data
    ## nom : nom devant apparaitre dans le tableau si diff?rent de x
    ##  pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
    ##  !!!! par d?faut sera repr?sent?e dans le tableau la m?diane [q1-q3]
  {
    if (!is.null(baz))
    {
      if (is.null(nom))
      {
        nom <- x
      }
      x <- baz[, x]
    }
    if (is.null(baz) & is.null(nom))
    {
      nom <- "x"
    }
    if (is.null(val))
    {
      val <- ""
    } else if (is.na(val)) {
      val <- ""
    }

    if (is.null(y))
    {
      nb.na <- sum(is.na(x))
      if (nb.na != length(x))
      {
        tmp <- summary(x, digits = digits)
        tmpna <- ifelse(!is.na(tmp[7]), tmp[7], 0)
        if (pres == "med")
        {
          tmp <- paste(tmp[3], " [", tmp[2], ";", tmp[5], "]", sep = "")
          tmp <- c(nom, val, paste(length(x) - tmpna), tmp)
          return(tmp)
        }
        if (pres == "med_mm")
        {
          tmp <-
            paste(tmp[3], " [", tmp[2], ";", tmp[5], "] (", tmp[1], ";", tmp[6], ")", sep =
                    "")
          tmp <- c(nom, val, length(x) - tmpna, tmp)
          return(tmp)
        }
        if (pres == "mean")
        {
          tmp <-
            paste(tmp[4], "(", signif(sd(x, na.rm = TRUE), digits = digits), ")", sep =
                    "")
          tmp <- c(nom, val, length(x) - tmpna, tmp)
          return(tmp)
        }
        if (pres == "mean_mm")
        {
          tmp <- paste(tmp[4], "(", tmp[1], ";", tmp[6], ")", sep = "")
          tmp <- c(nom, val, paste(length(x) - tmpna), tmp)
          return(tmp)
        }
      } else {
        return(c(nom, val, nb.na - length(x), NA))
      }
    }

    if (!is.null(y))
    {
      if (!is.null(baz))
      {
        y <- baz[, y]
      }
      y <- as.factor(y)

      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      eff <- tapply(x[!is.na(y)], y[!is.na(y)], length)
      if (!is.null(test))
      {
        aov.test <- function(formula)
        {
          p.value <- unlist(summary(aov(pbc$age ~ pbc$stage)))[9]
          p.value <- as.numeric(p.value)
          return(list(p.value = p.value))
        }

        if (test == "wilcox" & length(levels(as.factor(y))) > 2)
        {
          print(nom)
          print(
            "Vous avez demand? ? effectuer un test de wilcoxon alors que votre variable d'int?r?t comporte plus de 2 classes."
          )
          print("Un test de kruskal wallis a ?t? effectu? ? la place.")
          test = "kruskal"
        }
        restest <-
          suppressWarnings(eval(call(paste(
            test, ".test", sep = ""
          ), x ~ y)))
        p.value <- format_pv(restest$p.value)
        #p.value <- ifelse (p.value>0.055,signif(p.value,1),signif(p.value,2))
        #p.value <- ifelse (p.value<0.0001,"<0.0001",p.value)
        #p.value <- ifelse(nchar(as.character(p.value))==3,paste(p.value,"0",sep=""),p.value)
      }
      tempo <- tapply(x, y, summary, digits = digits)
      for (ii in names(tempo))
      {
        tempo[[ii]] <- tempo[[ii]][1:6]
      }
      tempo <- matrix(unlist(tempo), nrow = dim(tempo), byrow = TRUE)

      if (pres == "med")
      {
        truc <- paste(tempo[, 3], " [", tempo[, 2], ";", tempo[, 5], "]", sep =
                        "")
        tmp <- NULL
        for (i in seq_along(levels(y)))
        {
          tmp <- cbind(tmp, paste(eff[i] - nb.na[i]), paste(truc[i]))
        }
        for (i in seq_along(tmp))
        {
          if (tmp[i] == "NA [NA;NA]")
            tmp[i] <- ""
        }
        tmp <- c(nom, val, tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "med_mm")
      {
        truc <-
          paste(tempo[, 3],
                " [",
                tempo[, 2],
                ";",
                tempo[, 5],
                "] (",
                tempo[, 1],
                ";",
                tempo[, 6],
                ")",
                sep = "")
        tmp <- NULL
        for (i in seq_along(levels(y)))
        {
          tmp <- cbind(tmp, paste(eff[i] - nb.na[i]), paste(truc[i]))
        }
        for (i in seq_along(tmp))
        {
          if (tmp[i] == "NA [NA;NA] (NA;NA)")
            tmp[i] <- ""
        }
        tmp <- c(nom, val, tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "mean")
      {
        tmp2 <- tapply(x, y, sd, na.rm = TRUE)
        tmp2 <- signif(tmp2, digits = digits)
        truc <- paste(tempo[, 4], "(", tmp2, ")", sep = "")
        tmp <- NULL
        for (i in seq_along(levels(y)))
        {
          tmp <- cbind(tmp, paste(eff[i] - nb.na[i]), paste(truc[i]))
        }
        for (i in seq_along(tmp))
        {
          if (tmp[i] == "NaN (NA)")
            tmp[i] <- ""
        }
        tmp <- c(nom, val, tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "mean_mm")
      {
        truc <- paste(tempo[, 4], "(", tempo[, 1], ";", tempo[, 6], ")", sep = "")
        tmp <- NULL
        for (i in seq_along(levels(y)))
        {
          tmp <- cbind(tmp, paste(eff[i] - nb.na[i]), paste(truc[i]))
        }
        for (i in seq_along(tmp))
        {
          if (tmp[i] == "NaN (NA;NA)")
            tmp[i] <- ""
        }
        tmp <- c(nom, val, tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
    }
  }

qualit <-
  function(x,
           y = NULL,
           test = NULL,
           ref = NULL,
           digits = 1,
           baz = NULL,
           nom = NULL,
           valeurs = NULL)
  {
    # x : variable d'int?r?t ou "nom" de la variable
    # y : groupe ? comparer ou "nom" de la variable
    # test : "fisher", "chisq" ou "mcnemar"
    # ref : valeur de r?f?rence de la variable x
    # digits : pr?cision du pourcentage de sortie
    # baz : data
    # nom : nom devant apparaitre dans le tableau si diff?rent de x
    if (!is.null(baz))
    {
      if (is.null(nom))
      {
        nom <- x
      }
      x <- baz[, x]
    }

    if (is.null(baz) & is.null(nom))
    {
      nom <- "x"
    }

    x <- as.factor(x)
    if (!is.null(valeurs))
    {
      levels(x) <- valeurs
    }

    if (is.null(y))
    {
      nb.na <- sum(is.na(x))
      if (nb.na != 0)
      {
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      if (is.null(ref))
      {
        tab <- table(x)
        tmp <-
          paste(tab, " (", round(tab / sum(tab) * 100, digits = digits), ")", sep =
                  "")
        tmp <- cbind(c(nom, rep("", (
          length(tab) - 1
        ))), names(tab), tmp)
        return(tmp)
      }
      if (!is.null(ref))
      {
        tab <- table(x == ref)
        tmp <-
          paste(tab, " (", round(tab / sum(tab) * 100, digits = digits), ")", sep =
                  "")
        return(c(nom, ref, tmp[2]))
      }
    }

    if (!is.null(y))
    {
      if (!is.null(baz))
      {
        y <- baz[, y]
      }
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      if (sum(nb.na) != 0)
      {
        nb.na <- paste(nb.na, sep = "", collapse = "/")
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      tab <- table(x, y)
      if (!is.null(test))
      {
        p.value <-
          suppressWarnings(eval(call(paste(
            test, ".test", sep = ""
          ), tab)))
        p.value <- signif(p.value$p.value, 2)
        p.value[p.value < 0.0001] <- "<0.0001"
      }
      if (is.null(ref))
      {
        som <-
          matrix(
            colSums(tab),
            nrow = dim(tab)[1],
            ncol = dim(tab)[2],
            byrow = TRUE
          )
        tmp <-
          paste(tab, " (", round(tab / som * 100, digits = digits), ")", sep = "")
        tmp <- matrix(tmp, nrow = dim(tab)[1])
        tmp <- cbind(c(nom, rep("", (dim(
          tab
        )[1] - 1))), rownames(tab), tmp)
        if (!is.null(test))
        {
          tmp <- cbind(tmp, c(rep("", (
            dim(tab)[1] - 1
          )), p.value))
        }
        return(tmp)
      }
      if (!is.null(ref))
      {
        tab <- table(x == ref, y)
        tmp <-
          paste(tab, " (", round(tab / sum(tab) * 100, digits = digits), ")", sep =
                  "")
        som <-
          matrix(
            colSums(tab),
            nrow = dim(tab)[1],
            ncol = dim(tab)[2],
            byrow = TRUE
          )
        tmp <-
          paste(tab, " (", round(tab / som * 100, digits = digits), ")", sep = "")
        tmp <- matrix(tmp, nrow = dim(tab)[1])
        tmp <- c(nom, ref, tmp[2, ])
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
    }
  }


quantit <-
  function(x,
           y = NULL,
           test = NULL,
           digits = max(3, getOption("digits") - 3),
           baz = NULL,
           nom = NULL,
           pres = "med_mm")
  {
    # x : variable d'int?r?t ou "nom" de la variable
    # y : groupe ? comparer ou "nom" de la variable
    # test : "t", "wilcox","aov","kruskal"
    # digits : pr?cision du pourcentage de sortie
    # baz : data
    # nom : nom devant apparaitre dans le tableau si diff?rent de x
    # pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
    if (!is.null(baz))
    {
      if (is.null(nom))
      {
        nom <- x
      }
      x <- baz[, x]
    }

    if (is.null(baz) & is.null(nom))
    {
      nom <- "x"
    }

    if (is.null(y))
    {
      nb.na <- sum(is.na(x))
      if (nb.na != 0)
      {
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      tmp <- summary(x, digits = digits)
      if (pres == "med")
      {
        tmp <- paste(tmp[3], " [", tmp[2], ";", tmp[5], "]", sep = "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
      if (pres == "med_mm")
      {
        tmp <-
          paste(tmp[3], " [", tmp[2], ";", tmp[5], "] (", tmp[1], ";", tmp[6], ")", sep =
                  "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
      if (pres == "mean")
      {
        tmp <-
          paste(tmp[4], " +/-", signif(sd(x, na.rm = TRUE), digits = digits), sep =
                  "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
    }

    if (!is.null(y))
    {
      if (!is.null(baz))
      {
        y <- baz[, y]
      }
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      if (sum(nb.na) != 0)
      {
        nb.na <- paste(nb.na, sep = "", collapse = "/")
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      if (!is.null(test))
      {
        aov.test <- function(formula)
        {
          p.value <- unlist(summary(aov(pbc$age ~ pbc$stage)))[9]
          p.value <- as.numeric(p.value)
          return(list(p.value = p.value))
        }
        p.value <-
          suppressWarnings(eval(call(paste(
            test, ".test", sep = ""
          ), x ~ y)))
        p.value <- signif(p.value$p.value, 2)
        p.value_tmp <- p.value
        p.value[p.value < 0.0001] <- "<0.0001"
        p.value[p.value_tmp < 0.05] <-
          paste(p.value[p.value_tmp < 0.05], "*")
      }
      tmp <- tapply(x, y, summary, digits = digits)
      for (ii in names(tmp))
      {
        tmp[[ii]] <- tmp[[ii]][1:6]
      }
      tmp <- matrix(unlist(tmp), nrow = dim(tmp), byrow = TRUE)
      #print(tmp)
      if (pres == "med")
      {
        tmp <- paste(tmp[, 3], " [", tmp[, 2], ";", tmp[, 5], "]", sep = "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "med_mm")
      {
        tmp <-
          paste(tmp[, 3], " [", tmp[, 2], ";", tmp[, 5], "] (", tmp[, 1], ";", tmp[, 6], ")", sep =
                  "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "mean")
      {
        tmp2 <- tapply(x, y, sd, na.rm = TRUE)
        tmp2 <- signif(tmp2, digits = digits)
        tmp <- paste(tmp[, 4], " +/-", tmp2, sep = "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
    }
  }



quantit2 <-
  function(x,
           y = NULL,
           test = NULL,
           digits = max(3, getOption("digits") - 3),
           baz = NULL,
           nom = NULL,
           pres = "med_mm")
  {
    #### IDEM precedente avec une option en sus
    #### mean2 : mean (min; max)
    # x : variable d'int?r?t ou "nom" de la variable
    # y : groupe ? comparer ou "nom" de la variable
    # test : "t", "wilcox","aov","kruskal"
    # digits : pr?cision du pourcentage de sortie
    # baz : data
    # nom : nom devant apparaitre dans le tableau si diff?rent de x
    # pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
    if (!is.null(baz))
    {
      if (is.null(nom))
      {
        nom <- x
      }
      x <- baz[, x]
    }

    if (is.null(baz) & is.null(nom))
    {
      nom <- "x"
    }

    if (is.null(y))
    {
      nb.na <- sum(is.na(x))
      if (nb.na != 0)
      {
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      tmp <- summary(x, digits = digits)
      if (pres == "med")
      {
        tmp <- paste(tmp[3], " [", tmp[2], ";", tmp[5], "]", sep = "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
      if (pres == "med_mm")
      {
        tmp <-
          paste(tmp[3], " [", tmp[2], ";", tmp[5], "] (", tmp[1], ";", tmp[6], ")", sep =
                  "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
      if (pres == "mean")
      {
        tmp <-
          paste(tmp[4], " +/-", signif(sd(x, na.rm = TRUE), digits = digits), sep =
                  "")
        tmp <- c(nom, "", tmp)
        return(tmp)
      }
    }

    if (!is.null(y))
    {
      if (!is.null(baz))
      {
        y <- baz[, y]
      }
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      nb.na <- tapply(is.na(x[!is.na(y)]), y[!is.na(y)], sum)
      if (sum(nb.na) != 0)
      {
        nb.na <- paste(nb.na, sep = "", collapse = "/")
        nom <- paste(nom, " (NA=", nb.na, ")", sep = "")
      }
      if (!is.null(test))
      {
        aov.test <- function(formula)
        {
          p.value <- unlist(summary(aov(pbc$age ~ pbc$stage)))[9]
          p.value <- as.numeric(p.value)
          return(list(p.value = p.value))
        }
        p.value <-
          suppressWarnings(eval(call(paste(
            test, ".test", sep = ""
          ), x ~ y)))
        p.value <- signif(p.value$p.value, 2)
        p.value_tmp <- p.value
        p.value[p.value < 0.0001] <- "<0.0001"
        p.value[p.value_tmp < 0.05] <-
          paste(p.value[p.value_tmp < 0.05], "*")
      }
      tmp <- tapply(x, y, summary, digits = digits)
      for (ii in names(tmp))
      {
        tmp[[ii]] <- tmp[[ii]][1:6]
      }
      tmp <- matrix(unlist(tmp), nrow = dim(tmp), byrow = TRUE)
      #print(tmp)
      if (pres == "med")
      {
        tmp <- paste(tmp[, 3], " [", tmp[, 2], ";", tmp[, 5], "]", sep = "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "med_mm")
      {
        tmp <-
          paste(tmp[, 3], " [", tmp[, 2], ";", tmp[, 5], "] (", tmp[, 1], ";", tmp[, 6], ")", sep =
                  "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "mean")
      {
        tmp2 <- tapply(x, y, sd, na.rm = TRUE)
        tmp2 <- signif(tmp2, digits = digits)
        tmp <- paste(tmp[, 4], " +/-", tmp2, sep = "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
      if (pres == "mean2")
      {
        tmp2 <- tapply(x, y, sd, na.rm = TRUE)
        tmp2 <- signif(tmp2, digits = digits)
        tmp <- paste(tmp[, 4], " (", tmp[, 1], ";", tmp[, 6], ")", sep = "")
        tmp <- c(nom, "", tmp)
        if (!is.null(test))
        {
          tmp <- c(tmp, p.value)
        }
        return(tmp)
      }
    }
  }
