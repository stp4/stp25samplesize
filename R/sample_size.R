

#' Sample Size
#'
#'
#' @param x Test name or pwr / WebPower object
#'
#' @param ...
#'
#' @export
#' @importFrom pwr pwr.f2.test
#' @importFrom pwr pwr.r.test
#' @importFrom pwr pwr.anova.test
#' @importFrom WebPower wp.kanova
#' @importFrom WebPower wp.rmanova
sample_size <-   function(x,
                          ...) {
  UseMethod("sample_size")
}



#' @param treatment,ndf, Number of treatment-groups
#' @param control Number of control-groups
#' @param  ng  treatment  * control
#' @param measurements,nm number of measurements
#' @param nscor Nonsphericity correction coefficien Sphericity was non-significant = 1
#' @param type Type of analysis (0 or 1 or 2). The value "0" is for between-effect; "1" is for within-effect; and "2" is for interaction effect.
#' @param predictors,df,u,v,k,n predictors for lm
#'
#' u	=  degrees of freedom for numerator (pwr.f2.test)
#' v	= degrees of freedom for denominator (pwr.f2.test)
#' k	= Number of groups (pwr.anova.test)
#' n	= Number of observations (per group pwr.anova.test)
#'
#' @param f,f2,r,d  	Effect size. r="Correlation coefficient"
#' d	= "Standardized difference value (Cohen's d)"
#' f= "Cohen's f"
#' f2=	"Cohen's f-squared"
#' @param power Statistical power.
#' @param sig.level significance level chosed for the test. It equals 0.05 by default.
#' @param alternative hypothese
#'
#' @return data.frame
#' @rdname sample_size
#' @export
#'
#' @examples
#'
#' sample_size("cor", drop.out.rate = .10)
#' sample_size("aov")
#' sample_size("lm")
#' sample_size("aov", treatment = 2 , f = .5)
#' sample_size("anova", treatment = 2, f = .5)
#' sample_size("anova", treatment = 2, control = 2 * 2, f = .5)
#' sample_size("anova", treatment = 1, measurements = 4, f = 0.608)

sample_size.character <-
  function(x = c("lm",
                 "glm",
                 "cor",
                 "lmer",
                 "aov",
                 "t.test",
                 "chisq.test",
                 "wilcox.test"),
           # ANOVA Drug treatments
           treatment = NULL,
           control = 1,
           ndf =  treatment - 1,
           ng = (ndf + 1) * control,
           measurements = 1,
           nm = measurements,


           drop.out.rate= NULL,
           nscor=1, #
         type="two.sample",
           #k= ndf+1,

           # lm
           predictors=NULL,
           df = predictors,
           u = df,
           v = NULL,



           d = c(small = .20, medium =.50, large =.80),   # pwr.t.test
           r = c(small = .10, medium =.30, large =.50),
           f = c(small =.10 , medium =.25, large =.40),  # pwr.anova.test
           f2 = c(small = .02, medium =.15, large =.35), # pwr.f2.test
           h = c(small = .2, medium =.5, large =.8),
           w = c(small =.10 , medium =.30, large =.50),


           #pwr.f2.test
           power = 0.80,
           sig.level = 0.05,

           # cor
           alternative = "two.sided") {
    rslt <- NULL
    if (x[1] %in% c("f2", "lm")) {
      for (j in f2) {
        x <- sample_size(pwr.f2.test(
          u = if (!is.null(u))
            u
          else
            1,
          v = v,
          f2 = j,
          sig.level = sig.level,
          power = power
        ))

        rslt <- rbind(rslt, x)
      }
    }
    else if (x[1] %in% c("cor", "r")) {
        for (j in r) {
        x <- sample_size (pwr.r.test(
          r = j,
          sig.level = sig.level,
          power = power
        ))

        rslt <- rbind(rslt, x)
      }

    }
    else if (x[1] == "aov") {
      for (j in f) {

        x <- sample_size (
          pwr.anova.test(
            k = if (length(ndf) == 0)
              2
            else
              ndf + 1,
            n = NULL,
            f = j,
            sig.level = sig.level,
            power = power
          )
        )

        rslt <- rbind(rslt, x)
      }

    }
    else if (x[1] == "anova" & measurements==1) {
      for (j in f) {
        x <- sample_size (wp.kanova(
          ndf = ndf,
          ng = ng,
          n = NULL,
          f = j,
          alpha = sig.level,
          power = power
        ))

        rslt <- rbind(rslt, x)
      }

    }
    else if (x[1] == "anova" & measurements>1) {
      for (j in f) {
        x <- sample_size(
          wp.rmanova(

          ng = ng,
          nm = nm,
          n = NULL,
          f = j, nscor=nscor,
          alpha = sig.level,
          power = power,
          type=type
        ))

        rslt <- rbind(rslt, x)
      }

    }
    else if ( x[1] ==  "chisq.test"){

      for (j in w) {
        x <- sample_size (pwr.chisq.test(
          w = j,
          df =df,
          sig.level = sig.level,
          power = power
        ))

        rslt <- rbind(rslt, x)
      }

    }
    else if ( x[1] ==  "t.test"){

      for (j in d) {
        x <- sample_size (pwr.t.test(
          d = j,
          sig.level = sig.level,
          power = power,
          type=type
        ))




        rslt <- rbind(rslt, x)
      }

    }


if(!is.null(drop.out.rate)){
  rslt$drop.out.rate=drop.out.rate
  rslt$n.drop.out=drop_out_rate( rslt$n, drop.out.rate)

}
    rslt
  }

#' @rdname sample_size
#' @export
sample_size.power.htest <- function(x) {

  if (names(x)[3] == "f2") {
    data.frame(
      test = "lm",
      df  = x$u,
      df.residual = ceiling(x$v),
      effect.size.f2 = round(x$f2, 2),
      sig.lev = round(x$sig.level, 3),
      power = round(x$power, 2),
      n = ceiling(x$v) + x$u + 1
    )
  }
  else if (names(x)[2] == "r") {
    data.frame(
      test = "cor",
      effect.size.r = round(x$r, 2),
      sig.lev = round(x$sig.level, 3),
      power = round(x$power, 2),
      n = ceiling(x$n)
    )
  }
  else if (names(x)[3] == "f") {
    data.frame(
      test = "aov",
      treatment = x$k,
      df.n = x$k - 1,

      effect.size.f = round(x$f, 2),
      sig.lev = round(x$sig.level, 3),
      power = round(x$power, 2),
      n = ceiling(x$n * x$k)

    )
  }
  else if (names(x)[1] == "w") {
    data.frame(
      test = "chisq.test",
      treatment = NA,
      df.n = x$df,

      effect.size.w = round(x$w, 2),
      sig.lev = round(x$sig.level, 3),
      power = round(x$power, 2),
      n = ceiling(x$N)

    )
  }
  else if (names(x)[2] == "d") {
    data.frame(
      test = "t.test",
      treatment = 1,
      df.n = 1,

      effect.size.d = round(x$d, 2),
      sig.lev = round(x$sig.level, 3),
      power = round(x$power, 2),
      n = ceiling(x$n)

    )
  }

}

#' @rdname sample_size
#' @export
sample_size.webpower <- function(x) {

if(x$method =="Multiple way ANOVA analysis")
  data.frame(
    test = "Anova",
    treatment = x$ndf + 1,
    df.n = x$ndf,
    df.d = round(x$ddf),
    ng = x$ng,

    effect.size.f = round(x$f, 2),
    sig.lev = round(x$alpha , 3),
    power = round(x$power, 2),
    n = ceiling(x$n)

  )
else if(x$method == "Repeated-measures ANOVA analysis")
  data.frame(
    test = "Anova (rep-measure)",
    treatment = x$ng,
    measurements=x$nm,

   nscor =x$nscor,
    effect.size.f = round(x$f, 2),
    sig.lev = round(x$alpha , 3),
    power = round(x$power, 2),
    n = ceiling(x$n)

  )

  else x$method
}



# sample_size(pwr.f2.test(
#   u = 6,
#   f2 = .4,
#   sig.level = 0.05,
#   power = 0.80
# ))
#
#
# sample_size("lm", df = 2)
#
#
#
# sample_size( pwr.r.test(  r = .3, sig.level = 0.05, power = .80 ))
# sample_size("cor")
#
#
# sample_size(pwr.anova.test(f=0.28,k=2,power=0.80,sig.level=0.05))
#sample_size(wp.kanova(ndf=1, f=.7, ng=1, alpha=0.05, power=0.80))
#sample_size(wp.kanova(ndf=1, f=.5, ng=2*4, alpha=0.05, power=0.80)
# require(stp25samplesize)
# require(pwr)
#
# pwr.chisq.test(w=0.10 , sig.level=.05, power=.80,  df=(4-1)*(3-1))
# sample_size( "chisq.test",w=0.10 , sig.level=.05, power=.80,  df=(4-1)*(3-1))







