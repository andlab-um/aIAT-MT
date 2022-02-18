t.test(MAD~condition1,data=agg_mad,paired=TRUE)

	Paired t-test

data:  MAD by condition1
t = -3.8913, df = 58, p-value = 0.0002597
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.11077490 -0.03551965
sample estimates:
mean of the differences 
            -0.07314727 

============================================

MAD

   condition1      mean        sd
1   congruent 0.4449547 0.1971359
2 incongruent 0.5181020 0.1958930

=============================================



Model: MAD ~ (1 + condition1.x | subjID) + condition1.x
Data: results
        Effect                              df                   F                    p.value

1 condition1.x 1,                57.97             15.12 ***          <.001

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1



Warning messages:
1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  Model failed to converge with max|grad| = 0.00254926 (tol = 0.002, component 1)
2: lme4 reported (at least) the following warnings for 'full':

  * Model failed to converge with max|grad| = 0.00254926 (tol = 0.002, component 1) 

==============================================

Model: RT.x ~ (1 | subjID) + condition1.x
Data: results
        Effect                           df                      F                             p.value

1 condition1.x 1,              58.00              29.52 ***                    <.001

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

==============================================

x_position t_test

Significant time points:  50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84
85 86 87 88 89 90 91 92 93

================================================

================================================

t.test(MAD~condition2, data=agg_mad2, paired=TRUE)

	Paired t-test

data:  MAD by condition2
t = -2.8631, df = 56, p-value = 0.005893
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.15521994 -0.02742624
sample estimates:
mean of the differences 
            -0.09132309 

==================================================

 MAD

 condition2      mean        sd
1    con_ev1 0.4307852 0.2308717
2  incon_ev1 0.5221082 0.2428724

==================================================

t.test(AD~condition2, data=agg_ad2, paired=TRUE)

	Paired t-test

data:  AD by condition2
t = -2.8592, df = 56, p-value = 0.005956
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.049759056 -0.008759233
sample estimates:
mean of the differences 
            -0.02925914 

==================================================

 AD

 condition2      mean         sd
1    con_ev1 0.1201795 0.07350016
2  incon_ev1 0.1494386 0.07885458

====================================================

 t.test(AUC~condition2, data=agg_auc2, paired=TRUE)

	Paired t-test

data:  AUC by condition2
t = -1.9442, df = 56, p-value = 0.05691
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.081136258  0.001214452
sample estimates:
mean of the differences 
             -0.0399609 

=====================================================

AUC

  condition2      mean        sd
1    con_ev1 0.2725178 0.1395741
2  incon_ev1 0.3124787 0.1446679









====================================================

t.test(MAD~condition2, data=agg_mad2)

	Welch Two Sample t-test

data:  MAD by condition2
t = -1.5675, df = 50, p-value = 0.1233
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.13928629  0.01718017
sample estimates:
mean of the differences 
            -0.06105306 

=====================================================

MAD

  condition2      mean        sd
1    con_ev2 0.4057786 0.2575341
2  incon_ev2 0.4668317 0.3083740

====================================================

t.test(AD~condition2, data=agg_ad2, paired=TRUE)

	Paired t-test

data:  AD by condition2
t = -1.8016, df = 50, p-value = 0.07763
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.052835062  0.002869318
sample estimates:
mean of the differences 
            -0.02498287 

====================================================

AD

  condition2      mean       sd
1    con_ev2 0.1115486 0.07687094
2  incon_ev2 0.1365315 0.11023996

====================================================

t.test(AUC~condition2, data=agg_auc2, paired=TRUE)

	Paired t-test

data:  AUC by condition2
t = 0.11372, df = 50, p-value = 0.9099
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.03704261  0.04148877
sample estimates:
mean of the differences 
            0.002223077 

=====================================================

AUC

  condition2      mean        sd
1    con_ev2 0.2676609 0.1750906
2  incon_ev2 0.2654378 0.1756599

======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: MAD ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                 df                       F                    p.value

1 condition2.x 3,   53.62              5.67 **              .002



Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

Warning message:
lme4 reported (at least) the following warnings for 'full':

  * Model failed to converge with max|grad| = 0.00712775 (tol = 0.002, component 1) 

=======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: AD ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                 df                          F                   p.value

1 condition2.x 3,   53.36                6.09 **               .001

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

Warning message:
lme4 reported (at least) the following warnings for 'full':

  * Model failed to converge with max|grad| = 0.00712775 (tol = 0.002, component 1) 

=======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: AUC ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                    df                        F                  p.value

1 condition2.x 3,         53.64             2.84 *              .047

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

Warning message:
lme4 reported (at least) the following warnings for 'full':

  * Model failed to converge with max|grad| = 0.00712775 (tol = 0.002, component 1) 

=======================================================

Ev1 : 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 9192

Ev2:  17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86