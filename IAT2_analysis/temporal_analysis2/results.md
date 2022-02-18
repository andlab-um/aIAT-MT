t.test(MAD~condition1,data=agg_mad,paired=TRUE)

	Paired t-test

data:  MAD by condition1
t = -3.8348, df = 25, p-value = 0.0007564
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.18078652 -0.05444842
sample estimates:
mean of the differences 
             -0.1176175 

============================================

MAD

   condition1      mean        sd
1   congruent 0.4620127 0.2249775
2 incongruent 0.5796302 0.2023761

=============================================



Model: MAD ~ (1 + condition1.x | subjID) + condition1.x
Data: results
        Effect                              df                   F                    p.value

1 condition1.x 1,              24.99                7.90 **             .009

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

==============================================

Model: RT.x ~ (1 | subjID) + condition1.x
Data: results
        Effect                           df                      F                             p.value

1 condition1.x 1,             8407.54            599.66 ***                <.001

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

==============================================

x_position t_test

Significant time points:  15 16 17 51 52 53 54 55 56 57 58 59 60 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90

================================================

================================================

t.test(MAD~condition2, data=agg_mad2, paired=TRUE)

	Paired t-test

data:  MAD by condition2
t = -2.1991, df = 25, p-value = 0.03734
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.232883209 -0.007629576
sample estimates:
mean of the differences 
             -0.1202564 

==================================================

 MAD

 condition2      mean        sd
1    con_ev1 0.4594705 0.2749697
2  incon_ev1 0.5797269 0.2587659

==================================================

t.test(AD~condition2, data=agg_ad2, paired=TRUE)

	Paired t-test

data:  AD by condition2
t = -2.0792, df = 25, p-value = 0.04801
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.0708659070 -0.0003366719
sample estimates:
mean of the differences 
            -0.03560129 

==================================================

 AD

 condition2      mean         sd
1    con_ev1 0.1316901 0.08411582
2  incon_ev1 0.1672914 0.08413999

====================================================

 t.test(AUC~condition2, data=agg_auc2, paired=TRUE)

	Paired t-test

data:  AUC by condition2
t = -2.0309, df = 25, p-value = 0.05303
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.145963946  0.001021668
sample estimates:
mean of the differences 
            -0.07247114 

=====================================================

AUC

  condition2      mean        sd
1    con_ev1 0.2801167 0.1625919
2  incon_ev1 0.3525879 0.1679411

====================================================

t.test(MAD~condition2, data=agg_mad2)

	Welch Two Sample t-test

data:  MAD by condition2
t = -2.6461, df = 39.051, p-value = 0.01168
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.40722853 -0.05438083
sample estimates:
  mean in group con_ev2 mean in group incon_ev2 
              0.4022813               0.6330859 

=====================================================

MAD

  condition2      mean        sd
1    con_ev2 0.4022813 0.2567966
2  incon_ev2 0.6330859 0.3340435

====================================================

t.test(AD~condition2, data=agg_ad2)

	Welch Two Sample t-test

data:  AD by condition2
t = -2.4141, df = 32.503, p-value = 0.02157
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.14168706 -0.01204948
sample estimates:
  mean in group con_ev2 mean in group incon_ev2 
              0.1128764               0.1897447 

====================================================

AD

  condition2      mean       sd
1    con_ev2 0.1128764 0.076584
2  incon_ev2 0.1897447 0.131689

====================================================

	Welch Two Sample t-test

data:  AUC by condition2
t = -1.1496, df = 45.337, p-value = 0.2563
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.16582307  0.04529393
sample estimates:
  mean in group con_ev2 mean in group incon_ev2 
              0.2655200               0.3257846 

=====================================================

AUC

  condition2      mean        sd
1    con_ev2 0.2655200 0.1858117
2  incon_ev2 0.3257846 0.1767470

======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: MAD ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                 df                       F                    p.value

1 condition2 3,       21.21             3.38*                     .037



Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

=======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: AD ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                 df                          F                   p.value

1 condition2 3,        21.11                3.59*             .031

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

=======================================================

Mixed Model Anova Table (Type 3 tests, KR-method)

Model: AUC ~ (1 + condition2 | subjID) + condition2
Data: results50
      Effect                    df                        F                  p.value

1 condition2 3,         21.18                 1.51                .241

Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

=======================================================

Ev1 : 16 17 18 19

Ev2: 31 32 33 34 35 36 37 38 39 40 57 58 59 60 61 62 63 64 65 66 67 68 69 74 75 76 77 78 79