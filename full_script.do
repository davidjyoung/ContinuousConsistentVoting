use "G:\My Drive\Papers\Correct Voting\R Project\cut_for_stata.dta"

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled disproportionality_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous_unprejudiced AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled disproportionality_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous_unprejudiced AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous_unweighted AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled disproportionality_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous_unweighted AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

use "G:\My Drive\Papers\Correct Voting\R Project\cut_for_stata_cses1-4.dta"

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled disproportionality_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)


use "G:\My Drive\Papers\Correct Voting\R Project\cut_for_stata_cses1-2.dta"

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled disproportionality_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)

mixed continuous AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, vce(robust)


use "G:\My Drive\Papers\Correct Voting\R Project\cut_for_stata.dta"

meglm binary AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, family(binomial) link(logit) vce(robust)


use "G:\My Drive\Papers\Correct Voting\R Project\cut_for_stata_cses1-2.dta"

meglm binary AGE polsoph EFFICACY logyrsdem_scaled media_scaled PPI_scaled clearlines_scaled enep_scaled pers_cut_scaled c.logyrsdem_scaled#c.AGE c.EFFICACY#c.media_scaled [pw = SAMPLE_WEIGHTS] || name: AGE polsoph EFFICACY, family(binomial) link(logit) vce(robust)
