test_models <- list(
        reg_pass = list(
                type = "sem",
                model = "Y1 ~ X1 + X2\nY2 ~ X3 + X4\nY1 ~~ 0*Y2"
        ),
        reg_corr_err_pass = list(
                type = "sem",
                model = "Y1 ~ X1\nY2 ~ Y1\nY3 ~ Y2\nY1 ~~ Y3"
        ),
        reg_feedback_fail = list(
                type = "sem",
                model = "Y1 ~ Y2\nY2 ~ Y1"
        ),
        cfa_two_fail = list(
                type = "cfa",
                model = "L1 =~ Y1 + Y2\nL2 =~ Y3 + Y4\nL1 ~~ 0*L2"
        ),
        cfa_two_pass_three_fail = list(
                type = "cfa",
                model = "L1 =~ Y1 + Y2 + Y3 + Y4\nL2 =~ Y5 + Y6\nL1 ~~ L2"
        ),
        cfa_three_pass = list(
                type = "cfa",
                model = "L1 =~ Y1 + Y2 + Y3\nL2 =~ Y4 + Y5 + Y6\nL1 ~~ L2"
        ),
        cfa_three_fail = list(
                type = "cfa",
                model = "L1 =~ Y1 + Y2 + Y3\nL2 =~ Y4 + Y5 + Y6\nL1 ~~ L2\n Y1 ~~ Y4"
        ),
        cfa_higher_order_pass = list(
                type = "cfa",
                model = "L1 =~ Y1 + Y2 + Y3\nL2 =~ Y4 + Y5 + Y6\nL3 =~ L1 + L2"
        ),
        sem_two_emitted_paths_fail = list(
                type = "sem",
                model = "L1 =~ NA*Y1\nL1 <~ X1\nL1 ~~ NA*L1\nY1 ~~ NA*Y1\nX1 ~~ NA*X1"
        ),
        sem_two_emitted_paths_pass = list(
                type = "sem",
                model = "L1 =~ NA*Y1 + NA*Y2 + NA*Y3\nL1 <~ X1\nL1 ~~ NA*L1\nY1 ~~ NA*Y1\nY2 ~~ NA*Y2\nY3 ~~ NA*Y3"
        ),
        sem_scaling_pass = list(
                type = "sem",
                model = "L1 =~ Y1 + Y2 + Y3\nL1 <~ X1\nL1 ~~ L1"
        ),
        sem_scaling_mean_pass = list(
                type = "sem",
                model = "f1 =~ y1 + y2 + y3\nf1 ~~ 1*f1\nf1 ~ 0*1"
        ),
        sem_scaling_fail = list(
                type = "sem",
                model = "L1 =~ NA*Y1 + NA*Y2 + NA*Y3\nL1 ~~ L1"
        ),
        sem_n_theta_fail = list(
                type = "sem",
                model = "L1 =~ NA*Y1 + NA*Y2\nL2 =~ NA*Y4 + NA*Y5\nL1 ~ L2\nL1 ~~ L1\nL2 ~~ L2"
        ),
        sem_two_scaling_ind = list(
                type = "sem",
                model = "L1 =~ 1*Y1 + 1*Y2 + Y3\nL1 <~ X1\nL1 ~~ L1"
        ),
        sem_zero_loading = list(
                type = "sem",
                model = "L1 =~ 0*Y1 + Y2 + Y3\nL1 <~ X1\nL1 ~~ L1"
        ),
        sem_negative_loading = list(
                type = "sem",
                model = "L1 =~ -1*Y1 + Y2 + Y3\nL1 <~ X1\nL1 ~~ L1"
        ),
        mlm = list(
                type = "sem",
                model = "level: 1\nY1 ~ X1 + X2\nlevel: 2\nY2 ~ Y1 + X3"
        ),
        categorical = list(
                type = "sem",
                model = "y1 | t1 + t2\ny2 | t3 + t4\ny1 ~*~ y1\ny2 ~*~ y2"
        )
)

