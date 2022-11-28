locs <- readRDS("data/ex33.rds")

 fit <- fit_ssm(locs %>% filter(id == "hp6-749-19"),
                model = "crw",
                time.step = 12,
                vmax = 4,
                control = ssm_control(verbose = 0))

 set.seed(10000)
 load(system.file("extdata/grad.rda", package = "foieGras"))
 trs <- foieGras::sim_fit(fit,
               what = "predicted",
               reps = 100,
               grad = grad,
               beta = c(-350, -350))

 trs_flt <- foieGras::sim_filter(trs,
                       keep = 0.25,
                       flag = 2)

 trs_flt_rr <- foieGras::route_path(trs_flt,
                                 map_scale = 50,
                                 buffer = 20000)
