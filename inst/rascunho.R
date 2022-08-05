devtools::document()
devtools::load_all()
devtools::test()


devtools::check()

usethis::use_pipe()

usethis::use_data(hlaDRpt, overwrite = TRUE)

usethis::use_gpl_license(version = 3, include_future = TRUE)

usethis::use_import_from("data.table", ":=")

usethis::use_package('knitr', type = 'Suggests')

usethis::use_mit_license(copyright_holder = 'TxOR')

usethis::use_news_md(open = rlang::is_interactive())

usethis::use_article("starts", title = "Get started")

usethis::use_vignette("how_to")

usethis::use_pkgdown()

usethis::use_readme_rmd()

usethis::use_github_action_check_standard() # exemplo para badge R-CMD-check


pkgdown::build_site()


usethis::use_pkgdown_github_pages()

usethis::git_sitrep()

usethis::create_github_token()

gitcreds::gitcreds_set()

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


devtools::load_all()
devtools::test_coverage()

library(covr)
report()

#  < install.packages('revdepcheck')

library(microbenchmark)
library(tictoc)
tic()
mbm <- microbenchmark("xmv0" = xmatch_r_v0(dA = c('1','2'),
                                           dB = c('5','7'),
                                           dDR = c('11','14')),
                      "xmv1" = xmatch_r(dA = c('1','2'),
                                        dB = c('5','7'),
                                        dDR = c('11','14')))

library(tidyverse)
autoplot(mbm)

toc()

library(hexSticker)
imgurl <- system.file("figures/chr_dna5.JPG", package="hexSticker")
s5 <- sticker(imgurl,
             package="histoc", p_size=20
             , s_x= 0.8
             , s_y= 1.2
             , p_x = 1
             , p_y = 0.5
             , h_fill = "#ffffff"
             , h_size = 2
             , p_color = "#0a75ad"
             , h_color = "#0a75ad"
             #, s_width=0.6, s_height=0.3
             ,filename="inst/figures/hex.jpg")

s5

###############################

# compute all possible donor-recipient pair for each one of the donors
all_pairs <- donor_recipient_pairs(df.donors = donors,
                      df.candidates = candidates,
                      df.abs = cabs,
                      algorithm = lima,
                      n = 0,
                      check.validity = FALSE)

# for loop to select available candidates for a pool of donors
used.candidates <- NULL
result <- NULL
for(i in 1: length(all_pairs)){
  tmp <- all_pairs[[i]][!ID %in% used.candidates][1:2,]

  result <- rbindlist(list(result, tmp))
  used.candidates <- c(used.candidates, tmp$ID)
}

# as a result we have all the selected donor-recipient pairs for a given pool of donors
result

library(tidyverse)
result %>%
  dplyr::filter(!is.na(ID)) %>%
  dplyr::as_tibble() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(txs = txscore(recipient.age = age,
                             recipient.dialysis = dialysis,
                             donor.age = donor_age,
                             mmHLA_A = mmA,
                             mmHLA_B = mmB,
                             mmHLA_DR = mmDR)$prob5y) %>%
  dplyr::ungroup()

result[!is.na(ID),][ ,
                     txs := txscore(recipient.age = age,
                                    recipient.dialysis = dialysis,
                                    donor.age = donor_age,
                                    mmHLA_A = mmA,
                                    mmHLA_B = mmB,
                                    mmHLA_DR = mmDR)$prob5y,
                     by = 'ID'][]

