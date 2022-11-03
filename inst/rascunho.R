devtools::document()
devtools::load_all()
devtools::test()


devtools::check()

usethis::use_pipe()

## data para HED ##
seqs2 <- seqinr::read.fasta(file.choose())

seqs <- histoc::seqs

dst <- histoc::dst

cHED(hla1 = "A*01:01", hla2 = "A*01:02")

cHED(hla1 ='DRB10405', hla2 ='DRB10701')

cHED('C*07:01','C*16:01')

startsWith('DRB10405','D') &
startsWith('DRB10701','D')


library(tidyverse)

names(seqs2)[str_detect(names(seqs2), '^D')]

usethis::use_data(seqs2, overwrite = TRUE)

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

# release to CRAN

devtools::spell_check()

devtools::check_rhub()

devtools::build()

devtools::release()


testthat::test_local()
