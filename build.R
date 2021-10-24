library(fs)
library(purrr)
library(here)

rmd_files <- dir_ls(path = here(), recurse = TRUE, glob = '*/notes/*.Rmd')


# parent and grandparent dirs of the rmd files
ancestor_dirs <- map(rmd_files, ~rev(rev(path_split(.)[[1]])[c(3, 4)]))

# get the /doc dir where the rmd rendered htmls should be placed
output_dirs <- map(ancestor_dirs, ~path_join(c(here(), 'docs', .)))

# render it
map2(rmd_files, output_dirs,
     ~rmarkdown::render(input = .x, output_dir = .y)
)

