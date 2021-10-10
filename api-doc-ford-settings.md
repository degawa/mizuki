---
output_dir: ./api-doc
src_dir: ./src
         ./tests
media_dir: ./doc/media
exclude_dir: ./src/test
project: MIZUKI
project_github: https://github.com/to-degawa/mizuki
summary: MIZUKI - An educational purpose single-phase and gas-liquid two-phase flow simulation code using the finite difference method
author: Tomohiro Degawa
license: by-nc
docmark: !
docmark_alt: *
predocmark: >
predocmark_alt: |
display: public
         protected
         private
sort: permission-alpha
search: true
source: false
extra_mods: iso_fortran_env: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
            stdlib_logger: https://stdlib.fortran-lang.org/module/stdlib_logger.html
graph: false
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

<!-- document's landing page content --->
{!api-doc-index.md!}