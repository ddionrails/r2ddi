#
# ddi2latex(ddi, file)
#
# Exports ddi object as a latex-codebook.
#
# Params:
# * ddi: ddi-object
# * file: file-name
#
ddi2latex = function(ddi, in_file, out_file){

  ##### LIBRARIES #####

  library('tools')
  library('brew')


  ##### START #####

  brew(in_file, out_file)
}
