# numbermatchsolver
A simple solver of the "Instagram ad"-like game, Number Match. Done for a Comp Sci project.

For a somewhat extensive description and apology of the various functions of numatch.ml, read README.**pdf**. 
It's in french at the moment, but I will try to translate it soon.

## To Run:

`rm m n` returns a random list of lists (matrix) of m by n numbers between 1-9.
`fullgame mat` plays number match with `mat` as initial setup. Prints and counts every move. (Consecutive deletions count as one move) 
`avg m n k` plays number match `k` times with different randomly generated (mxn) matrices. Returns average move count. 
