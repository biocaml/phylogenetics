# Random notes

## Change from Lacaml to Owl

Was initially motivated by the lack of matrix exponentiation, but I
observed a severe (x2) performance regression on tests. After a few
benchmarking, it turns out that while owl is as fast or faster on
large matrices than lacaml, it is severely outperformed (x2,x3) on
small (4x4) matrices, and on vector initialisation. Not very important
at the moment, but it's probably best to keep using a wrapping
interface for linear algebra...
