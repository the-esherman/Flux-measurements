#! /bin/sh

# shell script to correct delimter on the EGM5 CPY-5 when formated with two delimiters: "," and " "
# From
# M5,31/08/22,11:17:41,     1,   799,   431,  992.5, 259, 0.0, 0.0, 0.0,  0, 0.0000,   991, 0.0,17.0, 0.0, 50,     0,     1, 0.0000, 0.0000
# To
# M5,31/08/22,11:17:41,1,799,431,992.5,259,0.0,0.0,0.0,0,0.0000,991,0.0,17.0,0.0,50,0,1,0.0000,0.0000
# To
# M5	31/08/22	11:17:41	1	799	431	992.5	259	0.0	0.0	0.0	0	0.0000	991	0.0	17.0	0.0	50	0	1	0.0000	0.0000
# And adding lines in the header to match number of columns: The last five Parameters given by the EGM5 for mode 3
mkdir delim
for file in ./*.TXT
do
    sed -E -e 's/, +/,/g' -e 's/Msoil/Msoil,Process,DC,DT,Flux,QFlux/' -e 's/,/	/g' $file > delim/$file
done