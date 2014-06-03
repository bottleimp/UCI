wget -c -r -np -k -L $1
rm archive.ics.uci.edu/ml/machine-learning-databases/*/index.html*
mv archive.ics.uci.edu/ml/machine-learning-databases/* .
rm -rf archive.ics.uci.edu
