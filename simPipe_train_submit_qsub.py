# coding: utf-8

import os
import sys

geneVec = "600 100 7.5 7 1.8 1.8 "
dataDic = {"base": "FALSE mean none none ",
           "batch": "TRUE mean none none ",
           "sep_mod": "TRUE mean separate mod ",
           "sep_hyb": "TRUE mean separate null ",
           "cmb_mod": "TRUE mean combined mod ",
           "cmb_null": "TRUE mean combined null "}
designDic = {1: "200 100 200 100 ",
             2: "200 40 200 140 ",
             3: "200 100 200 100 ",
             4: "200 100 200 100 ",
             5: "200 40 200 140 "}
batchDic = {1: "2 0 50 50 50 50",
            2: "2 0 20 80 20 80",
            3: "2 0 30 70 70 30",
            4: "2 0 20 60 80 40",
            5: "2 0 8 144 32 16"}

designKey = sys.argv[1]
dataKey = sys.argv[2]

os.system("qsub -P johnsonlab -o logs/trn_" + designKey + "_" + dataKey + " -e logs/err_trn_" + designKey + "_" + dataKey + \
          " -N trn_" + designKey + "_" + dataKey + \
          " -cwd -b y -pe omp 8 Rscript simPipe.R " + geneVec + designDic[int(designKey)] + dataDic[dataKey] + batchDic[int(designKey)])