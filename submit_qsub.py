# coding: utf-8

import os
import sys

dataDic = {"base": "FALSE mean none none",
           "batch": "TRUE mean none none",
           "sep_mod": "TRUE mean separate mod",
           "sep_hyb": "TRUE mean separate null",
           "cmb_mod": "TRUE mean combined mod",
           "cmb_null": "TRUE mean combined null"}

designDic = {1: "200 200 5 5 5 5 5 5",
             2: "200 200 2 7 5 5 5 5",
             3: "200 200 5 5 3 7 2 8",
             4: "200 200 5 5 2 6 3 9",
             5: "200 200 2 7 2 9 3 9"}

designKey = sys.argv[1]
dataKey = sys.argv[2]

os.system("qsub -P combat -o logs_" + designKey + "_" + dataKey + " -e logs_" + designKey + "_" + dataKey + \
          " -N sim_" + designKey + "_" + dataKey + " -cwd -b y -pe omp 8 Rscript simPipe.R " + \
          dataDic[dataKey] + " " + designDic[int(designKey)])
