#!/bin/bash

export JAVA="java -Xmx4096m"

${JAVA} -jar 4S.jar -FILE_INPUT communities.csv \
                    -FILE_G_OUTPUT tmp_graph_out \
                    -FILE_C_OUTPUT	tmp_cliques_out \
                    -FILE_B_OUTPUT tmp_cliques_b_out \
                    -FILE_N_OUTPUT subspaces.out\
                    -FILE_RUNTIME_OUTPUT runtime.out \
                    -NUM_POINTS 1994 \
                    -NUM_DIMS 100 \
                    -MAX_VAL 12 \
                    -METHOD 0 \
                    -THTYPE 0 \
                    -FACTOR 1
                    
