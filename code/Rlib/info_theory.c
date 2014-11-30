#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

static int compare (void const *a, void const *b) {
    int const *pa = a;
    int const *pb = b;
    return *pa - *pb;
}

/******************/
/* Simple entropy */
/******************/
double calcEntropy(int *vec, int nItems){
    // Easy cases
    if (nItems < 2) return 0;

    // Gets the min and max
    int min, max, width;
    for (int i=0; i < nItems; i++)
        if (vec[i] == NA_INTEGER) {
            continue;
        } else {
            min = vec[i];
            max = vec[i];
            break;
        }
    for (int i=0; i < nItems; i++){
        if (vec[i] == NA_INTEGER) continue;
        if (vec[i] < min) min = vec[i];
        if (vec[i] > max) max = vec[i];
    }
    width = max - min + 1;
    // Creates the histogram
    int *counts;
    counts = calloc(width, sizeof(int));
    if (!counts){
        printf("Error creating histogram with size %d\n", width);
        return 0;
    }

    // Updates the histogram
    for (int i=0; i < nItems; i++){
        if (vec[i] == NA_INTEGER) continue;
        counts[vec[i] - min]++;
    }
    // Gets entropy
    double ent = 0, proba = 0;
    for (int i=0; i < width; i++){
        proba = (double) counts[i] / nItems;
        ent -= (proba > 0) ? proba * log2(proba) : 0;
    }
    //Cleanup
    free(counts);

    // Done
    return ent;
}

SEXP entropy(SEXP series) {
    int nItems, *vec;
    double out;

    // Checking input
    if (isVector(series) && isInteger(series)){
        nItems = LENGTH(series);
        vec    = INTEGER(series);            
    } else {
        printf("invalid matrix.\n");
        return R_NilValue;
    }

    // Doing stuff
    out = calcEntropy(vec, nItems);

    // Out
    return ScalarReal(out);
}

/*****************/
/* Joint entropy */
/*****************/
double calcJointEntropy(int *s1, int *s2, int nItems){

    // Trivial cases
    if (nItems < 2) return 0;

    // Gets bounds
    int min1 = s1[0], min2 = s2[0], max1 = s1[0], max2 = s2[0];
    for (int i=0; i < nItems; i++)
        if (s1[i] == NA_INTEGER || s2[i] == NA_INTEGER){
            continue;
        } else {
            min1 = s1[i];
            max1 = s1[i];
            min2 = s2[i];
            max2 = s2[i];
            break;
        } 
    for (int i=0; i < nItems; i++){
        if (s1[i] == NA_INTEGER || s2[i] == NA_INTEGER) continue;
        if (s1[i] < min1) min1 = s1[i];
        if (s1[i] > max1) max1 = s1[i];
        if (s2[i] < min2) min2 = s2[i];
        if (s2[i] > max2) max2 = s2[i];
    } 

    // Allocs mem space
    int nBins   = (max1 - min1 + 1) * (max2 - min2 + 1);    
    int *counts = calloc(nBins, sizeof(int));
    if (!counts){
        printf("Error creating joint histogram\n");
        return 0;
    }

    // Updates the counts
    int bin;
    for (int i=0; i < nItems; i++){
        if (s1[i] == NA_INTEGER || s2[i] == NA_INTEGER) continue;
        bin = (s1[i] - min1) * (max2 - min2 + 1) + (s2[i] - min2);
        counts[bin]++;
    } 

    // Gets entropy
    double ent = 0, proba = 0;
    for (int i=0; i < nBins; i++){
        proba = (double) counts[i] / nItems;
        ent -= (proba > 0) ? proba * log2(proba) : 0;
    }

    // Free and return
    free(counts);
    return ent;

}

SEXP jointEntropy(SEXP series1, SEXP series2) {
    int nItems1, *v1, nItems2, *v2;
    double out;

    // Checking input
    if (isVector(series1) && isInteger(series1) &&
        isVector(series2) && isInteger(series2)){
        nItems1 = LENGTH(series1);
        v1      = INTEGER(series1);            
        nItems2 = LENGTH(series2);
        v2      = INTEGER(series2);            
    } else {
        printf("Invalid input.\n");
        return R_NilValue;
    }
    if (nItems1 != nItems2){
        printf("Columns must have same size for joint entropy");
        return R_NilValue;
    }

    // Doing stuff
    out = calcJointEntropy(v1, v2, nItems1);

    // Out
    return ScalarReal(out);
}

/****************************/
/* Variation of information */
/****************************/
double calcVI(int *v1, int *v2, int nItems){
    double out = 2 * calcJointEntropy(v1, v2, nItems) 
                - calcEntropy(v1, nItems) - calcEntropy(v2, nItems);
    out = out > 0 ? out : 0;
    return out;
}

SEXP variationInformation(SEXP series1, SEXP series2) {
    int nItems1, *v1, nItems2, *v2;
    double out;

    // Checking input
    if (isVector(series1) && isInteger(series1) &&
        isVector(series2) && isInteger(series2)){
        nItems1 = LENGTH(series1);
        v1      = INTEGER(series1);            
        nItems2 = LENGTH(series2);
        v2      = INTEGER(series2);            
    } else {
        printf("Invalid input.\n");
        return R_NilValue;
    }
    if (nItems1 != nItems2){
        printf("Columns must have same size for joint entropy");
        return R_NilValue;
    }

    // Doing stuff
    out = calcVI(v1, v2, nItems1); 

    // Out
    return ScalarReal(out);
}

SEXP bulkVariationInformation(SEXP series){

    // Checking input
    int *matrix, width, height;
    if (isMatrix(series) && isInteger(series)) {
        matrix = INTEGER(series);
        width  = ncols(series);
        height = nrows(series);
    }
    else {
        printf("invalid matrix.\n");
        return R_NilValue;
    }

    // Allocating output
    SEXP R_VIs = PROTECT(allocMatrix(REALSXP, width, width));
    double *VIs = REAL(R_VIs);

    // Doing stuff
    int *s1, *s2;
    for (int i=0; i < width * width; i++)
        VIs[i] = NA_REAL;

    double entropies[width];
    for (int i=0; i < width; i++)
        entropies[i] = calcEntropy(&matrix[i * height], height);

    int index;
    for (int i=0; i < width; i++){
        for (int j=i+1; j < width; j++){
            s1 = &matrix[i * height]; 
            s2 = &matrix[j * height];
            index = i * width + j;
            VIs[index] = 2 * calcJointEntropy(s1, s2, height)
                - entropies[i] - entropies[j];
            VIs[index] = VIs[index] > 0 ? VIs[index] : 0;
        }
    }

    // Cleanup, return
    UNPROTECT(1);
    return R_VIs;
}
