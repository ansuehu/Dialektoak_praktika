#include <R.h>
#include <Rinternals.h>

#define STRCMPR(str1, i, str2, j) (strcmp(CHAR(STRING_ELT(str1, i)), CHAR(STRING_ELT(str2, j))))
#define MAX2(nbr1, nbr2) (nbr1 < nbr2 ? nbr2 : nbr1)
/* #define SWAPSEXP(x, y, t) do { t=x; x=y; y=t; } while (0) */
#define SWAPSEXP(x, y, t) {t=x; x=y; y=t;};

enum {IRD = 1, IPI, LEVENSHTEIN, IRI, IPD};
enum {DICE = 1, JACCARD};

/* auxiliar functions */

/* match first char vector in list chars*/
int matchFirstStr(SEXP str, SEXP vecStr){
  SEXP vecElem;
  R_len_t strl, vecStrl;
  int i, j, z, flag;

  strl = length(str);
  vecStrl = length(vecStr);

  for (i = 0; i < vecStrl; i++){
    vecElem = VECTOR_ELT(vecStr, i);
    if (length(vecElem) != length(str)) continue;
    flag = 0;
    for (j = 0; j < strl; j++){
      if (STRCMPR(vecElem, j, str, j) != 0){ flag = 1; break; }
    }
    if (flag == 0){
      return i;
    }
  }
  error ("matchFirstStr has not found any result");
}

/* match char in char vector */
int matchFirstChar(const char *s, SEXP str){
  R_len_t len;
  int i;
  len = length(str);
  for (i = 0; i < len; i++){
    if (strcmp(s, CHAR(STRING_ELT(str, i))) == 0) return i;
  }
  error ("matchFirstChar has not found any results");
}

/* optimize these functions */

SEXP single_arg_R_fun(char* fun, SEXP x){
  SEXP s, t;
  t = s = PROTECT(allocList(2));
  SET_TYPEOF(s, LANGSXP);
  SETCAR(t, install(fun)); t = CDR(t);
  SETCAR(t,  x);
  UNPROTECT(1);
  return eval(s, R_GlobalEnv);
}

double do_binary(SEXP elem1, SEXP elem2, int byn){
  R_len_t  nelem1, nelem2;
  double vals[3] = {0.0, 0.0, 0.0};  /* vals = [a,b,c] */
  double aux;
  int i, j, flag;
  const char *s1, *s2;
  
  flag = 0; aux = 0.0;
  for (i = 0; i < length(elem1); i++){
    for (j = 0; j < length(elem2); j++){
      if (STRCMPR(elem1, i, elem2, j) == 0){
	vals[0] = vals[0] + 1.0;
	flag = 1;
	break;
      }
      if (flag != 1) aux = aux + 1.0;
      flag = 0;
    }
  }
  
  vals[1] = (double)length(elem1) - vals[0];
  vals[2] = (double)length(elem2) - vals[0];
  
  switch (byn){

  case DICE:
    return ((2.0 * vals[0]) / (2.0 * vals[0] + vals[1] + vals[2]));

  case JACCARD:
    return (vals[0] / (vals[0] + vals[1] + vals[2]));
    
  }
}

double do_binary_levenshtein(SEXP elem1, SEXP elem2, SEXP distMat, SEXP strs){
  SEXP t, i_vec, j_vec;
  R_len_t  nelem1, nelem2, multiplier, nstrs;
  double  val, candidateVal, finalVal, distance;
  int i, j, pos1, pos2, totalVal, counter;
  const char *s1, *s2;

  /* elem1 must be the long one */
  if (length(elem2) > length(elem1)){
    SWAPSEXP(elem1, elem2, t);
  }

  nstrs = length(strs);
  multiplier = length(elem2);
  distance = 0.0;

  PROTECT(i_vec = allocVector(REALSXP, length(elem1)));
  for (i = 0; i < length(elem1); ++i){
    for (j = 0; j < length(elem2); ++j){
      if (STRCMPR(elem1, i, elem2, j) == 0){
	s1 = CHAR(STRING_ELT(elem1, i));
	s2 = CHAR(STRING_ELT(elem2, j));
	val = 0.0;
	break;
      }
      
      pos1 = matchFirstChar(CHAR(STRING_ELT(elem1, i)), strs);
      pos2 = matchFirstChar(CHAR(STRING_ELT(elem2, j)), strs);

      candidateVal =  REAL(distMat)[nstrs * pos1 + pos2];
      if (j == 0){
	val = candidateVal;
	s1 = CHAR(STRING_ELT(elem1, i));
	s2 = CHAR(STRING_ELT(elem2, j));
	continue;
      }
	
      if (candidateVal < val){
	val = candidateVal;
	s1 = CHAR(STRING_ELT(elem1, i));
	s2 = CHAR(STRING_ELT(elem2, j));
      }
    }
    
    REAL(i_vec)[i] = val / (double) MAX2(strlen(s1), strlen(s2));
    distance += REAL(i_vec)[i];
  }

  
  j_vec = single_arg_R_fun("sort", i_vec);
  for (j = 0; j < length(elem2); j++){
    distance += REAL(j_vec)[j];
  }
  
  distance = distance / (double) (length(elem1) + length(elem2));
  UNPROTECT(1);
  return distance;
}

void do_ird_variable(double *answer, int *obs, SEXP strList, int binary_measure){
  SEXP first, second;
  R_len_t len;
  int i, j, ij;
  
  len = length(strList);
  ij = 0;
  for (i = 0; i < len; i++){
    for (j = 0; j < i; j++){
      first = VECTOR_ELT(strList, i);
      second = VECTOR_ELT(strList, j);
      if (length(first) == 0 || length(second) == 0){
	ij++;
	continue;
      }
      obs[ij] += 1;
      
      if (length(first) == 1 && length(second) == 1){  /* simple case */
	if (STRCMPR(first, 0, second, 0) == 0){
	  answer[ij] += 1.0;
	}
	ij++;
	continue;
      }
      
      answer[ij] += do_binary(first, second, binary_measure); /* binary measure */
      ij++;
    }
  }
}

void do_ipi_variable(double *answer, int *obs, SEXP strList, int binary_measure){
  SEXP count, uniqueLemmas, valueVector, coidentityVector, first, second;
  int i, j, ij, nobs, index;
  R_len_t nlem, nlemu;
  double *valueVector_ptrs, *coidentityVector_ptrs, val, bval, totalCount, *count_ptrs, firstVal, secondVal, aux;

  /* count lemmas unique lemmas */
  
  PROTECT(uniqueLemmas = single_arg_R_fun("unique", strList)); /* 1 */
  PROTECT(count = allocVector(REALSXP, length(uniqueLemmas))); /* 2 */
  memset(REAL(count), 0.0, length(uniqueLemmas) * sizeof(double));
  count_ptrs = REAL(count);
  totalCount = 0.0;
  nlem = length(strList);
  nlemu = length(uniqueLemmas);
  for (i = 0; i < nlem; i++){
    if (length(VECTOR_ELT(strList, i)) == 0) continue;
    index = matchFirstStr(VECTOR_ELT(strList, i), uniqueLemmas);
    count_ptrs[index] = count_ptrs[index] + 1.0;
    totalCount = totalCount + 1.0;
  }
  
  /* compute the TAX values */
  PROTECT(valueVector = allocVector(REALSXP, nlemu));  /* 3 */
  PROTECT(coidentityVector = allocVector(REALSXP, nlemu)); /* 4 */
  
  memset(REAL(valueVector), 0.0, nlemu * sizeof(double));
  memset(REAL(coidentityVector), 0.0, nlemu* sizeof(double));
  
  valueVector_ptrs = REAL(valueVector);
  coidentityVector_ptrs = REAL(coidentityVector);
  
  for (i = 0; i < nlemu; i++){
    first = VECTOR_ELT(uniqueLemmas, i);
    
    for (j = 0; j < nlemu; j++){
      second = VECTOR_ELT(uniqueLemmas, j);
      
      bval = do_binary(first, second, binary_measure);
      if (bval == 0.0){
	coidentityVector_ptrs[i]++;
	continue;
      }
      
      valueVector_ptrs[i] += (bval * count_ptrs[j]);
      
    }
    valueVector_ptrs[i] = valueVector_ptrs[i] / totalCount;
  }

  /* apply identity values */
  
  ij = 0;
  for (i = 0; i < nlem; i++){
    for (j = 0; j < i; j++){
      
      first = VECTOR_ELT(strList, i);
      second = VECTOR_ELT(strList, j);
      
      if (length(first) == 0 || length(second) == 0){ /* if any na pass */
	ij++;
	continue;
      }
      
      obs[ij]++;
      bval = do_binary(first, second, binary_measure);
      firstVal = valueVector_ptrs[matchFirstStr(first, uniqueLemmas)];
      secondVal = valueVector_ptrs[matchFirstStr(second, uniqueLemmas)];
      aux =  bval * (firstVal + secondVal) / 2.0;
      val = 1.0 - (aux - 1.0) / totalCount;
      answer[ij] += (val / (val + (1.0 - aux)));
      ij++;
    }
  }

  UNPROTECT(4);
}

void do_levenshtein_variable(double *answer, int *obs, SEXP strList, int binary_measure){
  SEXP uniqueStr, strs, strDists, ustrList, binaryValues, maxValues, first, second;
  int i, j, ij, index, index2;
  R_len_t nustrl, nulemmas, nlemmas;
  double *binaryValues_ptrs;

  PROTECT(uniqueStr = single_arg_R_fun("unique", single_arg_R_fun("unlist", strList)));  /* 1 */
  PROTECT(strs = single_arg_R_fun("unlist", strList)); /* 2 */
  PROTECT(strDists = single_arg_R_fun("adist", uniqueStr));  /* 3 */
  PROTECT(ustrList = single_arg_R_fun("unique", strList)); /* 4 */
  
  nustrl = length(ustrList);
  PROTECT(binaryValues = allocMatrix(REALSXP, nustrl, nustrl)); /* 5 */
  PROTECT(maxValues = allocMatrix(REALSXP, nustrl, nustrl)); /* 6 */
  binaryValues_ptrs = REAL(binaryValues);
  
  /* ij = 0; */
  for (i = 0; i < nustrl; i++){
    first = VECTOR_ELT(ustrList, i);
    for (j = 0; j <= i; j++){
      second = VECTOR_ELT(ustrList, j);
      if (length(first) == 0 || length(second) == 0){
	continue;
      }
      binaryValues_ptrs[j + i*nustrl] = binaryValues_ptrs[i + j*nustrl] =
	do_binary_levenshtein(first, second, strDists, uniqueStr);
    }
  }
  
  nlemmas = length(strList);
  ij = 0;
  for (i = 0; i < nlemmas; i++){
    for (j = 0; j < i; j++){
      first = VECTOR_ELT(strList, i);
      second = VECTOR_ELT(strList, j);
      if (length(first) == 0 || length(second) == 0){ /* if any na pass */
	ij++;
	continue;
      }
      obs[ij] = obs[ij] + 1;
      index = matchFirstStr(first, ustrList);
      index2 = matchFirstStr(second, ustrList);
      answer[ij] = answer[ij] + REAL(binaryValues)[index + index2 * nustrl];
      ij++;
    }
  }
  UNPROTECT(6);
}

SEXP do_ird(SEXP inputList, int binary_measure){
  SEXP answer, obsCount;
  R_len_t nvars, nids;
  int i;

  nids = length(VECTOR_ELT(inputList, 0));
  nvars = length(inputList);
  PROTECT(answer = allocVector(REALSXP, nids * (nids - 1) / 2));
  PROTECT(obsCount = allocVector(INTSXP, nids * (nids - 1) / 2));

  memset(REAL(answer), 0.0, length(answer) * sizeof(double));
  memset(INTEGER(obsCount), 0, length(obsCount) * sizeof(int));
  
  for (i = 0; i < nvars; i++){
    do_ird_variable(REAL(answer), INTEGER(obsCount), VECTOR_ELT(inputList, i), binary_measure);
  }
  
  for (i = 0; i < length(answer); i++){
    REAL(answer)[i] = REAL(answer)[i] * 100.0 / (double) INTEGER(obsCount)[i];
  }
  
  UNPROTECT(2);
  return answer;
}

SEXP do_ipi(SEXP inputList, int binary_measure){
  SEXP answer, nobs, lemmaList;
  R_len_t nvars, nids;
  int i, ansLen;

  nids = length(VECTOR_ELT(inputList, 0));
  nvars = length(inputList);
  ansLen = nids * (nids - 1) / 2;
  
  PROTECT(answer = allocVector(REALSXP, ansLen));
  PROTECT(nobs = allocVector(INTSXP, ansLen));
  
  memset(REAL(answer), 0.0, ansLen * sizeof(double));
  memset(INTEGER(nobs), 0, ansLen * sizeof(int));

  for (i = 0; i < nvars; i++){
    do_ipi_variable(REAL(answer), INTEGER(nobs), VECTOR_ELT(inputList, i), binary_measure);
  }

  for (i = 0; i < ansLen; i++){
    REAL(answer)[i] = 100.0 * REAL(answer)[i] / (double)(INTEGER(nobs)[i]);
  }
  
  UNPROTECT(2);
  return answer;
}

SEXP do_levenshtein(SEXP inputList, int binary_measure){
  SEXP answer, nobs;
  R_len_t nvars, nids;
  int i, ansLen;

  nids = length(VECTOR_ELT(inputList, 0));
  nvars = length(inputList);
  ansLen = nids * (nids - 1) / 2;

  PROTECT(answer = allocVector(REALSXP, ansLen));
  PROTECT(nobs = allocVector(INTSXP, ansLen));

  memset(REAL(answer), 0.0, ansLen * sizeof(double));
  memset(INTEGER(nobs), 0, ansLen * sizeof(int));

  for (i = 0; i < nvars; i++){
    do_levenshtein_variable(REAL(answer), INTEGER(nobs), VECTOR_ELT(inputList, i), binary_measure);
  }

  for (i = 0; i < ansLen; i++){
    REAL(answer)[i] = 100.0 * REAL(answer)[i] / (double)((INTEGER(nobs)[i]));
  }
  
  UNPROTECT(2);
  return answer;
}

void reverseMeasure (SEXP measure){
  R_len_t len;
  int i;
  
  len = length(measure);
  for (i = 0; i < len; ++i){
      REAL(measure)[i] =  100.0 - REAL(measure)[i];
  }
}

SEXP diaMeasure_C(SEXP inputList, SEXP measureR, SEXP binary_measureR, SEXP attrs){
  int measure, binary_measure, i;
  SEXP answer, diagVal, names;

  measure = INTEGER(measureR)[0];
  binary_measure = INTEGER(binary_measureR)[0];
  PROTECT(diagVal = allocVector(INTSXP, 1)); /* 1 */
  
  switch (measure){
    
  case IRD:
    PROTECT(answer = do_ird(inputList, binary_measure)); /* 2 */
    INTEGER(diagVal)[0] = 100;
    break;
    
  case IPI:
    PROTECT(answer = do_ipi(inputList, binary_measure)); /* 2 */
    INTEGER(diagVal)[0] = 100;
    break;

  case LEVENSHTEIN:
    PROTECT(answer = do_levenshtein(inputList, binary_measure)); /* 2 */
    INTEGER(diagVal)[0] = 0;
    break;

  case IRI:
    PROTECT(answer = do_ird(inputList, binary_measure)); /* 2 */
    reverseMeasure(answer);
    INTEGER(diagVal)[0] = 0;
    break;

  case IPD:
    PROTECT(answer = do_ipi(inputList, binary_measure)); /* 2 */
    reverseMeasure(answer);
    INTEGER(diagVal)[0] = 0;
    break;
  }

  PROTECT(names = getAttrib(attrs, R_NamesSymbol)); /* 3 */
  for (i = 0; i < length(attrs); i++)
    setAttrib(answer, install(translateChar(STRING_ELT(names, i))), 
	      VECTOR_ELT(attrs, i));
  setAttrib(answer, install("diagv"), diagVal);

  UNPROTECT(3);
  return answer;
}


