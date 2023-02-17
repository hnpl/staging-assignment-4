//--------------------------------------------------------------------------
// Includes 

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>


//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset.h"

#include "util.h"

//--------------------------------------------------------------------------
// Main
//

void __attribute__((optimize("O1"))) copy(const int size, const int stride, data_t* src, data_t* dst)
{
     for (int i = 0; i < size; i+=stride)
        dst[i] = src[i];
}

int main( int argc, char* argv[] )
{
   static data_t results_data[ARRAY_SIZE];

   copy(ARRAY_SIZE, STRIDE, source_data, results_data); // warm up

   copy(ARRAY_SIZE, STRIDE, source_data, results_data);

   int res = 12345678;

#ifndef NOVERIFY
   res = VERIFY_FUNCTION(ARRAY_SIZE, STRIDE, results_data, verify_data);
#endif

   return res;
}
