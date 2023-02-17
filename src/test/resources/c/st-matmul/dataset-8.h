#ifndef __DATASET_H
#define __DATASET_H
// Command: ./st-matmul/matmul_gendata.py --dim-size=8 --output-name=./st-matmul/dataset.h --data-type=int
#define ARRAY_SIZE 64
#define DIM_SIZE 8
typedef int data_t ;
static data_t input1_data[64] =
{
   0,   1,   1,   0,   1,   2,   0,   1,
   1,   1,   1,   2,   2,   2,   2,   0,
   2,   1,   2,   0,   1,   2,   2,   1,
   2,   1,   1,   2,   0,   1,   1,   0,
   0,   1,   2,   2,   1,   0,   2,   2,
   1,   0,   0,   2,   1,   0,   0,   0,
   2,   1,   2,   0,   1,   1,   0,   2,
   0,   1,   2,   1,   0,   2,   2,   2,
};
static data_t input2_data[64] =
{
   0,   1,   2,   0,   0,   0,   1,   2,
   1,   2,   0,   0,   2,   1,   0,   0,
   0,   2,   2,   1,   1,   1,   0,   0,
   0,   1,   1,   0,   0,   0,   1,   0,
   2,   1,   2,   1,   0,   0,   2,   2,
   0,   0,   0,   0,   2,   1,   1,   2,
   2,   2,   2,   1,   1,   0,   0,   0,
   0,   1,   0,   1,   1,   2,   0,   1,
};
static data_t verify_data[64] =
{
   3,   6,   4,   3,   8,   6,   4,   7,
   9,  13,  14,   5,   9,   4,   9,  10,
   7,  14,  14,   6,  11,   7,   6,  11,
   3,  10,  10,   2,   6,   3,   5,   6,
   7,  15,  12,   7,   8,   7,   4,   4,
   2,   4,   6,   1,   0,   0,   5,   4,
   3,  11,  10,   5,   8,   8,   5,  10,
   5,  13,   9,   6,  12,   9,   3,   6,
};
#endif // __DATASET_H