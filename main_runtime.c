#include<stdio.h>
#include<stdlib.h>


#define FIXNUM_MASK 3
#define FIXNUM_SHIFT 2
#define FIXNUM_TAG 0

#define BOOL_MASK 255
#define BOOL_SHIFT 8
#define BOOL_TAG 7

#define CHAR_MASK 255
#define CHAR_SHIFT 8
#define CHAR_TAG 15

#define PTR_MASK        7
#define PAIR_TAG        1
#define VEC_TAG         2
#define STR_TAG         3
#define SYM_TAG         5
#define CLOSURE_TAG     6


void show (int x){
  if((x & FIXNUM_MASK) == FIXNUM_TAG){
    printf("%d",x >> FIXNUM_SHIFT);}
  else if((x & CHAR_MASK) == CHAR_TAG){
    printf("#\\%c",(char)(x >> CHAR_SHIFT));}
  else if((x & BOOL_MASK) == BOOL_TAG ){
    if(( x >> BOOL_SHIFT) != 0){
      printf("#t");
	}else { printf("#f");}
   }
}

//GCC specific syntax  that tells compiler to use the cdecl convention
//arguments are pushed from right to left and return value is stored in the %eax 
__attribute__ ((__cdecl__))

//extern tells the compiler that the function exist somewhere else
//so we don't allocate storage here , another file will provide it
extern int scheme_entry();


int main(int argc, const char **argv){
  int val = scheme_entry();
  show(val);
  printf("\n");
  return 0;

  
}
