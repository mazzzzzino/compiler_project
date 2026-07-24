#include<stdio.h>
#include<stdlib.h>


//GCC specific syntax  that tells compiler to use the cdecl convention
//arguments are pushed from right to left and return value is stored in the %eax 
__attribute__ ((__cdecl__))

//extern tells the compiler that the function exist somewhere else
//so we don't allocate storage here , another file will provide it
extern int scheme_entry();


int main(int argc, const char **argv){
  int val = scheme_entry();
  printf("%d\n",val);
  return 0;

  
}
