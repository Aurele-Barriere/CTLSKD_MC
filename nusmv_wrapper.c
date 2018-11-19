#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *TT = " true";

/* Runs NuSMV -dcx on problem written in ltl */
/* Returns 1 if the spec is true, 0 otherwise */
/* Inspired by https://stackoverflow.com/questions/646241/c-run-a-system-command-and-get-output */
int main( int argc, char *argv[] )
{

  FILE *fp;
  char path[1035];

  /* Open the command for reading. */
  fp = popen("./NuSMV -dcx ltl", "r");
  if (fp == NULL) {
    printf("Failed to run command\n" );
    exit(1);
  }
  char *p;
  /* Read the output a line at a time - output it. */
  while (fgets(path, sizeof(path)-1, fp) != NULL) {
    if (strlen(path) > 4) {
      p = strrchr(path, ' ');
      if (strlen(p) > 4) {
	if (p[1] == 't' & p[2] == 'r' & p[3] == 'u' & p[4] == 'e') {
	  return 1;
	}
      }
    }
  }

  /* close */
  pclose(fp);
  return 0;
}
